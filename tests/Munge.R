# Prepare session, set paths ----------------------------------------------

rm(list = ls())

library(gdata)
library("FixEncoding") # I have rewritten DiagnoseEncoding.R as a package, which can be installed via devtools::install_github("jkroes/FixEncoding")

# map_data <- readRDS("/Users/justin/Desktop/evidence_heat/Sam_data/map_data_final_5_13_R16.rds")
map_data <- readRDS("/Users/LumpyGrads/Desktop/evidence/Sam_data/map_data_final_5_13_R16.rds")
# load("/Users/justin/Desktop/evidence_heat/Sam_data/evidence_based_5_13_16.RData")
load("/Users/LumpyGrads/Desktop/evidence/Sam_data/evidence_based_5_13_16.RData")
# Fix encoding issues -----------------------------------------------------
map_matches <- check_column_encoding(map_data)
# map_matches$Title # use to construct ASCII_replacement
ASCII_replacement <- matrix(c(rep("a", times = 3), "e", rep("'", times = 12), rep.int("jigglypuff", 13), "'", rep.int("jigglypuff", 2)), nrow = 16)
map_data_fixed <- ascii_replace(map_data, map_matches, "Title", ASCII_replacement)
# > grep("jigglypuff", map_data_fixed$Title %>% unique)
# integer(0)
# This is a good indication that there were no secondary bytes (besides the one in element 14) that I missed; otherwise jigglypuff would show up.
biblio_matches <- enc_check(data.biblio)
# all.equal(biblio_matches$Title$unique_errors %>% unname %>% unlist %>% unique,
#           map_matches$Title$unique_errors %>% unname %>% unlist %>% unique)
## [1] TRUE
# You may re-use ASCII_replacement
data.biblio.fixed <- ASCIIreplace(data.biblio, biblio_matches, "Title", ASCII_replacement)

keep(map_data_fixed, data.biblio.fixed, sure = TRUE) # remove intermediate objects
# Note that I've only fixed the "Title" columns. The other columns still have issues.
