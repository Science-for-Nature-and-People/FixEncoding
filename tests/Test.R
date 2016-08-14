# Prepare session, set paths ----------------------------------------------

rm(list = ls())

source("/Users/LumpyGrads/Desktop/FE/R/ascii_replace.R")
source("/Users/LumpyGrads/Desktop/FE/R/check_column_encoding.R")
# library("FixEncoding") # I have rewritten DiagnoseEncoding.R as a package, which can be installed via devtools::install_github("jkroes/FixEncoding")

map_data <- readRDS("/Users/LumpyGrads/Desktop/evidence/Sam_data/map_data_final_5_13_R16.rds")
map_matches <- check_column_encoding(map_data)
# map_matches$Authors # use to construct ASCII_replacement
ASCII_replacement <- c(rep("a", times = 4), "e", "!", "%", rep("youcan'tstoptherockandroll", times = 6)) %>% 
  cbind(., sort(., decreasing = TRUE))
map_fix <- ascii_replace(map_data, map_matches, "Authors", ASCII_replacement)
meta_match <- check_column_encoding(map_fix)
names(meta_match)
View(map_fix)
#### For debugging
dset <- map_data
enc_check_results <- map_matches
column_name <- "Authors"
rep_str <- ASCII_replacement
###

# map_data_fixed <- exp_ascii_replace_exp(map_data, map_matches, "Title", ASCII_replacement)
# meta_matches <- check_column_encoding(map_data_fixed)
# meta_fix <- exp_ascii_replace_exp(meta_matches, map_matches, "Title", ASCII_replacement)

