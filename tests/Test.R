# Prepare session, set paths ----------------------------------------------

rm(list = ls())

library("FixEncoding") # I have rewritten DiagnoseEncoding.R as a package, which can be installed via devtools::install_github("jkroes/FixEncoding")

# map_data <- readRDS("/Users/justin/Desktop/evidence_heat/Sam_data/map_data_final_5_13_R16.rds")
map_data <- readRDS("/Users/LumpyGrads/Desktop/evidence/Sam_data/map_data_final_5_13_R16.rds")
# load("/Users/justin/Desktop/evidence_heat/Sam_data/evidence_based_5_13_16.RData")
load("/Users/LumpyGrads/Desktop/evidence/Sam_data/evidence_based_5_13_16.RData")
# Fix encoding issues -----------------------------------------------------
map_matches <- check_column_encoding(map_data)
# map_matches$Authors # use to construct ASCII_replacement
ASCII_replacement <- c(rep("a", times = 4), "e", "!", "%", rep("youcan'tstoptherockandroll", times = 7)) # % and ! are the double replacement for line 6

#### For debugging
dataset <- map_matches
enc_check_results <- map_matches
column_name <- "Authors"
rep_str <- ASCII_replacement
###

# map_data_fixed <- exp_ascii_replace_exp(map_data, map_matches, "Title", ASCII_replacement)
# meta_matches <- check_column_encoding(map_data_fixed)
# meta_fix <- exp_ascii_replace_exp(meta_matches, map_matches, "Title", ASCII_replacement)

