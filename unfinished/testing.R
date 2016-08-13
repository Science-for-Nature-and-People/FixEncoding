# Only necessary for building the adapted function. Not afterwards.
source("unfinished/check_column_encoding.R")
source("unfinished/exp_ascii_replace.R")
# dataset <- readRDS("/Users/justin/Desktop/evidence_heat/Sam_data/map_data_final_5_13_R16.rds")
dataset <- readRDS("/Users/LumpyGrads/Desktop/evidence/Sam_data/map_data_final_5_13_R16.rds")
enc_check_results <- check_column_encoding(dataset) # Luckily, this message won't show up on the user's end... I hope. May have to rework when it shows up and what functions are internal.
# > enc_check_results %>% names
# [1] "Authors"         "Title"           "Title.formatted" "Fullcitation"
column_name <- "Title"
rep_str <- c(rep("a", times = 3), "e", rep("'", times = 12))
test <- exp_ascii_replace_exp(dataset, enc_check_results, column_name, rep_str)
meta_test <- check_column_encoding(test)
# > meta_test %>% names
# [1] "Authors"         "Title.formatted" "Fullcitation"
