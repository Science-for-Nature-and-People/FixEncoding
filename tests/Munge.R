# Prepare session, set paths ----------------------------------------------

rm(list = ls())

map_data <- readRDS("/Users/justin/Desktop/evidence_heat/Sam_data/map_data_final_5_13_R16.rds")
map_matches <- check_column_encoding(map_data)

# Example uni_repl_replace
uni_repl_str <- matrix(c(rep("a", times = 3), "e", rep("'", times = 12), rep.int("jigglypuff", 13), "'", rep.int("jigglypuff", 2)), nrow = 16)
test2 <- uni_repl_replace(map_data, map_matches, "Title", uni_repl_str)
meta_test2 <- check_column_encoding(test2)
meta_test2 %>% names
# [1] "Authors"         "Title.formatted" "Fullcitation"
grep("jigglypuff", test2$Title %>% unique)
# integer(0)
# This is a good indication that there were no secondary bytes (besides the one in element 14) that I missed; otherwise jigglypuff would show up.

# Example control replace
control_rep_str = matrix(c(rep_len("098098098", 35),
                           rep_len("secondary098", 35),
                           rep_len("NotImportantAtAll", 35)), nrow = 35)
test <- control_replace(map_data, map_matches, "Title.formatted", control_rep_str)
meta_test <- check_column_encoding(test)
meta_test %>% names
# [1] "Authors"      "Title"        "Fullcitation"
grep("NotImportantAtAll", test$Title.formatted %>% unique)
# [1] 189
# This is actually good. I was lazy and did not add a different value for the only row to have three control bytes to replace. So only one showing up is awesome!

# ### Debugging
# library(magrittr)
# library(dplyr)
# library(stringi)
# library(data.table)
# source("R/check_column_encoding.R")
# source("R/control_replace.R")
# source("R/uni_repl_replace.R")
#
# ### Debugging control_replace.R
# dset <- map_data
# enc_check_results <- map_matches
# column_name <- "Title.formatted"
# rep_str <- control_rep_str
# ###
#
# ### Debugging uni_repl_replace.R
# dset <- map_data
# enc_check_results <- map_matches
# column_name <- "Title"
# rep_str <- uni_repl_str
# max_seq_length <- 2
# ###
