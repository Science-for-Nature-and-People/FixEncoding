# Prepare session, set paths ----------------------------------------------

rm(list = ls())

# Source newest scripts
pathway <- "/Users/justin/Desktop/FixEncoding/R"
scripts <- list.files(pathway)
source_scripts <- function(file) {
  source(file.path(pathway, file))
}
lapply(scripts, source_scripts)

# Load data, check for issues, and convert control bytes to Unicode replacement
# symbol
map_data <- readRDS("/Users/justin/Desktop/FixEncoding/tests/Sam_data/map_data_final_5_13_R16.rds")
map_matches <- check_column_encoding(map_data)

# Find number of columns necessary for replacement matrix for each column.
find_max_num_seqs(map_matches)
# $Authors
# [1] 2
#
# $Title
# [1] 2
#
# $Title.formatted
# [1] 3
#
# $Fullcitation
# [1] 1

# Replace replacement symbol with proper characters. Note that the new
# functions, including check_column_encoding.R, match the lower octal range.
# Thus, the replacement matrix will differ from earlier results used in
# Munge.R.
col1 <- c("\xc3\xa1",
          "\xc3\xa9",
          "\xc3\xa1",
          "'",
          "'",
          "'",
          " ",
          "'",
          " ",
          " ",
          "'",
          "'",
          "'",
          "'",
          "\xc3\xa1",
          "",
          "'",
          "'",
          "'",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          " ",
          "'")


col2 <- c("jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          " ",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "'",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff",
          "",
          "jigglypuff",
          "jigglypuff",
          "jigglypuff")

uni_repl_str <- cbind(col1, col2)

fixed_title <- replace_bytes(map_data, map_matches, "Title", uni_repl_str)
meta_fixed_title <- check_column_encoding(fixed_title)
meta_fixed_title %>% names
# [1] "Authors"         "Title.formatted" "Fullcitation"
grep("jigglypuff", fixed_title$Title %>% unique)
# integer(0)
# This is a good indication that there were no secondary bytes (besides the one in element 14) that I missed; otherwise jigglypuff would show up--it was my filler word.

# Example control replace
control_rep_str = matrix(c(rep_len("apples", 35),
                           rep_len("apples", 35),
                           rep_len("apples", 35)), nrow = 35)
fixed_2_columns <- control_replace(fixed_title, map_matches, "Title.formatted", control_rep_str)
meta_fixed_2_columns <- check_column_encoding(fixed_2_columns)
meta_fixed_2_columns %>% names
# [1] "Authors" "Fullcitation"

# # Blah
# load("/Users/justin/Desktop/evidence_heat/Sam_data/evidence_based_5_13_16.RData")
# biblio_matches <- check_column_encoding(data.biblio)
# # all.equal(biblio_matches$Title, map_matches$Title)
# # [1] TRUE
# fixed_title2 <- diamond_replace(data.biblio, biblio_matches, "Title", uni_repl_str)



# Debugging ---------------------------------------------------------------
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
