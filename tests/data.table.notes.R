# Notes on data.table syntax
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select for multiple columns
# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted