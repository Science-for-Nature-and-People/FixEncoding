source("unfinished/check_column_encoding.R")
dataset <- readRDS("/Users/justin/Desktop/evidence_heat/Sam_data/map_data_final_5_13_R16.rds")
buggy_columns <- check_column_encoding(dataset)

invalid_bytes <- byte_string[["invalid_bytes"]]

ascii_replace <- function(dataset, enc_check_results, column_name, rep_str) {

  while (any(search() == "enc_check_results[[column_name]]")) detach("enc_check_results[[column_name]]")
  attach(enc_check_results[[column_name]])
  # [1] "searchable_idx" "pretty_idx"     "errors"         "unique_errors"
  # attach(map_matches$Authors) # used for building the code below because of length 3 (or 4 or something. I'm tired) unique errors

  simplification <- function(x) x %>%
    unname %>%
    unlist

  # Patterns
  bytes <- searchable_idx %>%
    unique
  bytes <- lapply(1:length(bytes), function(x) replicate(x, bytes, simplify = FALSE)) %>%
    sapply(., function(x) expand.grid(x, stringsAsFactors = FALSE)) %>%
    sapply(., function(x) apply(x, 2, as.character)) %>%
    sapply(., function(x) split(x, 1:nrow(x))) %>%
    sapply(., unname) %>%
    sapply(., function(x) lapply(x, paste, collapse = "")) %>%
    unlist # this may not be necessary

  # Find byte combinations that match invalid bytes for each unique error-ridden observation
  unique_errors2 <- simplification(unique_errors) %>% unique
  byte_matching <- function(j) sapply(bytes, grep, j, useBytes = TRUE)
  redundant <- sapply(unique_errors2, byte_matching)
  idx <- apply(redundant, 2, function(x) max(which(x > 0)))
  unique_bytes <- sapply(names(idx), function(x) extract(redundant[,x], idx[x]) %>% names) %>% unname

  # Replace invalid characters
  uniqueFix <- mapply(gsub, unique_bytes, rep_str, unique_errors2, MoreArgs = list(useBytes = TRUE)) %>%
    unname
  errors2 <- simplification(errors)
  pretty_idx2 <- simplification(pretty_idx)

  for (i in 1:length(unique_errors2)) {
    errors2 <- gsub(unique_errors2[i], uniqueFix[i], errors2, fixed = TRUE, useBytes = TRUE)
  }

  dataset[pretty_idx2, column_name] <- errors2

  detach(enc_check_results[[column_name]])

  # return(uniqueFix)
  return(dataset)

}





# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted

# Notes
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select
