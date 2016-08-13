#'@export
exp_ascii_replace_exp <- function(dataset, enc_check_results, column_name, rep_str) {

  valid_bytes <- c( '\n', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname
  
  invalid_bytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% valid_bytes)) %>%
    paste0("\\x", .) %>%
    c(., "\\xef\\xbf\\xbd") # plus Unicode replacement character

  second_match <- sapply(invalid_bytes, grep, enc_check_results[[column_name]], useBytes = TRUE) %>% unlist
  single_byte_idx <- unname(second_match)
  single_bytes <- names(second_match) %>% substr(1, 4)
  searchable_idx <- set_names(single_bytes, single_byte_idx)

  # Patterns
  usingle_bytes <- searchable_idx %>% unique
  byte_grid <- lapply(1:length(usingle_bytes), function(x) replicate(x, usingle_bytes, simplify = FALSE)) %>%
    sapply(expand.grid, stringsAsFactors = FALSE) %>%
    sapply(apply, 2, as.character) %>%
    sapply(function(x) split(x, 1:nrow(x))) %>%
    sapply(unname) %>%
    sapply(lapply, paste, collapse = "") %>%
    unlist

  # Find byte combinations that match invalid bytes for each unique error-ridden observation
  third_match_fun <- function(j) sapply(byte_grid, grep, j, useBytes = TRUE)
  lmatch <- sapply(enc_check_results[[column_name]], third_match_fun) # a matrix -- at least for the data I've used for testing. Could change and screw things up...
  lmatch_idx <- apply(lmatch, 2, function(x) max(which(x > 0))) # only accounts for a single invalid byte sequence per original dataset column element... My guess is that I can fix this by essentailly checking to see if earlier idx are a substring of later idx
  actual_bytes <- sapply(names(lmatch_idx), function(x) extract(lmatch[,x], lmatch_idx[x]) %>% names) %>% unname

  # Replace invalid characters
  enc_check_results_fix <- mapply(gsub,
                                  actual_bytes,
                                  rep_str,
                                  enc_check_results[[column_name]],
                                  MoreArgs = list(useBytes = TRUE)) %>%
    unname
  dataset %<>% as.data.table
  column <- dataset[, get(column_name)]
  for (i in 1:length(enc_check_results[[column_name]])) {
    column_fix <- gsub(pattern = enc_check_results[[column_name]][i],
                       replacement = enc_check_results_fix[i],
                       x = column,
                       fixed = TRUE,
                       useBytes = TRUE)
    column <- column_fix
  }
  dataset %<>% as.data.table
  dataset[, (column_name) := column]
  return(dataset)
}

# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted

# Notes
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select
