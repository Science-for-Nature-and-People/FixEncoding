#'@title Replace invalid UTF-8 bytes.
#'@description Replace invalid bytes detected by \code{check_column_encoding}
#'  with valid ASCII or UTF-8 characters. Involves manual construction of a
#'  replacement vector (see \code{rep_str}).
#'@param dset A data.frame or data.table.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}.
#'@param column_name The name of an element in the list returned by
#'  \code{check_column_encoding}, corresponding to a column header in
#'  \code{dset} where encoding issues were detected (or possibly false
#'  positives).
#'@param rep_str A character vector of the same length as
#'  \code{enc_check_results[[column_name]]}. Strings within the character vector
#'  consist of a single character to replace in the corresponding strings of
#'  \code{enc_check_results[["column_name]]}. As of yet, the function only
#'  handles replacement of single invalid bytes sequences per
#'  \code{enc_check_results[["column_name]]} string. This vector must be
#'  manually constructed, as there is no method for guessing the proper ASCII or
#'  UTF-8 character to replace an invalid byte sequence. Care must be taken in
#'  the order of replacement for strings with more than one invalid sequence.
#'  Until muliple-sequence replacement is added, there is no way to know which
#'  sequence will be replaced ahead of time. However, it is rare to see strings
#'  with more than two invalid byte sequences, so simple trial and error should
#'  suffice if this situation is encountered. Simply specify the correct
#'  character in rep_str and re-run the code. The function is also only capable
#'  of replacing single columns at a time. To replace additional columns, the
#'  data.table returned by \code{ascii_replace} must be fed back into the
#'  function as the value of \code{dset}--likely with a different value for
#'  \code{rep_str}.
#'@return A data.table with the same structure as \code{dset} but valid UTF-8
#'  bytes.
#'@export
ascii_replace <- function(dset, enc_check_results, column_name, rep_str) {

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
  third_match_fun <- function(j) sapply(byte_grid, grepl, j, useBytes = TRUE)
  lmatch <- sapply(enc_check_results[[column_name]], third_match_fun)
  lmatch_idx <- apply(lmatch, 2, which)
  # lmatch_pattern <- sapply(lmatch_idx, names)
  # grepl("\\x90", "\\x90\\xa9", fixed = TRUE)
  max_idx <- sapply(lmatch_idx, max) # only accounts for a single invalid byte sequence per original dset column element... My guess is that I can fix this by essentailly checking to see if earlier idx are a substring of later idx
  actual_bytes <- sapply(names(max_idx), function(x) extract(lmatch[,x], max_idx[x]) %>% names) %>% unname

  # Replace invalid characters
  enc_check_results_fix <- mapply(gsub,
                                  actual_bytes,
                                  rep_str,
                                  enc_check_results[[column_name]],
                                  MoreArgs = list(useBytes = TRUE)) %>%
    unname
  if (dset %>% is.data.table %>% not) dset %<>% as.data.table
  column <- dset[, get(column_name)]
  for (i in 1:length(enc_check_results[[column_name]])) {
    column_fix <- gsub(pattern = enc_check_results[[column_name]][i],
                       replacement = enc_check_results_fix[i],
                       x = column,
                       fixed = TRUE,
                       useBytes = TRUE)
    column <- column_fix
  }
  dset[, (column_name) := column]
  return(dset)
}

# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted

# Notes
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select
