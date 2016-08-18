#'@title Replace invalid UTF-8 bytes.
#'@description Replace invalid bytes detected by \code{check_column_encoding}
#'  with valid ASCII or UTF-8 characters. Involves manual construction of a
#'  replacement matrix (see \code{rep_str}). These bytes are represented by the
#'  Unicode replacement character in this function, which looks like a diamond
#'  with an internal question mark.
#'@param dset A data.frame or data.table.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}. Or, if you previously called control_replace.R
#'  and saved the output, the data.table it returned.
#'@param column_name The name of an element in the list returned by
#'  \code{check_column_encoding}, or a column header in \code{dset} or the
#'  data.table returned by control_replace.R. These are all equivalent.
#'@param rep_str A matrix. The number of rows should equal the length of
#'  \code{enc_check_results[[column_name]]}. The number of columns should equal
#'  the maximum number of invalid bytes sequences observed in any element of
#'  \code{enc_check_results[[column_name]]}. Elements within each row of this
#'  matrix are the replacement characters for each invalid byte sequence
#'  observed in the corresponding elements of
#'  \code{enc_check_results[[column_name]]}, or a random filler character or
#'  string. Because some elements of \code{enc_check_results[[column_name]]} may
#'  have more invalid byte sequences than others, and because the number of
#'  columns in the replacement matrix is equal to the maximum number of
#'  sequences, some matrix rows may need dummy strings in order to be completely
#'  filled. This matrix must be manually constructed, as there is no method for
#'  guessing the proper ASCII or UTF-8 character to replace an invalid byte
#'  sequence. The function is only capable of replacing single columns at a
#'  time. To replace additional columns, the data.table returned by this
#'  function must be fed back into the function as the value of
#'  \code{dset}--likely with a different value for \code{rep_str}. While this
#'  may seem like an error-prone approach, remember that you can script your
#'  manual construction. A good idea is to use the same filler word throughout
#'  your matrix. Then call \code{grep} using that filler word as the value of
#'  the \code{pattern} argument and the result of calling \code{diamond_replace}
#'  as the value of argument \code{x}. If the filler word is matched, it
#'  probably means that you missed a secondary, tertiary, etc. byte sequence
#'  lurking in the corresponding element of
#'  \code{enc_check_results[[column_name]]}. (It's happened to me at least
#'  once!).
#'@param max_seq_length The highest number of invalid byte sequences observed in
#'  any element of {enc_check_results[[column_name]]}. Without the correct
#'  value, character replacement will not proceed correctly but it is possible
#'  no error will result. Always check your results!
#'@return A data.table with the same structure as \code{dset} but valid UTF-8
#'  bytes.
#'@export
diamond_replace <- function(dset, enc_check_results, column_name, rep_str, max_seq_length) {

  # In case any columns have mixed results--but not on the same line! At that point, I quit
  uni_repl_results <- enc_check_results[[column_name]][grepl("\ufffd", enc_check_results[[column_name]])]

  # Pattern replacement on column of unique values
  uni_repl_fix <- uni_repl_results
  for (j in 1:ncol(rep_str)) {
    uni_repl_fix <- mapply(sub,
                           rep_len("\ufffd+", length(uni_repl_fix)),
                           rep_str[,j],
                           uni_repl_fix) %>%
      unname
  }

  # Pattern replacement in dataset using column of unique values before and after column replacement
  dset %<>% apply(2, iconv, to = "UTF-8", sub = "\ufffd")
  if (dset %>% is.data.table %>% not) dset %<>% data.table
  column_to_fix <- dset[, get(column_name)]
  for (i in 1:length(uni_repl_fix)) {
    column_to_fix <- sub(pattern = uni_repl_results[i],
                      replacement = uni_repl_fix[i],
                      x = column_to_fix,
                      fixed = TRUE)
  }
  dset[, (column_name) := column_to_fix]

  return(dset)
  }

# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted

# Notes
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select for multiple columns
