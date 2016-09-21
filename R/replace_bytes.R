#'@title Replace invalid UTF-8 bytes.
#'@description Replace bytes detected by \code{check_column_encoding} with
#'  characters of your choice. Involves manual construction of a replacement
#'  matrix (see \code{rep_str}). This function will replace both the Unicode
#'  replacement symbol as well as UTF-8 control sequences in both the lower
#'  and upper ranges, represented respectively by octal and lower-case "u"
#'  escape sequences (see ?Quotes).
#'@param dset A data.frame or data.table. Or, if you previously called
#'  replace_bytes.R and saved the output, the data.table it returned.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}.
#'@param column_name The name of an element in the list returned by
#'  \code{check_column_encoding}.
#'@param rep_str A matrix. The number of rows should equal the length of
#'  \code{enc_check_results[[column_name]]}. The number of columns should equal
#'  the maximum number of byte sequences observed in any element of
#'  \code{enc_check_results[[column_name]]}. This can be queried using
#'  find_max_seq_length(). Elements within each row of this matrix are the
#'  replacement characters for each byte sequence observed in the corresponding
#'  elements of \code{enc_check_results[[column_name]]}, or a random filler
#'  character or string. Because some elements of
#'  \code{enc_check_results[[column_name]]} may have more byte sequences than
#'  others, and because the number of columns in the replacement matrix is equal
#'  to the maximum number of sequences, some matrix rows may need dummy strings
#'  in order to be completely filled. This matrix must be manually constructed,
#'  as there is no method for guessing the proper ASCII or UTF-8 character to
#'  replace an invalid byte sequence. The function is only capable of replacing
#'  single columns at a time. To replace additional columns, the data.table
#'  returned by this function must be fed back into the function as the value of
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
#'@return A data.table with the same structure as \code{dset}.
#'@export
replace_bytes <- function(dset, enc_check_results, column_name, rep_str) {

  # Replace control bytes with Unicode replacement symbol for easier pattern
  # replacement
  uni_repl <- control_to_rep_symbol(enc_check_results) %>%
    extract2(column_name)

  # Pattern replacement on column of unique values
  uni_repl_fix <- uni_repl
  for (j in 1:ncol(rep_str)) {
    uni_repl_fix <- mapply(sub,
                           rep_len("\ufffd+", length(uni_repl_fix)),
                           rep_str[,j],
                           uni_repl_fix) %>%
      unname
  }

  # Pattern replacement in dataset using column of unique values before and
  # after column replacement. Note that the dataset must be converted using
  # iconv to prevent errors about invalid strings. Additionally, the
  # pattern matching will only work in this case. We perform this same
  # operation in check_column_encoding.R; however, the data.table is not
  # returned. Thus, we must do it again. Note that the documentation says
  # that you can feed a data.table back into this function multiple times.
  # In that case, iconv becomes redundant. However, it shouldn't cause any
  # problems. If problems do arise, then investigate this line of code for
  # its effects on the second, etc. iteration of this function on a dataset.
  dset %<>% apply(2, iconv, to = "UTF-8", sub = "\ufffd")
  if (dset %>% is.data.table %>% not) dset %<>% data.table

  orig_results <- enc_check_results[[column_name]]
  column_to_fix <- dset[, get(column_name)]
  for (i in 1:length(uni_repl_fix)) {
    column_to_fix <- sub(pattern = orig_results[i],
                         replacement = uni_repl_fix[i],
                         x = column_to_fix,
                         fixed = TRUE)
  }
  dset[, (column_name) := column_to_fix]

  return(dset)
}