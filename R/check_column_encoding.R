#'@title Find data.frame/data.table invalid UTF-8 bytes in a data.frame.
#'@description \code{check_column_encoding} returns a list of \code{dset}
#'  observations where invalid UTF-8 bytes are detected by pattern matching,
#'  organized by original column name. NOTE: This function is intended only for
#'  use on UTF-8 systems. If in doubt about your system encoding, run
#'  \code{Sys.getlocale()}.
#'@details  Each byte utilized in pattern matching by
#'  \code{check_column_encoding} is given as UTF-8 2-digit hexadecimal
#'  number/code points (rather than binary, decimal, or octal). In particular,
#'  the invalid bytes are the non-character code point for single-byte UTF-8
#'  (ASCII), which includes continuation bytes. The code point of continuation
#'  bytes is not utilized by single-byte characters in UTF-8, enabling computers
#'  to distinguish between single-byte characters and mutli-byte characters
#'  without any ambiguity. The rationale for using non-single-byte code point is
#'  that even if the file being read is not UTF-8, any byte sequences that match
#'  UTF-8 byte sequences will be displayed as if it were UTF-8 on a Mac
#'  computer. This function therefore catches byte sequences that absolutely
#'  cannot be interpreted as UTF-8 characters and will instead be displayed as
#'  hexidecimal bytes rather than characters in R. It does not guarantee that
#'  the file is actually UTF-8, or that the characters displayed in R are the
#'  author's intended characters. For the sake of catching as many errors as
#'  possible, \code{check_column_encoding} matches UTF-8 continuation bytes,
#'  which may be part of a valid byte sequence (UTF-8 code point consists of
#'  between 1 and 4 bytes.) There may be some false positives that should be
#'  visually checked. The reason I search for these is that sometimes R
#'  interprets segments, or sub-sequences, of invalid UTF-8 byte sequences as
#'  valid sequences. Thus, only part of the invalid byte sequence is visually
#'  displayed as invalid. This is especially a problem with non-UTF-8 encodings,
#'  which may overlap in code point with UTF-8. \code{enc_check2} lacks false
#'  positives but also seems to be incapable of catching invalid bytes that
#'  display in RStudio as continuation bytes.
#'@param dset A data.frame or data.table.
#'@param column_names A character vector whose elements are the column names of
#'  \code{dset} to be searched. Defaults to all columns.
#'@return A list of columns where invaliad bytes are detected. Each list
#'  element, which corresponds to a column, contains a character vector of
#'  unique observations where detection occurred.
#'@export
check_column_encoding <- function(dset, column_names = colnames(dset)) {

  # Convert dset to data.table
  if (dset %>% is.data.table %>% not) dset %<>% as.data.table

  # Select columns based on input column_names into function. Can increase speed of execution for large dsets.
  if (not(all(is_in(colnames(dset), column_names)))) {
    not_names <- colnames(dset) %in%
      column_names %>%
      not %>%
      extract(names(dset), .)
    dset[, (not_names) := NULL] # replacement syntax for with = FALSE and character value to argument j
  }

  # Search for encoding issues by column
  valid_bytes <- c( '\n', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname

  invalid_bytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% valid_bytes)) %>%
    paste0("\\x", .) %>%
    c(., "\\xef\\xbf\\xbd") # plus Unicode replacement character

  # Use of a single, large string (but not too large ...) eliminates for loops around ldset, below.
  invalid_string <- invalid_bytes %>%
    extract(., seq_along(.) - 1) %>%
    paste("|", collapse = "") %>%
    paste(last(invalid_bytes), collapse = "") %>%
    gsub(" ", "", .) # Had to rewrite to avoid a loop

  ldset <- dset[, lapply(.SD, function(x) grepl(invalid_string, x, useBytes = TRUE)), .SDcols = column_names] # syntax is iffy?

  # Remove columns without encoding issues from dset
  lnz <- sapply(ldset, any)
  if (not(all(lnz))) {
    totally_valid_columns <- names(which(not(lnz)))
    dset[, (totally_valid_columns) := NULL]
    ldset[, (totally_valid_columns) := NULL]
    message(c("No encoding issues found in  the following columns: ",
              toString(totally_valid_columns))) # may need to use deparse(substitute(.))
  }

  ucol <- vector(mode = "list", length = dim(dset)[2])
  for (i in seq_along(names(dset))) {
    ucol[[i]] <- dset[ldset[, get(names(dset)[i])] == TRUE,
                       get(names(dset)[i])] %>%
      unique
  }
  names(ucol) <- names(dset)

  return(ucol)
}


