#'@title Find invalid UTF-8 bytes in a data.frame
#'@description \code{check_column_encoding} returns a list of \code{dset}
#'  observations where invalid UTF-8 or UTF-8 control bytes are detected by
#'  pattern matching, organized by original column name. NOTE: This function is
#'  intended only for use on UTF-8 systems, such as Mac OS X. If in doubt about
#'  your system encoding, run \code{Sys.getlocale()}.
#'@details  Each byte utilized in pattern matching by
#'  \code{check_column_encoding} is either a single-byte control character in
#'  UTF-8 in the range 0x80 - 0x9f or the Unicode replacement character, U+FFFD.
#'  This function uses iconv() to replace all invalid UTF-8 bytes with U+FFFD;
#'  however, it does not convert control characters that may be present. I leave
#'  intact any control characters within the range 0x00 - 0x1f, which R does
#'  utilize. For example, to represent the newline character. See ?Quotes for a
#'  complete list of control characters. In the future, I may also convert
#'  control characters such as the carriage return.
#'@param dset A data.frame or data.table.
#'@param column_names Names of data.frame columns to search. Defaults to
#'  \code{colnames(dset)}
#'@return A list of columns where invalid bytes are detected. Each list element,
#'  which corresponds to a column, contains a character vector of unique
#'  observations where detection occurred.
#'@export
check_column_encoding <- function(dset, column_names = colnames(dset)) {

  # Replace invalid bytes with Unicode replacement character
  dset %<>% apply(2, iconv, to = "UTF-8", sub = "\ufffd")

  # Convert dset to data.table
  if (dset %>% is.data.table %>% not) dset %<>% data.table

  # Control characters. These are displayed in the console as octal (for the
  # lower range) and Unicode escape sequences. They are not displayed as <xx>,
  # where x is a hex digit, when using View(). This is in contrast to invalid
  # bytes--the ones that iconv catches. Note: It is important to be aware that
  # R internally defines the range from \006 to \015 according to ASCII. Thus,
  # this function catches whitespace characters (plus some others). See
  # http://www.ascii-code.com/
  octal_control <- c("\001","\002","\003", "\004", "\005", "\006", "\007",
                     "\010", "\011", "\012", "\013", "\014", "\015", "\016",
                     "\017", "\020", "\021", "\022", "\023", "\024", "\025",
                     "\026", "\027", "\030", "\031", "\032", "\033", "\034",
                     "\035", "\036", "\037")
  uni_control <- c("\u0080", "\u0081", "\u0082", "\u0083", "\u0084", "\u0085",
                   "\u0086", "\u0087", "\u0088", "\u0089", "\u008a", "\u008b",
                   "\u008c", "\u008d", "\u008e", "\u008f", "\u0090", "\u0091",
                   "\u0092", "\u0093", "\u0094", "\u0095", "\u0096", "\u0097",
                   "\u0098", "\u0099", "\u009a", "\u009b", "\u009c", "\u009d",
                   "\u009e", "\u009f")
  invalid_bytes <- c(octal_control, uni_control, "\ufffd")

  # Use of a single, large string (but not too large ...) eliminates for loops
  # around ldset, below.
  invalid_string <- invalid_bytes[-length(invalid_bytes)] %>%
    paste0("|", collapse = "") %>%
    paste0(last(invalid_bytes), collapse = "")

  # Logical data.table indicating invalid strings in each cell
  # Editor's Note:
  # .SD seems to say apply over all columns specified, and .SDcols specifies
  # the columns to apply. The default value of column_names is set to all names.
  ldset <- dset[, lapply(.SD, function(x) grepl(invalid_string, x)),
                .SDcols = column_names]

  # Remove columns without encoding issues from dset
  lnz <- sapply(ldset, any)
  if (not(all(lnz))) {
    totally_valid_columns <- names(which(not(lnz)))
    dset[, (totally_valid_columns) := NULL]
    ldset[, (totally_valid_columns) := NULL]
    message(c("No encoding issues found in  the following columns: ",
              toString(totally_valid_columns))) # may need to use deparse(substitute(.))
  }

  # Editor's note: Produces class "list", but a type of list that I have never
  # seen before
  # ucol2 <- vector(mode = "list", length = dim(dset)[2])
  # for (i in seq_along(names(dset))) {
  #   ldset[, i, with = FALSE]
  #   ucol2[[i]] <- dset[ldset[, get(names(dset)[i])],
  #                     i,
  #                     with = FALSE] %>%
  #     unique
  # }

  # Collect invalid strings as elements of a named list
  ucol <- vector(mode = "list", length = dim(dset)[2])
  for (i in seq_along(names(dset))) {
    ucol[[i]] <- dset[ldset[, get(names(dset)[i])],
                      get(names(dset)[i])] %>%
      unique
  }


  names(ucol) <- names(dset)

  return(ucol)
}

