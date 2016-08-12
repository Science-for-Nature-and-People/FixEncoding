#'@title Find and replace invalid UTF-8 bytes in a data.frame.
#'
#'@description \code{enc_check()} returns a list of \code{dataset} values and
#'  indices where invalid UTF-8 bytes are detected by pattern matching. NOTE:
#'  This function is intened only for use on UTF-8 systems. If in doubt about
#'  your system encoding, run \code{Sys.getlocale()}.
#'@details See "Value". Row \code{searchable_idx} is best used for pattern
#'  matching in order to subset the original data to view values with specific
#'  invalid bytes. The same invalid byte may represent multiple different
#'  characters, so the indices corresponding to a byte may correspond to data
#'  values with different issues. Row \code{pretty_idx} makes for easy viewing,
#'  with indices of invalid bytes grouped by byte code point, but is not easily
#'  searched. It contains lists whose element names are backquoted regular
#'  expressions that confuse autocomplete. I have already extracted all
#'  potentially error-ridden data values in rows \code{errors} and
#'  \code{unique_errors}. For the sake of catching as many errors as possible,
#'  \code{enc_check} matches UTF-8 continuation bytes, which may be part of a
#'  valid byte sequence (UTF-8 code point consists of between 1 and 4 bytes.)
#'  Thus, there may be some false positives that should be visually checked. The
#'  reason I search for these is that sometimes R interprets segments of invalid
#'  UTF-8 byte sequences form valid UTF-8 sequences as valid byte sequences.
#'  Thus, only part of the invalid byte sequence is visually displayed as
#'  invalid. This is especially a problem with non-UTF-8 encodings, which may
#'  overlap in code point with UTF-8. Each byte utilized in pattern matching by
#'  \code{enc_check} is given as UTF-8 2-digit hexadecimal number/code points
#'  (rather than binary, decimal, or octal). In particular, the invalid bytes
#'  are the non-character code point for single-byte UTF-8 (ASCII), which
#'  includes continuation bytes. The code point of continuation bytes is not
#'  utilized by single-byte characters in UTF-8, enabling computers to
#'  distinguish between single-byte characters and mutli-byte characters without
#'  any ambiguity. The rationale for using non-single-byte code point is that
#'  even if the file being read is not UTF-8, any byte sequences that match
#'  UTF-8 byte sequences will be displayed as if it were UTF-8 on a Mac
#'  computer. This function therefore catches byte sequences that absolutely
#'  cannot be interpreted as UTF-8 characters and will instead be displayed as
#'  hexidecimal bytes rather than characters in R. It does not guarantee that
#'  the file is actually UTF-8, or that the characters displayed in R are the
#'  author's intended characters. Thus, there are false positives in
#'  \code{enc_check} that are not present in \code{enc_check2}. Of course,
#'  \code{enc_check2} misses bytes displayed in R as one-to-four-hex-digit
#'  Unicode (See ?Quotes). \code{enc_check} is able to locate these, albeit only
#'  because there is some overlap in hex code point between the single-byte
#'  UTF-8 and Unicode code points.
#'@param dataset A basic data.frame. Untested for newer objects from dplyr,
#'  tibble, and data.table packages.
#'@return \code{enc_check} Provisional: A matrix whose column names are those of the
#'  original data. Its rows consist of two different structures for indices and
#'  one row for data values.
#'@examples
#' # Call functions
#' \dontrun{
#' errors <- enc_check(dataset)
#' }
#'
#' # Assuming "Authors" is a valid column name in our dataset where invalid
#' bytes were detected
#' \dontrun{
#' searchAuthors <- errors$Authors$searchable_idx
#' }
#'
#' # Four backslashes are necessary for regular expression searches that use
#' character classes
#' \dontrun{
#' vector_idx <- grep('\\\\x[[:xdigit:]][[:xdigit:]]',
#'                    searchAuthors)
#' data_idx <- as.numeric(names(searchAuthors[vector_idx]))
#' dataset$Authors[data_idx]
#' }
#'
#' # Exact matching uses two backslashes; we specify actual hex numbers
#' instead of the hex class [:xdigit:]
#' \dontrun{
#' invalidBytes <- unique(searchAuthors)
#' vector_idx <- grep(invalidBytes[1],
#'                    searchAuthors,
#'                    fixed = TRUE)
#' data_idx <- as.numeric(names(searchAuthors[vector_idx]))
#' dataset$Authors[data_id]
#' }
#' @export
enc_check <- function(dataset) {

  validBytes <- c( '\n', '\r', '\t','\b', '\a', '\f', '\v', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname

  # invalidBytes <- c(0:31, 127:159) %>%
  #   as.hexmode %>%
  #   as.character %>%
  #   extract(., !(. %in% validBytes)) %>%
  #   paste0("\\x", .)

  replacementChar <- "\\xef\\xbf\\xbd"

  invalidBytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% validBytes)) %>%
    paste0("\\x", .) %>%
    c(., replacementChar)

  # source("/Users/LumpyGrads/Desktop/Temp/NewEncodingStrategy.R") # can't use source in a package. would need to devise a workaround.
  # invalidBytes <- byte_generator("/Users/LumpyGrads/Desktop/Temp/bytes.RData")

  # Create list of data.frame columns (as vectors) with invalid bytes as names
  # This is the performance bottleneck. The larger the vector of invalidBytes, the longer it takes to run this. I can't use the invalidBytes constructed from byte_generator, because of the sheer size of the vector.
  inner_apply <- function(x) sapply(invalidBytes, grep, x, useBytes = TRUE) %>%
    unlist
  byte_matches <- apply(dataset, 2, inner_apply) %>%
    list(bm = ., bm_len = lapply(., length)) %$%
    extract(bm, bm_len > 0)

  if (!(length(byte_matches) > 0)) {
    dataset_name <- deparse(substitute(dataset))
    message(c("No invalid bytes found in ", dataset_name))
  }

  # Indices of invalid bytes
  if (length(byte_matches) > 0) {
    viewOrWork <- function(list_element) {

      # For each list element (named vectors), switch the names of vector elements (bytes)
      # with vector elements (data.frame indices of matching bytes), and vice versa.
      vector_elements <- unname(list_element)
      element_names <- names(list_element) %>%
        substr(1, 4) # Remove numerical suffix that is automatically appended to bytes.
      searchable_vector <- set_names(element_names,
                                     vector_elements)

      viewable_list <- unique(searchable_vector) %>%
        sapply(., grep, searchable_vector, fixed = TRUE, simplify = FALSE) %>%
        lapply(., function(x) extract(names(searchable_vector), x)) %>%
        lapply(., as.numeric)

      return(list("searchable_idx" = searchable_vector, "pretty_idx" = viewable_list))
    }

    error_idx <- byte_matches %$%
      sapply(., viewOrWork) # simplify to array

    # Find unique dataset elements with possible errors
    pretty_idx <- error_idx["pretty_idx",]
    inner_apply2 <- function(i) lapply(pretty_idx[[i]],
                                       function(x) extract(dataset[[i]], x)
    )
    errors <- sapply(names(pretty_idx),
                     inner_apply2,
                     simplify = FALSE
    )

    # unique_errors <- errors %$% # seems to have crashed the script, even though it worked when not sourcing
    #   sapply(., lapply, unique, simplify = FALSE)
    inner_apply3 <- function(i) lapply(i, unique)
    unique_errors <- sapply(errors, inner_apply3, simplify = FALSE)

    errorFrame <- rbind(error_idx, errors, unique_errors) %>%
      as.data.frame

    return(errorFrame)
  }
}
