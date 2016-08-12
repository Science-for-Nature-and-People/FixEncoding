#'@title Find and replace invalid UTF-8 bytes.
#'@description \code{enc_check2()} detects invalid UTF-8 bytes by using
#'  \code{stringi::stri_enc_isutf8()}. NOTE: This function is intended only for
#'  use on UTF-8 systems. If in doubt about your system encoding, run
#'  \code{Sys.getlocale()}.
#'@details Unlike \code{enc_check}, this function generates zero false
#'  positives. \code{enc_check} applies a character vector of regular
#'  expressions that evaluate to invalid bytes to pattern matching functions
#'  that search through dataset. \code{enc_check2} utilizes
#'  \code{stringi::stri_enc_isutf8()} and processes the results into a useable
#'  format.
#'@param dataset A basic data.frame. Untested for newer objects from dplyr,
#'  tibble, and data.table packages.
#'@return Provisional: A matrix whose column names are those of \code{dataset},
#'  or else a list with a single element if only one column of \code{dataset}
#'  contains invalid UTF-8 bytes.
#'@examples
#' # Call functions
#' \dontrun{
#' errors <- enc_check2(dataset)
#' }

enc_check2 <- function(dataset) {

  # Find indices of dataset elements with invalid UTF-8 bytes
  find_idx <- function(x) {
    if (sapply(x, is.character) %>% all) {
      utf8Bytes <- x %>% stri_enc_isutf8
    } else next
    utf8Bytes[is.na(utf8Bytes)] <- TRUE
    which(utf8Bytes == FALSE)
  }

  dataset_name <- deparse(substitute(dataset))
  idx <- apply(dataset, 2, find_idx)
  idx %<>%
    extract(., sapply(., function(x) length(x) > 0))

  # Find (unique) dataset elements with invalid UTF-8 bytes
  if(length(idx) > 0) {
    errors <- sapply(names(idx),
                     function(i) extract(dataset[[i]], idx[[i]]),
                     simplify = FALSE)
    unique_errors <- sapply(errors, unique, simplify = FALSE)
    if(length(names(idx)) > 1) {
      errorFrame <- list(idx = idx, errors = errors, unique_errors = unique_errors) %$%
        sapply(., as.matrix, rownames.force = TRUE) %>%
        set_rownames(., names(idx)) %>%
        t %>%
        as.data.frame
    } else {
      errorFrame <- list(idx = idx, errors = errors, unique_errors = unique_errors)
    }
    return(errorFrame)
  }
}
