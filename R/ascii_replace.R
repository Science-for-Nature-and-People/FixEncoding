#'@title Find and replace invalid UTF-8 bytes.
#'@description Replace invalid bytes detected by \code{enc_check} with valid
#'  ASCII or UTF-8 characters. Involves manual construction of a replacement
#'  vector (see \code{rep_str}).
#'@param dataset A basic data.frame. Untested for newer objects from dplyr,
#'  tibble, and data.table packages.
#'@param enc_check_results A matrix returned by calling \code{enc_check} with
#'  \code{dataset} as the sole argument.
#'@param column_name A column header returned by \code{enc_check}, corresponding
#'  to a column header in \code{dataset} where encoding issues were detected (or
#'  possibly false positives).
#'@param rep_str A character vector whose elements are ASCII characters, though
#'  any character can be used. These characters are used to replace the invalid
#'  bytes displayed by \code{enc_check_results$column_name$unique_errors \%>\%
#'  unname \%>\% unlist \%>\% unique}. Provisional: As of yet, the function only
#'  handles replacement of single invalid bytes sequences per element of the
#'  above code. This vector must be manually constructed, as there is no method
#'  for guessing the proper ASCII or UTF-8 character to replace an invalid byte
#'  sequence. Currently, the function is only capable of replacing single
#'  columns at a time. To replace additional columns, the data.frame returned by
#'  \code{ASCII_replace} must be fed back into the function as \code{dataset}.
#'@return A data.frame with the same structure as \code{dataset} but valid
#'   UTF-8 bytes.
#'@examples
#' \dontrun{
#' map_matches <- enc_check(map_data)
#' # Use to construct rep_str = ASCII_replacement
#' map_matches$Title$unique_errors %>% unname %>% unlist %>% unique
#' ASCII_replacement <- c(rep("a", times = 3),
#'                        "e",
#'                        rep("'", times = 12))
#' map_data_fixed <- ascii_replace(map_data,
#'                                 map_matches,
#'                                 "Title",
#'                                 ASCII_replacement)
#' }
#' @export
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
