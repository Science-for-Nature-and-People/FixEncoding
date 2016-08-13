#'@export
byte_generator <- function() {
  # Because we're running on OS X, we consider most of the special escape
  # sequences invalid. Macs only use \n to indicate a new line, unlike
  # Windows/DOS.
  valid_bytes <- c( '\n', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname

  invalid_bytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% valid_bytes)) %>%
    paste0("\\x", .) %>%
    c(., "\\xef\\xbf\\xbd") # plus Unicode replacement character
  # # Experimental
  # source("unfinished/NewEncodingStrategy.R") # can't use source in a package. would need to devise a workaround.
  # invalid_bytes <- exp_byte_generator("unfinished/bytes.RData")

  # Use of a single, large string (but not too large ...) eliminates for loops around ldset, below.
  invalid_string <- invalid_bytes %>%
    extract(., seq_along(.) - 1) %>%
    paste("|", collapse = "") %>%
    paste(last(invalid_bytes), collapse = "") %>%
    gsub(" ", "", .) # Had to rewrite to avoid a loop

  return(list(invalid_string = invalid_string, invalid_bytes = invalid_bytes))
}
