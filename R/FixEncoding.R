#' FixEncoding: A package for locating and replacing invalid UTF-8 bytes
#'
#' The FixEncoding package provides three functions: check_column_encoding()
#' for locating invalid and control UTF-8 bytes; find_max_num_seqs(), a helper
#' function for constructing replacement matrices; and replace_bytes() for
#' using replacement matrices to replace unwanted characters/bytes in your
#' data.
#'
#' @docType package
#' @name FixEncoding
#' @import dplyr
#' @import magrittr
#' @import data.table
NULL
