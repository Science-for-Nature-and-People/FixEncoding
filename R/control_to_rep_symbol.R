#'@title Convert control bytes to Unicode replacement symbol
#'@description For usage by replace_bytes() and find_max_num_seqs().
#'@return A list similar to the one returned by calling
#'\code{check_column_encoding} but with control bytes sequences converted to
#'a sequence of Unicode replacement symbols.
#'@export
control_to_rep_symbol <- function(enc_check_results) {
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
  invalid_bytes <- c(octal_control, uni_control)

  invalid_string <- invalid_bytes[-length(invalid_bytes)] %>%
    paste0("|", collapse = "") %>%
    paste0(last(invalid_bytes), collapse = "")

  lapply(enc_check_results,
         function(x) gsub(invalid_string, replacement = "\ufffd", x))
}