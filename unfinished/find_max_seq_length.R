find_max_seq_length <- function(dset, enc_check_results, column_name) {

  # Find maximum length of all byte sequences
  uni_control <- c("\u0080", "\u0081", "\u0082", "\u0083", "\u0084", "\u0085",
                   "\u0086", "\u0087", "\u0088", "\u0089", "\u008a", "\u008b",
                   "\u008c", "\u008d", "\u008e", "\u008f", "\u0090", "\u0091",
                   "\u0092", "\u0093", "\u0094", "\u0095", "\u0096", "\u0097",
                   "\u0098", "\u0099", "\u009a", "\u009b", "\u009c", "\u009d",
                   "\u009e", "\u009f")

  invalid_string <- uni_control %>%
    extract(., seq_along(.) - 1) %>%
    paste("|", collapse = "") %>%
    paste(last(uni_control), collapse = "") %>%
    gsub(" ", "", .)

  # Script might be broken here. I just realized it was using a variable
  # named for some test data. Bullshit.
  match_idx <- gregexpr(invalid_string, enc_check_results[[column_name]])

  # Max number of sequences
  for (j in seq_along(match_idx)) {
    list_slice <- match_idx[[j]]
    if (length(list_slice) > 1) {
      for (i in (1:(length(list_slice) - 1))) {
        if (list_slice[i] + 1 == list_slice[i + 1]) {
          list_slice[i] = NA
          match_idx[[j]][i] <- list_slice[i]
        }
      }
    }
    match_idx[[j]] <- match_idx[[j]][match_idx[[j]] %>% is.na %>% not]
  }
  max_num_seq <- match_idx %>% lengths %>% max
}