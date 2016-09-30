#'@title Assistance in constructing replacement matrices.
#'@description Find maximum number of sequences in an element of the results of
#'  control_to_rep_symbol(check_column_encoding()). Results indicate the number
#'  of columns needed in the replacement matrix for the respective column name
#'  fed to replace_bytes(). Note that this function calls
#'  control_to_rep_symbol() on the results of check_column_encoding(). No action
#'  is needed by the user for this step.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}.
#'@return A named list whose element names correspond to those of
#'  check_column_encoding or control_to_rep_symbol(check_column_encoding()).
#'  Each element of this list contains a single number, which indicates the
#'  number of columns needed for the replacement matrix associated with a column
#'  name fed to replace_bytes().
#'@export
find_max_num_seqs <- function(enc_check_results) {

  if (convert_control) {
    uni_repl <- control_to_rep_symbol(enc_check_results) %>%
      extract2(column_name)
  } else {
    uni_repl <- enc_check_results[[column_name]]
  }

  max_seq_lengths <- function(x) {
    gregexpr("\ufffd+", x) %>%
      lengths %>%
      max
  }

  lapply(uni_repl, max_seq_lengths)
}