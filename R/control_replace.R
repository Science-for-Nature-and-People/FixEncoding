#'@title Replace UTF-8 control bytes.
#'@description Replace control bytes detected by \code{check_column_encoding}
#'  with valid ASCII or UTF-8 characters. Involves manual construction of a
#'  replacement matrix (see \code{rep_str}). Control bytes are represented by
#'  a backslash followed by lowercase-u and a series of hexadecimal numbers.
#'@param dset A data.frame or data.table. Or, if you previously called
#'  diamond_replace.R and saved the output, the data.table it returned.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}.
#'@param column_name The name of an element in the list returned by
#'  \code{check_column_encoding}, or a column header in \code{dset} or the
#'  data.table returned by diamond_replace.R. These are all equivalent.
#'@param rep_str A matrix. The number of rows should equal the length of
#'  \code{enc_check_results[[column_name]]}. The number of columns should equal
#'  the maximum number of control bytes sequences observed in any element of
#'  \code{enc_check_results[[column_name]]}. Elements within each row of this
#'  matrix are the replacement characters for each control byte sequence
#'  observed in the corresponding elements of
#'  \code{enc_check_results[[column_name]]}, or a random filler character or
#'  string. Because some elements of \code{enc_check_results[[column_name]]} may
#'  have more control byte sequences than others, and because the number of
#'  columns in the replacement matrix is equal to the maximum number of
#'  sequences, some matrix rows may need dummy strings in order to be completely
#'  filled. This matrix must be manually constructed, as there is no method for
#'  guessing the proper ASCII or UTF-8 character to replace an invalid byte
#'  sequence. The function is only capable of replacing single columns at a
#'  time. To replace additional columns, the data.table returned by this
#'  function must be fed back into the function as the value of
#'  \code{dset}--likely with a different value for \code{rep_str}. While this
#'  may seem like an error-prone approach, remember that you can script your
#'  manual construction. A good idea is to use the same filler word throughout
#'  your matrix. Then call \code{grep} using that filler word as the value of
#'  the \code{pattern} argument and the result of calling \code{control_replace}
#'  as the value of argument \code{x}. If the filler word is matched, it
#'  probably means that you missed a secondary, tertiary, etc. byte sequence
#'  lurking in the corresponding element of
#'  \code{enc_check_results[[column_name]]}. (It's happened to me at least
#'  once!). Finally, if you simply want to get rid of a control character
#'  without substituting another character, simply use \code{""} as the value of
#'  that matrix element, as you normally would with \code{sub} pr \code{gsub}.
#'@return A data.table with the same structure as \code{dset}.
#'@export
control_replace <- function(dset, enc_check_results, column_name, rep_str) {

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

  max_seq_length <- vector(mode = "numeric", length = length(match_idx))
  for (j in seq_along(match_idx)) {
    list_slice <- match_idx[[j]]
    idx_differences <- rle(diff(list_slice))
    if (any(idx_differences$values == 1)) {
      idx_differences <- idx_differences[idx_differences$values == 1] # search for consecutive
      max_seq_length[j] <- idx_differences$lengths %>% max + 1
    } else {
      max_seq_length[j] = 1
    }
  }
  max_seq_length <- max_seq_length %>% max
  # # Max number of sequences. But probably not what we want
  # for (j in seq_along(match_idx)) {
  #   list_slice <- match_idx[[j]]
  #   if (length(list_slice) > 1) {
  #     for (i in (1:(length(list_slice) - 1))) {
  #       if (list_slice[i] + 1 == list_slice[i + 1]) {
  #         list_slice[i] = NA
  #         match_idx[[j]][i] <- list_slice[i]
  #       }
  #     }
  #   }
  #   match_idx[[j]] <- match_idx[[j]][match_idx[[j]] %>% is.na %>% not]
  # }
  # max_seq_length <- match_idx %>% lengths %>% max

  control_results <- enc_check_results[[column_name]]
  usingle_bytes <- sapply(uni_control, grep, control_results) %>%
    extract(., lapply(., length) > 0) %>%
    names
  if (max_seq_length > 1) {
    byte_grid <- lapply(1:max_seq_length,
                        function(x)
                          replicate(x,
                                    usingle_bytes,
                                    simplify = FALSE)) %>%
      sapply(expand.grid, stringsAsFactors = FALSE) %>%
      sapply(apply, 2, as.character) %>%
      sapply(function(x) split(x, 1:nrow(x))) %>%
      sapply(unname) %>%
      sapply(lapply, paste, collapse = "") %>%
      unlist
  } else {
    byte_grid <- usingle_bytes
  }

  # Find all byte sequences that match invalid bytes for each column observation
  third_match_fun <- function(j) sapply(byte_grid, grepl, j, useBytes = TRUE)
  lmatch <- sapply(control_results, third_match_fun)
  match_idx <- apply(lmatch, 2, which)
  # match_pattern <- sapply(match_idx, names)
  # grepl("\\x90", "\\x90\\xa9", fixed = TRUE)
  matches <- sapply(match_idx, names)
  max_num_matches <- max(lengths(matches))

  # Remove redundant byte sequence matches
  for (i in seq_along(matches)) {
    for (j in 1:(max_num_matches - 1)) {
      z <- 1
      while(j + z <= lengths(matches)[i]) {
        if (grepl(matches[[i]][j], matches[[i]][j + z], fixed = TRUE)) {
          matches[[i]][j]<- "NA"
          break
        } else z <- z + 1
      }
    }
  }

  # Create a list of orderly, exact byte-sequence matches for each observation in column_name
  pad_matches_NA <- lapply(matches, function(x) c(x, rep_len("NA", max_num_matches - length(x)))) %>%
    unname %>%
    do.call(rbind, .)

  pad_matches_NA_idx <- lapply(1:ncol(pad_matches_NA),
                 function(j) pad_matches_NA[, j]) %>%
    sapply(function(pattern) mapply(gregexpr,
                                    pattern,
                                    control_results,
                                    useBytes = TRUE)) %>%
    set_rownames(NULL)

  ordered_matches <- vector(mode = "list", length = length(matches))
  for (i in 1:nrow(pad_matches_NA)) {
    str_position <- pad_matches_NA_idx[i, ] %>% unlist
    length_by_row_els <- pad_matches_NA_idx[i,] %>% lengths
    row_names <- pad_matches_NA[i, ]
    names(str_position) <- mapply(rep_len, row_names, length_by_row_els) %>% unlist %>% unname
    ordered_matches[[i]] <- str_position[!(names(str_position) == "NA")] %>% sort %>% names
  }

  max_num_ordered_matches <- ordered_matches %>% lengths %>% max

  ordered_bytes <- lapply(ordered_matches,
                          function(x) c(x, rep_len("NA",
                                                   max_num_ordered_matches - length(x)))) %>%
    do.call(rbind, .)

  # Remove all-NA columns
  ordered_bytes[ordered_bytes == "NA"] <- NA
  tryCatch(
    {
      j <- 1
      while (j <= ncol(ordered_bytes)) {
        if (not(any(not(is.na(ordered_bytes[, j]))))) {
          ordered_bytes <- ordered_bytes[, -j]
          j = j - 1
        } else j = j + 1
      }
    },

    condition = function(c) {
    },
    finally = {
    }
  )
  ordered_bytes[is.na(ordered_bytes)] <- "Good luck matching this phrase using string matching" # there's no way this phrase will ever match anything, so we can keep the structure that allows us to use mapply(), below

  # Pattern replacement on column of unique values
  control_fix <- control_results
  for (j in 1:ncol(ordered_bytes)) {
    control_fix <- mapply(sub,
                          ordered_bytes[,j],
                          rep_str[,j],
                          control_fix) %>%
      unname # are these arguments necessary ^
  }


  # Pattern replacement in dataset using column of unique values before and after column replacement
  dset %<>% apply(2, iconv, to = "UTF-8", sub = "\ufffd")
  if (dset %>% is.data.table %>% not) dset %<>% data.table
  column_to_fix <- dset[, get(column_name)]
  for (i in 1:length(control_fix)) {
    column_to_fix <- sub(pattern = control_results[i],
                         replacement = control_fix[i],
                         x = column_to_fix,
                         fixed = TRUE) # this one is necessary
  }
  dset[, (column_name) := column_to_fix]


  return(dset)
}
