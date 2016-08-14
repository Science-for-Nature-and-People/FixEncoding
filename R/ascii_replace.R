#'@title Replace invalid UTF-8 bytes.
#'@description Replace invalid bytes detected by \code{check_column_encoding}
#'  with valid ASCII or UTF-8 characters. Involves manual construction of a
#'  replacement vector (see \code{rep_str}).
#'@param dset A data.frame or data.table.
#'@param enc_check_results A list returned by calling
#'  \code{check_column_encoding}.
#'@param column_name The name of an element in the list returned by
#'  \code{check_column_encoding}, corresponding to a column header in
#'  \code{dset} where encoding issues were detected (or possibly false
#'  positives).
#'@param rep_str A matrix. The number of rows should equal the length of 
#'  \code{enc_check_results[[column_name]]}. The number of columns should equal
#'  the maximum number of invalid bytes sequences observed in any element of 
#'  \code{enc_check_results[[column_name]]}. Strings within the character vector
#'  consist of either 1) a single character to replace in the corresponding
#'  strings of \code{enc_check_results[["column_name]]}, or 2) a random filler
#'  character or string. That is, because some elements of
#'  \code{enc_check_results[[column_name]]} may have more invalid bytes
#'  sequences than others, and because the number of columns in the replacement
#'  matrix is equal to the maximum number of invalid sequences, some matrix rows
#'  may need dummy strings in order to be completely filled. This matrix must be
#'  manually constructed, as there is no method for guessing the proper ASCII or
#'  UTF-8 character to replace an invalid byte sequence. The function is only
#'  capable of replacing single columns at a time. To replace additional
#'  columns, the data.table returned by \code{ascii_replace} must be fed back
#'  into the function as the value of \code{dset}--likely with a different value
#'  for \code{rep_str}.
#'@return A data.table with the same structure as \code{dset} but valid UTF-8
#'  bytes.
#'@export
ascii_replace <- function(dset, enc_check_results, column_name, rep_str) {

  # Generate all possible byte sequences based on enc_check_results
  valid_bytes <- c( '\n', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname

  invalid_bytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% valid_bytes)) %>%
    paste0("\\x", .) %>%
    c(., "\\xef\\xbf\\xbd") # plus Unicode replacement character

  usingle_bytes <- sapply(invalid_bytes, grep, enc_check_results[[column_name]], useBytes = TRUE) %>% 
    unlist %>% 
    names %>% 
    substr(1, 4) %>% 
    unique
  byte_grid <- lapply(1:length(usingle_bytes), 
                      function(x) replicate(x, 
                                            usingle_bytes, 
                                            simplify = FALSE)) %>%
    sapply(expand.grid, stringsAsFactors = FALSE) %>%
    sapply(apply, 2, as.character) %>%
    sapply(function(x) split(x, 1:nrow(x))) %>%
    sapply(unname) %>%
    sapply(lapply, paste, collapse = "") %>%
    unlist
 
  # Find all byte sequences that match invalid bytes for each column observation
  third_match_fun <- function(j) sapply(byte_grid, grepl, j, useBytes = TRUE)
  lmatch <- sapply(enc_check_results[[column_name]], third_match_fun)
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
  
  temp <- lapply(1:ncol(pad_matches_NA), 
                 function(j) pad_matches_NA[, j]) %>%
    sapply(function(pattern) mapply(gregexpr, 
                                    pattern, 
                                    enc_check_results[[column_name]], 
                                    useBytes = TRUE)) %>% 
    set_rownames(NULL) 
  
  temp_list <- vector(mode = "list", length = length(matches))
  for (i in 1:nrow(pad_matches_NA)) {
    str_position <- temp[i, ] %>% unlist
    length_by_row_els <- temp[i,] %>% lengths
    row_names <- pad_matches_NA[i, ]
    names(str_position) <- mapply(rep_len, row_names, length_by_row_els) %>% unlist %>% unname
    temp_list[[i]] <- str_position[!(names(str_position) == "NA")] %>% sort %>% names
  }
  
  ordered_bytes <- lapply(temp_list, 
                         function(x) c(x, rep_len("NA", 
                                                  max_num_matches - length(x)))) %>%
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
  enc_check_results_fix <- enc_check_results[[column_name]]
  for (j in 1:ncol(ordered_bytes)) {
    enc_check_results_fix <- mapply(sub, 
           ordered_bytes[,j], 
           rep_str[,j], 
           enc_check_results_fix, 
           MoreArgs = list(useBytes = TRUE)) %>% 
      unname
  }
  
  # Pattern replacement in original dataset using column of unique values before and after column replacement
  if (dset %>% is.data.table %>% not) dset %<>% as.data.table
  column_fix <- dset[, get(column_name)]
  for (i in 1:length(enc_check_results_fix)) {
    column_fix <- sub(pattern = enc_check_results[[column_name]][i],
                      replacement = enc_check_results_fix[i],
                      x = column_fix,
                      fixed = TRUE,
                      useBytes = TRUE)
    # column <- column_fix
  }
  # dset[, (column_name) := column]
  dset[, (column_name) := column_fix]
  return(dset)
}




# setkey(subset_col) # sets all columns as keys (i.e. can subset rows by character vectors, where order corresponds to column order). used later for easy joins
# subset_col[subset_col[7]] # exmaple of join. I guess the values don't need to be quoted

# Notes
# subset_col <- dset[, mget(column_names)] # same effect as dplyr::select for multiple columns
