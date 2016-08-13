#'@export
check_column_encoding <- function(dset, column_names = colnames(dset)) {

  # Convert dataset to data.table
  dset %<>% as.data.table # consider adding some tryCatch later for different kinds of inputs

  # Select columns based on input column_names into function. Can increase speed of execution for large datasets.
  if (not(all(is_in(colnames(dset), column_names)))) {
    not_names <- colnames(dset) %in%
      column_names %>%
      not %>%
      extract(names(dset), .)
    dset[, (not_names) := NULL] # replacement syntax for with = FALSE and character value to argument j
  }

  # Search for encoding issues by column
  valid_bytes <- c( '\n', '\\', "\'", "\"", '\`') %>%
    sapply(charToRaw) %>%
    unname
  
  invalid_bytes <- c(0:31, 127:255) %>%
    as.hexmode %>%
    as.character %>%
    extract(., !(. %in% valid_bytes)) %>%
    paste0("\\x", .) %>%
    c(., "\\xef\\xbf\\xbd") # plus Unicode replacement character
  
  # Use of a single, large string (but not too large ...) eliminates for loops around ldset, below.
  invalid_string <- invalid_bytes %>%
    extract(., seq_along(.) - 1) %>%
    paste("|", collapse = "") %>%
    paste(last(invalid_bytes), collapse = "") %>%
    gsub(" ", "", .) # Had to rewrite to avoid a loop
  
  ldset <- dset[, lapply(.SD, function(x) grepl(invalid_string, x, useBytes = TRUE)), .SDcols = column_names] # syntax is iffy?

  # Remove columns without encoding issues from dset
  lnz <- sapply(ldset, any)
  if (not(all(lnz))) {
    totally_valid_columns <- names(which(not(lnz)))
    dset[, (totally_valid_columns) := NULL]
    ldset[, (totally_valid_columns) := NULL]
    message(c("No encoding issues found in  the following columns: ",
              toString(totally_valid_columns))) # may need to use deparse(substitute(.))
  }

  ucol <- vector(mode = "list", length = dim(dset)[2])
  for (i in seq_along(names(dset))) {
    ucol[[i]] <- dset[ldset[, get(names(dset)[i])] == TRUE,
                       get(names(dset)[i])] %>%
      unique
  }
  names(ucol) <- names(dset)

  return(ucol)
}


