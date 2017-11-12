as_numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

commas <- function(x) {
  if (length(x) == 0) return("")
  MAX_LEN <- 6
  if (length(x) > MAX_LEN) {
    x <- c(as.character(x[seq_len(MAX_LEN - 1)]), "...")
  }
  paste(x, collapse = ", ")
}
