fill_call_stack <- function(session, results, result_pos, pkg) {
  result <- get_result(results, result_pos)

  if (is.null(result)) {
    choices <- c("No result selected" = 1L)
    files <- NA_character_
    lines <- NA_integer_
    columns <- NA_integer_
  } else {
    srcrefs <- lapply(result$call, attr, "srcref")
    has_ref <- map_lgl(srcrefs, inherits, "srcref")
    valid_srcrefs <- srcrefs[has_ref]
    files <- map_chr(valid_srcrefs, function(x) attr(x, "srcfile")$filename)
    files <- gsub(paste0(pkg, "/"), "", files)
    lines <- map_int(valid_srcrefs, function(x) as.vector(x)[1])
    columns <- map_int(valid_srcrefs, function(x) as.vector(x)[2])

    addr <- paste0(files, ":", lines, ":", columns)
    choices <- set_names(seq_along(addr), addr)
  }

  call_stack_df <- data.frame(
    file = files, line = lines, column = columns,
    stringsAsFactors = FALSE
  )

  updateRadioButtons(session, "call_stack", choices = choices, selected = 1L)

  navigate_call_stack_entry(call_stack_df, 1L)
  call_stack_df
}

get_result_message <- function(session, results, result_pos) {
  result <- get_result(results, result_pos)

  if (is.null(result)) {
    ""
  } else {
    result$message
  }
}

navigate_call_stack_entry <- function(call_stack_df, call_stack_pos) {
  call_stack_pos <- as_numeric_or_na(call_stack_pos)
  if (is.na(call_stack_pos)) return()
  if (!is.numeric(call_stack_pos)) return()
  file_pos <- call_stack_df[call_stack_pos, , drop = FALSE]
  if (is.na(file_pos$file)) return()
  rstudioapi::navigateToFile(file_pos$file, file_pos$line, file_pos$column)
}
