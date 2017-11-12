result_type <- function(result) {
  result_types[ class(result)[[1]] ]
}

get_run_output <- function(session, run_output, run, pkg) {
  if (identical(unclass(run), unclass(run_output$run))) return(run_output)

  if (!is.null(session)) {
    showElement(id = "testing-overlay", anim = TRUE, animType = "fade")
    on.exit(
      hideElement(id = "testing-overlay", anim = FALSE),
      add = TRUE
    )
  }

  results <- run_tests(pkg)

  list(
    run = run,
    results = results
  )
}

run_tests <- function(pkg) {
  reporter <- BrushReporter$new()
  devtools::test(pkg = pkg, reporter = reporter, quiet = TRUE)
  reporter$get_results()
}

filter_results <- function(session, run_output, filter, run, n_max, pkg) {
  run_output <- get_run_output(session, run_output, run, pkg)
  results_df <- run_output$results

  show_result <- which(results_df$type %in% filter)
  if (n_max > 0 && length(show_result) > n_max) {
    show_result <- show_result[seq_len(n_max)]
  }

  results <- results_df$result
  shown_results <- results[show_result]

  if (length(shown_results) == 0L) {
    choices <- set_names(0L, paste0("Showing 0 out of ", length(results), " results"))
  } else {
    test_names <- paste0(
      map_chr(shown_results, result_type),
      ": ",
      map_chr(shown_results, "[[", "test")
    )
    choices <- set_names(show_result, test_names)
  }

  if (!is.null(session)) {
    updateRadioButtons(session, "results", choices = choices)
  }

  run_output
}

get_result <- function(results, result_pos) {
  result <- NULL
  result_pos <- as_numeric_or_na(result_pos)
  if (!is.na(result_pos) && result_pos != 0) {
    result <- results$result[[result_pos]]
  }
  result
}
