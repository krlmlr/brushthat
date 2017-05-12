result_types <- c(
  expectation_success = "\u2713",
  expectation_failure = "\u2717",
  expectation_error = "\u2757",
  expectation_skip = "\u2013",
  expectation_warning = "\u26a0"
)

ui <- miniPage(
  gadgetTitleBar(
    "brushthat",
    left = miniTitleBarButton("run", "Run test", primary = TRUE)
  ),
  miniContentPanel(
    checkboxGroupInput(
      "filter", "Result types",
      set_names(names(result_types), result_types),
      selected = names(result_types)[-1],
      inline = TRUE
    ),
    splitLayout(
      radioButtons("results", "Results", choices = "Running tests"),
      radioButtons("call_stack", "Call stack", choices = "No test selected")
    ),
    textOutput("message")
  )
)

#' Displays a gadget that interacts with testthat
#'
#' Shows test results, allows navigating to failures and re-run.
#'
#' @param pkg The package to test
#' @export
shine <- function(pkg = ".") {
  pkg <- normalizePath(pkg, winslash = "/")

  for (i in 1:20) {
    port <- get_random_port()
    tryCatch(
      {
        system2(
          file.path(Sys.getenv("R_HOME"), "bin", "R"),
          c("-q", "-e", shQuote(paste0("brushthat:::serve('", pkg, "',", port, ")"))),
          wait = FALSE
        )
        Sys.sleep(10)
        rstudioapi::viewer(paste0("http://127.0.0.1:", port), 300)
        break
      },
      error = function(e) {}
    )
  }
}

# Reject ports in this range that are considered unsafe by Chrome
# http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
ports <- setdiff(3000:8000, c(3659L, 4045L, 6000L, 6665:6669))

get_random_port <- function() {
  sample(ports, 1L)
}

serve <- function(pkg, port) {
  server <- function(input, output, session) {

    run_output <- NULL
    call_stack_df <- NULL

    observe(run_output <<- filter_results(
      session, run_output, input$filter, input$run, pkg
    ))
    observe(call_stack_df <<- fill_call_stack(
      session, run_output$results, as.integer(input$results), pkg
    ))
    output$message <- renderText(get_result_message(
      session, run_output$results, as.integer(input$results)
    ))

    observe(navigate_call_stack_entry(
      call_stack_df, as.numeric(input$call_stack)
    ))

    observeEvent(input$done, {
      stopApp()
    })
    session$onSessionEnded(stopApp)

  }

  runApp(shinyApp(ui, server, options = list(port = port, launch.browser = FALSE)))
}

result_type <- function(result) {
  result_types[ class(result)[[1]] ]
}

get_run_output <- function(run_output, run, pkg) {
  if (identical(run, run_output$run)) return(run_output)

  reporter <- BrushReporter$new()
  devtools::test(pkg = pkg, reporter = reporter, quiet = TRUE)

  list(
    run = run,
    results = reporter$get_results()
  )
}

filter_results <- function(session, run_output, filter, run, pkg) {
  run_output <- get_run_output(run_output, run, pkg)
  results <- run_output$results

  results_class <- map_chr(map(results, class), "[[", 1L)
  show_result <- results_class %in% filter
  shown_results <- results[show_result]

  if (length(shown_results) == 0L) {
    choices <- set_names(0L, paste0("Showing 0 out of ", length(results), " results"))
  } else {
    test_names <- paste0(
      map_chr(shown_results, result_type),
      ": ",
      map_chr(shown_results, "[[", "test")
    )
    choices <- set_names(which(show_result), test_names)
  }

  updateRadioButtons(session, "results", choices = choices)

  run_output
}

get_result <- function(results, result_pos) {
  result <- NULL
  if (!is.na(result_pos) && is.numeric(result_pos) && result_pos != 0) {
    result <- results[[result_pos]]
  }
  result
}

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
  if (is.na(call_stack_pos)) return()
  file_pos <- call_stack_df[call_stack_pos, , drop = FALSE]
  if (is.na(file_pos$file)) return()
  rstudioapi::navigateToFile(file_pos$file, file_pos$line, file_pos$column)
}
