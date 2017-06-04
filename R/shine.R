result_types <- c(
  expectation_success = "\u2713",
  expectation_failure = "\u2717",
  expectation_error = "\u2757",
  expectation_skip = "\u2013",
  expectation_warning = "\u26a0"
)

get_ui <- function() {
  appCSS <- "
.overlay {
  position: absolute;
  background: #DDDDDD;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
  display: none;
}
"

  ui <- miniPage(
    useShinyjs(),
    inlineCSS(appCSS),
    div(
      id = "testing-overlay",
      class = "overlay",
      h2("Updating test results...")
    ),
    div(
      id = "shell-overlay",
      class = "overlay",
      h2("Shell is active"),
      p("Quit shell with ", HTML("<kbd>c</kbd> <kbd>Enter</kbd>"), " to continue")
    ),
    gadgetTitleBar(
      "brushthat",
      left = miniTitleBarButton("run", "Run test", primary = TRUE)
    ),
    miniContentPanel(
      splitLayout(
        radioButtons("results", "Results", choices = "Running tests"),
        list(
          checkboxGroupInput(
            "filter", "Result types",
            set_names(names(result_types), result_types),
            selected = names(result_types)[-1],
            inline = TRUE
          ),
          sliderInput("n_max", "Max # results (0: unlimited)", 0, 50, 0),
          actionLink("shell", "Shell"),
          radioButtons("call_stack", "Call stack", choices = "No test selected")
        )
      ),
      textOutput("message")
    )
  )

  ui
}

#' Displays a gadget that interacts with testthat
#'
#' Shows test results, allows navigating to failures and re-run.
#'
#' @param pkg The package to test
#' @export
shine <- function(pkg = ".") {
  pkg <- normalizePath(pkg, winslash = "/")

  message("Initializing test results")
  initial_run_output <- filter_results(
    NULL, NULL, character(), 0L, 0, pkg
  )

  server <- function(input, output, session) {

    call_stack_df <- NULL
    run_output <- initial_run_output

    observe(run_output <<- filter_results(
      session, run_output, input$filter, input$run, input$n_max, pkg
    ))
    observe(call_stack_df <<- fill_call_stack(
      session, run_output$results, input$results, pkg
    ))
    output$message <- renderText(get_result_message(
      session, run_output$results, input$results
    ))

    observe(navigate_call_stack_entry(
      call_stack_df, input$call_stack
    ))

    observeEvent(input$shell, show_shell())

    observeEvent(input$done, {
      stopApp()
    })
    session$onSessionEnded(stopApp)

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(get_ui(), server, viewer = viewer)

}

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

  reporter <- BrushReporter$new()
  devtools::test(pkg = pkg, reporter = reporter, quiet = TRUE)

  list(
    run = run,
    results = reporter$get_results()
  )
}

filter_results <- function(session, run_output, filter, run, n_max, pkg) {
  run_output <- get_run_output(session, run_output, run, pkg)
  results <- run_output$results

  results_class <- map_chr(map(results, class), "[[", 1L)
  show_result <- which(results_class %in% filter)
  if (n_max > 0 && length(show_result) > n_max) {
    show_result <- show_result[seq_len(n_max)]
  }
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
  if (!is.numeric(call_stack_pos)) return()
  file_pos <- call_stack_df[call_stack_pos, , drop = FALSE]
  if (is.na(file_pos$file)) return()
  rstudioapi::navigateToFile(file_pos$file, file_pos$line, file_pos$column)
}

show_shell <- function() {
  showElement(id = "shell-overlay", anim = TRUE, animType = "fade")
  message("Entering shell, quit with c <Enter>")
  on.exit(browser(), add = TRUE)
  on.exit(
    hideElement(id = "shell-overlay", anim = FALSE),
    add = TRUE
  )
}
