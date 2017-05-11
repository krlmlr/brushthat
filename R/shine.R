# Our ui will be a simple gadget page, which
# simply displays the time in a 'UI' output.
ui <- miniPage(
  gadgetTitleBar(
    "brushthat",
    left = miniTitleBarButton("run", "Run test", primary = TRUE)
  ),
  miniContentPanel(
    splitLayout(
      radioButtons("results", "Tests", choices = "No tests found"),
      radioButtons("call_stack", "Call stack", choices = "No test selected")
    ),
    textOutput("status", inline = TRUE)
  )
)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
shine <- function(pkg = ".") {
  pkg <- normalizePath(pkg, winslash = "/")

  server <- function(input, output, session) {

    results <- NULL
    call_stack_df <- NULL

    output$status <- renderText({
      reporter <- BrushReporter$new()
      devtools::test(pkg = pkg, reporter = reporter, quiet = TRUE)

      results <<- reporter$get_results()

      test_names <- paste0(
        map_chr(results, result_type),
        ": ",
        map_chr(results, "[[", "test")
      )
      choices <- set_names(seq_along(results), test_names)

      updateRadioButtons(session, "results", choices = choices)

      paste0("Runs: ", input$run)
    })

    observe(call_stack_df <<- fill_call_stack(session, results[[as.integer(input$results)]], pkg))

    observe(navigate_call_stack_entry(call_stack_df, as.numeric(input$call_stack)))

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}

result_type <- function(result) {
  switch(class(result)[[1]],
    expectation_success = "\u2713",
    expectation_failure = "\u2717",
    expectation_error = "\u2763",
    expectation_skip = "\u2013",
    expectation_warning = "\u26a0",
    "?"
  )
}

fill_call_stack <- function(session, result, pkg) {
  if (is.null(result)) return()

  srcrefs <- lapply(result$call, attr, "srcref")
  has_ref <- map_lgl(srcrefs, inherits, "srcref")
  valid_srcrefs <- srcrefs[has_ref]
  files <- map_chr(valid_srcrefs, function(x) attr(x, "srcfile")$filename)
  files <- gsub(paste0(pkg, "/"), "", files)
  lines <- map_int(valid_srcrefs, function(x) as.vector(x)[1])
  columns <- map_int(valid_srcrefs, function(x) as.vector(x)[2])

  addr <- paste0(files, ":", lines, ":", columns)
  choices <- set_names(seq_along(addr), addr)

  updateRadioButtons(session, "call_stack", choices = choices, selected = 1L)

  call_stack_df <- data.frame(
    file = files, line = lines, column = columns,
    stringsAsFactors = FALSE
  )
  navigate_call_stack_entry(call_stack_df, 1L)
  call_stack_df
}

navigate_call_stack_entry <- function(call_stack_df, call_stack_pos) {
  if (is.na(call_stack_pos)) return()
  file_pos <- call_stack_df[call_stack_pos, , drop = FALSE]
  print(file_pos)
  rstudioapi::navigateToFile(file_pos$file, file_pos$line, file_pos$column)
}
