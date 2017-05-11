# Our ui will be a simple gadget page, which
# simply displays the time in a 'UI' output.
ui <- miniPage(
  gadgetTitleBar(
    "brushthat",
    left = miniTitleBarButton("run", "Run test", primary = TRUE)
  ),
  miniContentPanel(
    radioButtons("failures", "Failing tests", choices = "No test failures found"),
    textOutput("status", inline = TRUE)
  )
)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
shine <- function(pkg = ".") {
  server <- function(input, output, session) {

    results <- NULL

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

      updateRadioButtons(session, "failures", choices = choices)

      paste0("Runs: ", input$run)
    })

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
  toupper(substr(gsub("^expectation_", "", class(result)[[1]]), 1, 1))
}
