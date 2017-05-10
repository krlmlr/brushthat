# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
shine <- function() {
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("brushthat", left = NULL),
    miniContentPanel(
      textInput("path", "Path", getwd()),
      actionButton("run", "Run test"),
      radioButtons("failures", "Failing tests", choices = "No test failures found"),
      textOutput("status", inline = TRUE)
    )
  )

  server <- function(input, output, session) {

    output$status <- renderText({
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
