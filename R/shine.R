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
          sliderInput("n_max", "Max # visible results (0: unlimited)", 0, 50, 10),
          actionLink("shell", "Shell"),
          radioButtons("call_stack", "Call stack", choices = "No test selected")
        )
      ),
      textOutput("message", container = htmltools::pre)
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

show_shell <- function() {
  showElement(id = "shell-overlay", anim = TRUE, animType = "fade")
  message("Entering shell, quit with c <Enter>")
  on.exit(browser(), add = TRUE)
  on.exit(
    hideElement(id = "shell-overlay", anim = FALSE),
    add = TRUE
  )
}
