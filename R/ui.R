ui <- function() {
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

  miniPage(
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
}

result_types <- c(
  expectation_success = "\u2713",
  expectation_failure = "\u2717",
  expectation_error = "\u2757",
  expectation_skip = "\u2013",
  expectation_warning = "\u26a0"
)
