server <- function(input, output, session) {

  message("Initializing test results")
  initial_run_output <- filter_results(
    NULL, NULL, character(), 0L, 0, globals$pkg
  )

  call_stack_df <- NULL
  run_output <- initial_run_output

  observe(run_output <<- filter_results(
    session, run_output, input$filter, input$run, input$n_max, globals$pkg
  ))
  observe(call_stack_df <<- fill_call_stack(
    session, run_output$results, input$results, globals$pkg
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
