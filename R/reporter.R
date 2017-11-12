#' @import tibble
BrushReporter <- R6::R6Class("BrushReporter", inherit = testthat::Reporter,
  public = list(
    initialize = function() {
      private$results <- tibble(file = character(), result = list())
    },

    start_file = function(name) {
      private$current_file <- name
    },

    add_result = function(context, test, result) {
      private$results <- add_row(
        private$results,
        file = private$current_file %||% "<unknown>",
        result = list(result)
      )
    },

    get_results = function() {
      private$results
    }
  ),

  private = list(
    current_file = NULL,
    results = NULL
  )
)
