#' @import tibble
BrushReporter <- R6::R6Class("BrushReporter", inherit = testthat::Reporter,
  public = list(
    initialize = function() {
      private$results <- tibble(
        file = character(),
        type = character(),
        test = character(),
        result = list()
      )
    },

    add_result = function(context, test, result) {
      private$results <- add_row(
        private$results,
        file = getSrcFilename(result$srcref, full.names = TRUE),
        type = class(result)[[1]],
        test = test,
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
