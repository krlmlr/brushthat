BrushReporter <- R6::R6Class("BrushReporter", inherit = testthat::Reporter,
  public = list(
    add_result = function(context, test, result) {
      private$results <- c(private$results, list(result))
    },

    get_results = function() {
      private$results
    }
  ),

  private = list(
    results = NULL
  )
)
