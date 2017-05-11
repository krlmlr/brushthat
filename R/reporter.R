BrushReporter <- R6::R6Class("BrushReporter", inherit = testthat::Reporter,
  public = list(
    start_test = function(context, test) {
      self$cat_line("Start test: ", test)
    },

    add_result = function(context, test, result) {
      private$results <- c(private$results, list(result))
      self$cat_line(gsub("^expectation_", "", class(result)[[1]]))
      # ref <- result$srcref
      # if (is.null(ref)) {
      #   location <- "?#?:?"
      # } else {
      #   location <- paste0(basename(attr(ref, "srcfile")$filename), "#", ref[1], ":1")
      # }
      #
      # status <- expectation_type(result)
      # self$cat_line("  ", location, " [", status, "]")
    },

    end_test = function(context, test) {
      self$cat_line("End test: ", test)
      # self$cat_line()
    },

    get_results = function() {
      private$results
    }
  ),

  private = list(
    results = NULL
  )
)
