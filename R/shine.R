#' Displays a gadget that interacts with testthat
#'
#' Shows test results, allows navigating to failures and re-run.
#'
#' @param pkg The package to test
#' @export
shine <- function(pkg = ".") {
  pkg <- normalizePath(pkg, winslash = "/")

  globals$pkg <- pkg
  on.exit(reset_globals())

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
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
