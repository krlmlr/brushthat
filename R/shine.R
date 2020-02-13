#' Displays a gadget that interacts with testthat
#'
#' Shows test results, allows navigating to failures and re-run.
#'
#' @param remote Set to `FALSE` to run the gadget in blocking mode.
#'   The default starts a new R process.
#'   Currently, navigating to files is broken with `remote = TRUE`.
#' @param pkg Deprecated.
#' @export
shine <- function(remote = TRUE, pkg = NULL) {
  if (!is.logical(remote)) {
    lifecycle::deprecate_soft(
      "brushthat::shine(remote = 'can\\'t be a string')",
      details = "The first argument has changed to `remote`. The package to test is defined by from `usethis::proj_get()`."
    )
    remote <- TRUE
  }

  if (!is.null(pkg)) {
    lifecycle::deprecate_soft(
      "brushthat::shine(pkg = 'can\\'t be set')",
      details = "The package to test is defined by from `usethis::proj_get()`.")
  }

  if (remote) {
    shine_remote()
  } else {
    shine_local()
  }
}

shine_remote <- function() {
  ps <- callr::r_bg(
    function() brushthat::shine(remote = FALSE),
    supervise = TRUE
  )

  port_rx <- "^Listening on .*:([0-9]+)$"

  time <- Sys.time()
  port <- NULL
  while (time + 5 > Sys.time()) {
    error_lines <- ps$read_error_lines()
    match <- grep(port_rx, error_lines)
    if (length(match) > 0) {
      port <- gsub(port_rx, "\\1", error_lines[match][[1]])
      break
    }
    Sys.sleep(0.001)
  }

  if (is.null(port)) {
    ps$kill_tree()
    stop("Can't detect port number.")
  }

  url <- paste0("http://localhost:", port)

  time <- Sys.time()
  destfile <- tempfile()
  text <- NULL
  while (time + 5 > Sys.time()) {
    tryCatch(
      {
        withr::with_connection(
          list(con = curl::curl(url)),
          text <- readLines(con)
        )
        break
      },
      error = function(e) {}
    )
    Sys.sleep(0.001)
  }

  if (is.null(text)) {
    ps$kill_tree()
    stop("Can't connect to gadget.")
  }

  if (rstudioapi::isAvailable(child_ok = FALSE)) {
    rstudioapi::viewer(url)
  } else {
    utils::browseURL(url)
  }

  ps$supervise(FALSE)
  invisible(ps)
}

shine_local <- function() {
  pkg <- usethis::proj_get()

  globals$pkg <- pkg
  on.exit(reset_globals())

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space.
  viewer <- paneViewer(300)
  withr::with_options(
    c(shiny.quiet = FALSE),
    runGadget(ui, server, viewer = viewer)
  )
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
