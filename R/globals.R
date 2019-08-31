globals <- list2env(list())

reset_globals <- function() {
  default_values <- list(
    pkg = NULL
  )

  mapply(names(default_values), default_values, FUN = assign, MoreArgs = list(globals))
  invisible()
}

reset_globals()
