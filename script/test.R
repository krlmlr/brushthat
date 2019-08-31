# https://github.com/rstudio/rstudio/pull/5336
.rs.addFunction("findBreakpointSteps", function(funBody)
{
  if (typeof(funBody) != "language")
  {
    return(NULL)
  }
  for (idx in 1:length(funBody))
  {
    # if this is a doTrace call, we found a breakpoint; stop recursion here
    if (is.call(funBody[[idx]]) &&
        as.character(funBody[[idx]][[1]])[[1]] == ".doTrace")
    {
      return(idx + 1)
    }
    nestedSteps <- .rs.findBreakpointSteps(funBody[[idx]])
    if (!is.null(nestedSteps))
    {
      return(c(idx, nestedSteps))
    }
  }
})

pkgload::load_all()
brushthat::shine()
