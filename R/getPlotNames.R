#' @title getPlotNames
#'
#' @description Returns the names of plots from an \code{NPSForVeg} object that meet various criteria.
#'
#' @inheritParams getPlots
#' @param output Either "vector" (the default) or "list". Note that this must be in qutoes. Determines the type of output from the function
#' @param ... Other arguments that are passed on to \code{getPlots}.
#'
#' @details This function is a wrapper for \code{getPlots()$Plot_Name}.  It first calls \code{\link{getPlots}} and then extracts the \code{Plot_Name} field. It is meant as a convenience function.
#'
#' @export

setGeneric(name = "getPlotNames", function(object, type = "active", visits = NA, years = NA, subparks = NA, output = "vector", ...) {
  standardGeneric("getPlotNames")
}, signature = "object")


setMethod(
  f = "getPlotNames", signature = c(object = "list"),
  function(object, type = "active", visits = NA, years = NA, subparks = NA, output = "vector", ...) {
    OutPlots <- lapply(X = object, FUN = getPlotNames, type = type, visits = visits, years = years, subparks = subparks, ...)
    switch(output,
      list = {
        names(OutPlots) <- getNames(object, name.class = "code")
        return(OutPlots)
      },
      vector = return(unlist(OutPlots))
    )
  }
)



setMethod(
  f = "getPlotNames", signature = c(object = "NPSForVeg"),
  function(object, type = "active", visits = NA, years = NA, subparks = NA, ...) {
    return(getPlots(object = object, type = type, visits = visits, years = years, subparks = subparks, ...)$Plot_Name)
  }
)
