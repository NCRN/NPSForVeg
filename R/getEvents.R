#' @title getEvents
#'
#' @importFrom  dplyr bind_rows filter
#' @importFrom  purrr map
#'
#' @description This function gets the contents of the "Events" slot of an NPSForVeg object.
#'
#' @param object  Either an NPSForVeg object of a list of such objects.
#' @param plots   A character vector containing one or more plot names. Defaults to NA. If not NA then only events from the given plots
#' will be returned.
#' @param years   A numeric vector containing one or more years. Defaults to NA. If not NA then only events from the given years will be returned.
#' @param cycles   A numeric vector containing one or more sampling cycles. Defaults to NA. If not NA then only events from the given sampling cycles will be returned.
#' @param plot.type Only used when \code{object} is a \code{data.frame} or a \code{list}. Passed on to \code{\link{getPlotNames}} and used to filter events so that only events from certain plot types are returned.
#' One of three options are available (always in quotes).
#' \describe{
#' \item{"all"}{The default. Events from all types of plots are returned.}
#' \item{"active"}{Only returns events from plots which are listed as active in the Plots$Location_Status field.}
#' \item{"retired"}{Only returns events from plots which are listed as retired in the Plots$Location_Status field. }
#' }
#' @param output  Used type when \code{object} is a list to determine if the output is a data.frame or a list. Can be either
#'  "dataframe" (the default), or "list". Needs to be in quotes.
#'
#' @param ... Other arguments that are passed on to \code{getEvents}.
#'
#' @return The contents of an "Events" slot from one or more NPSForVeg objects either as a data.frame or a list. The output can be filtered by
#'  using the "plots", "years" or "plot.type" arguments.
#'
#' @details This function returns the \code{Events} slot of an NPSForVeg object or a list of such objects. This can be filtered by
#' specifying \code{plots}, \code{years}, or \code{plot.type}.  If \code{object} is a single NPSForVeg object than a data.frame is returned.
#'  If \code{object} is a list and \code{output} is "dataframe" then a single data.frame with data from all NPSForVeg objects in the list
#'  is returned. If \code{object} is a list and \code{output} is "list" then a list will be returned. Each element of the list will be a
#'  \code{data.frame} with the \code{Events} slot of one of the \code{NPSForVeg} objects.
#'
#' @examples
#' \dontrun{
#'
#' netn <- importNETN("C:/Data/")
#' ACAD_latest <- getEvents(netn, years = 2021:2024, parks = 'ACAD') |>
#'   dplyr::filter(!(Panel == 3 & Event_Year == 2021))
#'
#' ncrn <- importNCRN("C:/Data/")
#' prwi_active <- getEvents(ncrn, parks = "PRWI", plot.type = 'active')
#'
#' }
#'
#' @export

setGeneric(name = "getEvents", function(object, plots = NA, years = NA, cycles = NA, plot.type = "all", output = "dataframe", ...) {
  standardGeneric("getEvents")
}, signature = c("object"))

setMethod(f = "getEvents", signature = "list", function(object, plots, years, cycles, plot.type, output, ...) {

  XEvents <- map(object, ~ `@`(.x, Events))

  # update the plots argument to match the requirements from plot.type
  if(!plot.type=="all") {
    Plot_Vec<-getPlotNames(object,years=years, cycles=cycles, plots=plots, type=plot.type, ...)

    if(!anyNA(plots)) {plots<-intersect(Plot_Vec, plots)} else(plots<-Plot_Vec)
  }

  OutEvents <- switch(output,
    list = map(XEvents, ~ getEvents(.x, plots, years, cycles, plot.type)),
    dataframe = getEvents(object = bind_rows(XEvents), plots, years, cycles, plot.type)
  )


  return(OutEvents)
})


setMethod(
  f = "getEvents", signature = "NPSForVeg",
  function(object, plots, years, cycles, plot.type) {
    XEvents <- object@Events

    # update the plots argument to match the requirements from plot.type
    if(!plot.type=="all") {
      Plot_Vec<-getPlotNames(object,years=years, cycles=cycles, plots=plots, type=plot.type)

      if(!anyNA(plots)) {plots<-intersect(Plot_Vec, plots)} else(plots<-Plot_Vec)
    }

    OutEvents <- getEvents(XEvents, plots = plots, years = years, cycles=cycles, plot.type = plot.type)

    return(OutEvents)
  }
)

setMethod(
  f = "getEvents", signature = "data.frame",
  function(object, plots, years, cycles) {
    if (!anyNA(plots)) object <- filter(object, Plot_Name %in% plots)

    if (!anyNA(years)) object <- filter(object, Event_Year %in% years)

    if (!anyNA(cycles)) object <- filter(object, Cycle %in% cycles)

    return(object)
  }
)
