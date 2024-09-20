#' @title getPlots
#'
#' @importFrom dplyr filter semi_join
#' @importFrom purrr map map2
#'
#' @description Returns the contents of the \code{Plots} slot of an NPSForVeg object. The returned data can be filtered to meet various criteria.
#'
#' @param object An \code{NPSForVeg} object or a \code{list} of such objects.
#' @param type One of three options indicating the type of plots to be considered. Must be in quotes. Options are:
#' \describe{
#' \item{"active"}{The default. Only returns data for plots which are listed as active in the \code{Plots$Location_Status} field.}
#' \item{"all"}{Returns data from all types of plots.}
#' \item{"retired"}{Only returns data from plots which are listed as retired in the \code{Plots$Location_Status} field. }
#' }
#' @param visits A numeric vector. Returns only data from plots where the number of plot visits matches one of the values in \code{visits}. The number of visits to a plot is determined by the \code{Event_Count} column in the \code{Events} slot.
#' @param years A numeric vector. Returns only plot data from plots where the years the plot was visited  matches one of the values in \code{years}. The year a visit takes place is determined by the \code{Event_Year} column in the \code{Events} slot.
#' @param cycles A numeric vector. Returns only plot data from plots where the cycle the plot was visited  matches one of the values in \code{cycles}. The cycle a visit takes place is determined by the \code{Cycle} column in the \code{Events} slot.
#' @param parks A character vector. Returns only data from plots where the park the plot is in matches one of the values in \code{parks}. The park a plot is located in is determined by the \code{Unit_Code} column in the \code{Plots} slot.
#' @param subparks A character vector. Returns only data from plots where the sub-park the plot is in matches one of the values in \code{subparks}. The sub-park a plot is located in is determined by the \code{Subunit_Code} column in the \code{Plots} slot.
#' @param events A \code{data.frame} of events data like that produced by \code{getEvents}. Only used by the \code{data.frame} method when filtering by \code{year} or \code{cycle}. Typically this is automatically generated directly from the \code{NPSForVeg} objects.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function. Only used when the input \code{object} is a list.
#'
#' @details This function returns plot data either from a single NPSForVeg object or a list of such objects. The default output is a data.frame. However, if \code{object} is a list and \code{output} is "list" then a list of data.frames will be returned.
#'
#' @examples
#' \dontrun{
#' netn <- importNETN("C:/NETN/R_Dev/data/NPSForVeg/NETN")
#'
#' acad_mdiE <- getPlots(netn, subparks = "ACAD_MDI_East")
#'
#' SARA22 <- getPlots(netn, years = 2022, parks = "SARA")
#'}
#'
#' @export


setGeneric(name = "getPlots", function(object, type = "active", visits = NA, years = NA, cycles=NA,
                                       plots = NA, parks=NA, subparks = NA, events = NA, output = "dataframe") {
  standardGeneric("getPlots")
}, signature = "object")


setMethod(
  f = "getPlots", signature = c(object = "list"),
  function(object, type, visits, years, cycles, plots, parks, subparks, output) {
    XPlots <- map(object, ~ `@`(.x, Plots))

    events <- if (!anyNA(years) | !anyNA(cycles) ) {
      switch(output,
        list = map(object, ~ `@`(.x, Events)),
        dataframe = bind_rows(map(object, ~ `@`(.x, Events)))
      )
    } else NA

    OutPlots <- switch(output,
      list = map2(.x=XPlots, .y=events, .f= ~ getPlots(.x, type, visits, years, cycles, plots, parks, subparks, events=.y)),
      dataframe = getPlots(object = bind_rows(XPlots), type, visits, years, cycles, plots, parks, subparks, events)
    )
    return(OutPlots)
  }
)


setMethod(
  f = "getPlots", signature = c(object = "NPSForVeg"),
  function(object, type, visits, years, cycles, plots, parks,  subparks) {
    XPlots <- object@Plots

    if (!anyNA(years) |!anyNA(cycles)) {
      events <- object@Events
    }

    OutPlots <- getPlots(XPlots,
      type = type, visits = visits, years = years, cycles=cycles, plots = plots, events = events,
      parks=parks, subparks = subparks
    )

    return(OutPlots)
  }
)


setMethod(
  f = "getPlots", signature = c(object = "data.frame"),
  function(object, type, visits, years, cycles, plots, parks, subparks, events) {
    object <- switch(type,
      all = object,
      active = filter(object, Location_Status == "Active"),
      retired = filter(object, Location_Status == "Retired"),
      stop("getPlots type not recognized")
    )
    if (!anyNA(visits)) object <- filter(object, Event_Count %in% visits)

    if (!anyNA(plots)) object <- filter(object, Plot_Name %in% plots)

    if (!anyNA(parks)) object <- filter(object, Unit_Code %in% parks)

    if (!anyNA(subparks)) object <- filter(object, Subunit_Code %in% subparks)

    if (!anyNA(years)) object <- semi_join(object, filter(events, Event_Year %in% years), by = "Plot_Name")

    if (!anyNA(cycles)) object <- semi_join(object, filter(events, Cycle %in% cycles), by = "Plot_Name")

    return(object)
  }
)
