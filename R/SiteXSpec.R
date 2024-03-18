#' @title SiteXSpec
#'
#' @import  data.table
#'
#' @description Produces a Site X Species matrix. Each cell can contain a count, a measure of size or 1/0 indicating presence/abscence
#'
#' @param object either an object of class \code{NPSForVeg} or a list of such objects
#' @param group  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings",
#' "seedlings", "shrubs", "shseedlings" (indicating shrub seedlings), "vines", "herbs" or "cwd".
#' @param years Defaults to \code{NA}. A numeric vector indicating which years should be included. This is passed on to \code{\link{getPlants}}.
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is passed on to \code{\link{getPlants}}.
#' @param values Determines the data contained in the Site X Species matrix. Possible values are:
#' \describe{
#' \item{"count"}{The default. Each cell will include a count of the number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants. For vines, it is the number of trees a vine species grows on. For herbs it will be the number of quadrats the plant occurs in.}
#' \item{"size"}{For trees and saplings this is the total basal area in m2 per ha. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the average percent cover across all sampled quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
#' \item{"presab"}{Produces a presence/absence matrix. When a plant species is present in a given plot the corresponding cell value will 1, otherwise it is 0.}
#' }
#' @param status  A requried character string indicating if user wants data from living or dead plants. Used only for trees, saplings and shrubs. Values of this argument are matched to the \code{Status} field in the \code{Tree}, \code{Saplings} or \code{Shrubs} slot.  Acceptable options are:
#' \describe{
#' \item{"alive"}{The default. Includes any plant with a status of "Alive", "Alive Standing", "Alive Broken", "Alive Leaning" ,"Alive Fallen","AB","AF","AL","AM","AS","RB","RF","RL","RS" or "TR"}
#' \item{"snag"}{Standing dead tree only, Includes any plant with a status of "Dead", "Dead - Human Action", "Dead Leaning", "Dead Missing", "Dead Standing", "Dead - Too Small","DB","DL","DM",or"DS" }
#' \item{"other"}{Includes any plant with a status of "Missing", "Missing - Presumed Dead", "Missing - Uncertain" , "Downgraded to Non-Sampled","ES","EX","NL","XO", "XP" or"XS" }
#' \item{"all"}{Includes all plants}
#' }
#' @param area A character vector. Indicates if the values in the output are on a per plot or per area basis. This only works with counts,
#' basal areas, and length of seedlings.
#' \describe{
#' \item{"plot"}{The default. Each cell will be on a per plot basis. The only option for \code{values="presab"}.}
#' \item{"ha"}{Values will be on a per hectare basis}
#' \item{"ac"}{values will be on a per acre basis}
#' }
#' @param output Either "dataframe" or "list". Determines the output type when \code{object} is a list. "dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be a single large \code{data.frame}, containing all sites and species from all \code{NPSForVeg} objects. "list" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot.
#' @param species A character vector of names. Defaults to \code{NA}. When not \code{NA} only species included in \code{species} will be included in the matrix. If a name is in \code{species}, but is not present in the data, it will not appear in the output.
#' @param plots A character vector of plot names. Defaults to \code{NA}. When not \code{NA} only plots included in \code{plots} will be included in the matrix. If a plot name is in \code{plots}, but is not present in the data, it will not appear in the output.
#' @param plot.type Passed on to \code{\link{getPlotNames}} and used to filter events so that only events from certain plot types are returned.
#' One of three options are availbe (always ineclosed in quotes).
#' \describe{
#' \item{"all"}{Events from all types of plots are returned.}
#' \item{"active"}{The default. Only returns events from plots which are listed as active in the Plots$Location_Status field.}
#' \item{"retired}{Only returns events from plots which are listed as retired in the Plots$Location_Status field. }
#' }
#' @param Total Logical value. Determine if a "Total" column will be included in the output. Defaults to TRUE.
#' @param common Defaults to \code{FALSE}. Indicates if common names should be used rather than Latin names.
#' @param ... Other arguments passed on to \code{\link{getPlants}}
#'
#' @details This function will first call \code{\link{getPlants}} to retrieve the plant data and \code{\link{getEvents}} to retrieve the event data. Then a site X species matrix will be created.
#' Values in the cells are determined by the option selected with \code{values}. Each row corresponds to a different plot and each column
#' to a different species.
#'
#' Note that \code{species} can be a vector of common or Latin names. If common names are used then \code{common=TRUE} must be included in
#' the function call.
#'
#' @export
#'



setGeneric(name = "SiteXSpec", function(object, group, years = NA, cycles = NA, values = "count", status = "alive", area = "plot", output = "dataframe",
                                        species = NA, plots = NA, plot.type = "active", Total = TRUE, common = F, ...) {
  standardGeneric("SiteXSpec")
}, signature = "object")

setMethod(f = "SiteXSpec", signature = c(object = "list"), function(object, ...) {
  species <- if (anyNA(species)) {
    unique(getPlants(
      object = object, group = group, status = status, years = years, cycles = cycles, species = species,
      plots = plots, common = F, output = "dataframe", ...
    )$Latin_Name)
  } else {
    species
  }
  OutList <- lapply(
    X = object, FUN = SiteXSpec, group = group, status = status, years = years, cycles = cycles, values = values, area = area, species = species,
    plots = plots, plot.type = plot.type, Total = Total, common = common, ...
  )

  switch(output,
    dataframe = {
      OutDF <- rbindlist(OutList, use.names = TRUE, fill = TRUE)
      OutDF[is.na(OutDF)] <- 0
      OutDF <- as.data.frame(OutDF)
      Colnames <- c("Plot_Name", sort(names(OutDF)[!names(OutDF) %in% c("Plot_Name", "Total")]))
      Colnames <- if (Total) c(Colnames, "Total") else Colnames
      return(OutDF[Colnames])
    },
    list = {
      names(OutList) <- getNames(object, name.class = "code")
      return(OutList)
    }
  )
})

setMethod(
  f = "SiteXSpec", signature = c(object = "NPSForVeg"),
  function(object, group, years, cycles, values, status, species, plots, plot.type, common, ...) {
    XPlants <- data.table(getPlants(
      object = object, group = group, status = status, years = years, cycles = cycles, species = species, plots = plots,
      common = common, ...
    ))
    XPlots <- if (anyNA(plots)) {
      getPlotNames(object = object, years = years, cycles = cycles, type = "all")
    } else {
      plots[plots %in% getPlotNames(object = object, years = years, cycles = cycles, type = "all")]
    }
    XSubplots <- getSubplotCount(object = object, group = group, years = years, cycles = cycles, plots = plots, subtype = "all", plot.type = plot.type)
    XPlants <- merge(XPlants, XSubplots, by.x = c("Plot_Name", "Sample_Year"), by.y = c("Plot_Name", "Event_Year"))
    XPlants[, fPlot := factor(Plot_Name, levels = XPlots)]
    XSpecies <-#if (anyNA(species)) 
      unique(XPlants$Latin_Name) 
    #else    getPlantNames(object, names = species, in.style = "Latin", out.style = ifelse(common, "common", "Latin"))
    switch(values,
      count = OutData <- dcast.data.table(setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies), .N,
        by = .EACHI
      ], formula = fPlot ~ Latin_Name, value.var = "N"),
      size = {
        switch(group,
          trees = OutData <- dcast.data.table(
            setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies),
              switch(status,
                alive = sum(SumLiveBasalArea_cm2, na.rm = TRUE) / 10000,
                snag = sum(SumDeadBasalArea_cm2, na.rm = TRUE) / 10000,
                all = sum(SumLiveBasalArea_cm2 + SumDeadBasalArea_cm2, na.rm = TRUE) / 10000
              ),
              by = .EACHI
            ],
            formula = fPlot ~ Latin_Name, value.var = "V1", drop = FALSE
          ), # units are m2/ha
          saplings = OutData <- dcast.data.table(
            setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies),
              sum(SumLiveBasalArea_cm2, na.rm = TRUE) / 10000,
              by = .EACHI
            ],
            formula = fPlot ~ Latin_Name, value.var = "V1", drop = FALSE
          ), # units are m2/ha
          seedlings = ,
          shseedlings = OutData <- dcast.data.table(setkey(XPlants, fPlot, Latin_Name)[CJ(
            unique(levels(fPlot)),
            XSpecies
          ), sum(Height, na.rm = TRUE), by = .EACHI], formula = fPlot ~ Latin_Name, value.var = "V1", drop = FALSE),
          herbs = OutData <- dcast.data.table(
            setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies),
              sum(Percent_Cover, na.rm = TRUE) / (unique(numSubPlots)),
              by = .EACHI
            ],
            formula = fPlot ~ Latin_Name, value.var = "V1", drop = FALSE
          ),
          cwd = OutData <- dcast.data.table(
            setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies),
              sum(CWD_Vol, na.rm = TRUE),
              by = .EACHI
            ],
            formula = fPlot ~ Latin_Name, value.var = "V1", drop = FALSE
          ), # units are m3/ha
          shrubs = ,
          vines = stop("Cannot do a size based site x species matrix - no size measurement avaialable")
        )
      },
      presab = {
        OutData <- dcast.data.table(setkey(XPlants, fPlot, Latin_Name)[CJ(unique(levels(fPlot)), XSpecies), .N, by = .EACHI],
          formula = fPlot ~ Latin_Name, value.var = "N"
        )
        for (xx in seq_along(OutData)) {
          set(OutData, i = which(is.numeric(OutData[[xx]]) & as.numeric(OutData[[xx]]) > 0), j = xx, value = 1)
        }
      },
      stop("values type not recognized")
    )

    for (i in seq_along(OutData)) {
      set(OutData, i = which(is.na(OutData[[i]])), j = i, value = 0)
    } # removes NA's

    setnames(OutData, "fPlot", "Plot_Name")
    OutData[, Plot_Name := as.character(Plot_Name)]

    if (Total) {
      OutData[, Total := rowSums(.SD), .SDcol = -1]
    }


    OutData <- (as.data.frame(OutData))
    PlotSize <- XSubplots[order(XSubplots$Plot_Name), ]$SubPlotArea

    if ((group %in% c("trees", "saplings", "seedlings", "shrubs", "shseedlings", "vines")) & values == "count") {
      switch(area,
        ha = OutData[-1] <- OutData[-1] * (10000 / PlotSize),
        ac = OutData[-1] <- OutData[-1] * (4046.86 / PlotSize) # conversion from square meters to acres
      )
    }

    if ((group %in% c("trees", "saplings", "seedlings", "shseeldlings")) & values == "size") {
      switch(area,
        ha = OutData[-1] <- OutData[-1] * (10000 / PlotSize),
        ac = OutData[-1] <- OutData[-1] * (4046.86 / PlotSize) # conversion from square meters to acres
      )
    }

    return(OutData)
  }
)
