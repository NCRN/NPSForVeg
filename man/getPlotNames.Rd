% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPlotNames.R
\name{getPlotNames}
\alias{getPlotNames}
\title{getPlotNames}
\usage{
getPlotNames(
  object,
  type = "active",
  visits = NA,
  years = NA,
  subparks = NA,
  output = "vector",
  ...
)
}
\arguments{
\item{object}{An \code{NPSForVeg} object or a \code{list} of such objects.}

\item{type}{One of three options indicating the type of plots to be considered. Must be in quotes. Options are:
\describe{
\item{"active"}{The default. Only returns data for plots which are listed as active in the \code{Plots$Location_Status} field.}
\item{"all"}{The default. retruns data from all types of plots.}
\item{"retired"}{Only returns data from plots which are listed as retired in the \code{Plots$Location_Status} field. }
}}

\item{visits}{A numeric vector. Returns only data from plots where the number of plot visits matches one of the values in \code{visits} The number of visits to a plot is determined by the \code{Event_Count} column in the \code{Events} slot.}

\item{years}{A numeric vector. Returns only plot data from plots where the years the plot was visited  matches one of the values in \code{years}. The year a visit takes place is determined by the \code{Event_Year} column in the \code{Events} slot.}

\item{subparks}{A character vector. Returns only data from plots where the subpark the plot is in matches one of the values in \code{subparks}. The subpark a plot is located in is determined by the \code{SubUnit_Code} column in the \code{Plots} slot.}

\item{output}{Either "vector" (the default) or "list". Note that this must be in qutoes. Determines the type of output from the function}

\item{...}{Other arguments that are passed on to \code{getPlots}.}
}
\description{
Returns the names of plots from an \code{NPSForVeg} object that meet various criteria.
}
\details{
This function is a wrapper for \code{getPlots()$Plot_Name}.  It first calls \code{\link{getPlots}} and then extracts the \code{Plot_Name} field. It is meant as a convenience function.
}
