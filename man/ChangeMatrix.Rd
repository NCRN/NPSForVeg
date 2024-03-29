% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ChangeMatrix.R
\name{ChangeMatrix}
\alias{ChangeMatrix}
\title{ChangeMatrix}
\usage{
ChangeMatrix(
  object,
  group,
  years1,
  years2,
  plots = NA,
  type = "active",
  species = NA,
  values = "count",
  output = "dataframe",
  ...
)
}
\arguments{
\item{object}{either an object of class \code{NPSForVeg} or a list of such objects}

\item{years1}{A numeric vector indicating the first set of years to be included.}

\item{years2}{A numeric vector indicating the second set of years to be included.}

\item{plots}{A character vector of plot names. Defaults to \code{NA}. When not \code{NA} only plots included in \code{plots} will be included in the matrix. If a plot name is in \code{plots}, but is not present in the data, it will not appear in the output.}

\item{type}{Passed to \code{\link{getPlots}}. One of three options that indicate what type of plots are to be considered. Must be in quotes. Options are:
\describe{
\item{"active"}{The default. Only returns names of plots which are listed as active in the \code{Plots$Location_Status} field.} 
\item{"all"}{The default. Returns names from all types of plots.}
\item{"retired}{Only returns names from plots which are listed as retired in the \code{Plots$Location_Status} field.}
}}

\item{species}{A character vector of names. Defaults to \code{NA}. When not \code{NA} only species included in \code{species} will be included in the matrix. If a name is in \code{species}, but is not present in the data, it will not appear in the output.}

\item{values}{Determines the data contained in the Site X Species matrix. Possible values are:
\describe{
\item{"count"}{The default. Each cell will include a count of the number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants. For vines, it is the number of trees a vine species grows on. For herbs it will be the number of quadrats the plant occurs in.}
\item{"size"}{For trees and saplings this is the total basal area. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the total percent cover across all quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
\item{"presab"}{Produces a presence/absence matrix. When a plant species is present in a given plot the corresponding cell value will 1, otherwise it is 0.}
}}

\item{output}{Either "dataframe" or "list". Determines the output type When \code{object} is a list. "dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be a single large \code{data.frame}, containing all sites and species from all \code{NPSForVeg} objects. "list" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot.}

\item{...}{Other arguments passed on to \code{\link{SiteXSpec}}}

\item{groups}{A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings","shrubs", "shseedlings" (indicates shrub seedlings), "vines" or "herbs'.}
}
\description{
Produces a matrix which indicates the change in some measure of plant abundance between two time periods
}
\details{
This function will first call \code{\link{SiteXSpec}} to create  site by species matrices for each of the two time periods. The output matrix will then be created by subtracting the values from time 1 from the values from time 2. While it is generally assumed that the two time periods do not overlap and that \code{years2} are later than \code{years1}, this is not enforced and the user can enter any sets of years they like.

Additional arguments accepted by \code{\link{SiteXSpec}} or \code{\link{getPlants}} can be used as well. Currently using the \code{cycles} argument will result in errors. 
Note that \code{species} can be a vector of common or Latin names. If common names are used then \code{common=TRUE} must be included in the function call.
}
