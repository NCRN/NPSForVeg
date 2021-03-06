% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importCSV.R
\name{importCSV}
\alias{importCSV}
\title{importCSV}
\usage{
importCSV(Dir)
}
\arguments{
\item{Dir}{The directory where the data is found. Path should not have a trailing slash.}
}
\value{
Returns a \code{list} of \code{NPSForVeg} objects, each object is a park listed in the MetaData.csv file, named using the park code from that file.
}
\description{
This function imports data from the standard .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: MetaData, 
Plots, Events and CommonNames. The optional .csv files are: Trees, Saplings, Seedlings, Shrubs, Shrub_Seedlings, Vines, Herbs, and CWD.
}
