#' @title getEvents
#' 
#' @description This function gets the contents of the "Events" slot of an NPSForVeg object.
#' 
#' @param object  Either an NPSForVeg object of a list of such objects.
#' @param plots   A character vector containing one or more plot names. Defaults to NA. If not NA then only events from the given plots will be returned. 
#' @param years   A numeric vector containing one or more years. Defaults to NA. If not NA then only events from the given years will be returned.
#' @param plot.type Passed on to \code{\link{getPlotNames}} and used to filter events so that only events from certain plot types are returned. One of three options are availbe (always ineclosed in quotes). 
#' \describe{
#' \item{"all"}{The default. Events from all types of plots are returned.}
#' \item{"active"}{Only returns events from plots which are listed as active in the Plots$Location_Status field.} 
#' \item{"retired}{Only returns events from plots which are listed as retired in the Plots$Location_Status field. }
#' }
#' @param output  Used type when \code{object} is a list to determine if the output is a data.frame or a list. Can be either "dataframe" (the default), or "list". Needs to be in quotes.
#' 
#' @return The contents of an "Events" slot from one or more NPSForVeg objects either as a data.frame or a list. The output can be filtered by using the "plots", "years" or "plot.type" arguements.
#' 
#' @details This function returns the \code{Events} slot of an NPSForVeg object or a list of such objects. This can be filtered by specifying \code{plots}, \code{years}, or \code{plot.type}. Currently, however, you cannot specify both \code{years} and \code{plots}. If \code{object} is a single NPSForVeg object than a data.frame is returned. If \code{object} is a list and \code{output} is "dataframe" then a single data.frame with data from all NPSForVeg objects in the list is returned. If \code{object} is a list and \code{ouput} is "list" then a list will be returned. Each element of the list will be a data.frame with the "Events" slot of one of the NPSForVeg objects.
#' 
#' @export

setGeneric(name="getEvents",function(object,plots=NA,years=NA,plot.type="all",output="dataframe"){standardGeneric("getEvents")},signature=c("object") )

setMethod(f="getEvents", signature="list",
          function(object,plots,years,plot.type,output) 
          {OutEvents<-lapply(object,getEvents,plots,years)
          switch(output,
            list=return(OutEvents),
            dataframe=return(do.call("rbind",OutEvents))
          )
  })


setMethod(f="getEvents", signature="NPSForVeg",
          function(object,plots,years,plot.type){

            if(sum(!is.na(plots))>0 & sum(!is.na(years))>0) {stop("You must specify either plot or years but not both")}

            if(sum(is.na(plots))>0 & sum(is.na(years))>0) {plots<-getPlotNames(object,type=plot.type)}
              
              
            if(sum(!is.na(plots))>0 & sum(!is.na(years))==0) {   #There are plots but not years 
            return(object@Events[object@Events$Plot_Name %in% plots,])}
            
            if(sum(!is.na(plots))==0 & sum(!is.na(years))>0) {   #There are years but not plots
              return(object@Events[object@Events$Event_Year %in% years,])}
})