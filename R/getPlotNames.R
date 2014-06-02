#' @title getPlotNames
#' 
#' @description Returns the names of plots from an NPSForVeg object that meet various criteria. 
#' 
#' @param object Either an NPSForVeg object or a list of such objects
#' @param type One of three options that indicate what type of plots are to be considered. Must be in quotes. Options are:
#' \describe{
#' \item{"active"}{The default. Only returns names of plots which are listed as active in the \code{Plots$Location_Status} field.} 
#' \item{"all"}{The default. Returns names from all types of plots.}
#' \item{"retired}{Only returns names from plots which are listed as retired in the \code{Plots$Location_Status} field.}
#' }
#' @param visits A numeric vector. Returns only plot names where the number of plot visits matches one of the values in \code{visits} The number of visits to a plot is determined by the \code{Event_Count} column in the \code{Events} slot. 
#' @param years A numeric vector. Returns only plot names where the yeare the plot was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Event_Year} column in the \code{Events} slot. 
#' @param subparks A character vector. Returns only plot names where the subpark the plot is in matches one of the values in \code{subparks} The subpark a plot is locaed in is determined by the \code{SubUnit_Code} column in the \code{Plots} slot. 
#'  @param output Either "vector" (the default) or "list". Note that this must be in qutoes. Determines the type of output from the function
#'  
#'  @details This function returns plot names either from a single NPSForVeg object or a list of such objects. One use for this function is to assemble a vector of plot names that meet some criteria (location, when they were monitored, etc) that can then be passed on to the \code{plots} arguement of a second fucntion such as \code{getPlants}, \code{IV}, \code{dens} etc.  The default output is a charcacter vector. However, if \code{object} is a list and \code{output} is "list" then a list of character vectors will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NPSForVeg object. 
#'  
#'  @export

setGeneric(name="getPlotNames",function(object,...,type = "active",visits=NA, years=NA, subparks=NA, output="vector"){standardGeneric("getPlotNames")},signature="object")


setMethod(f="getPlotNames", signature=c(object="list"),
          function(object,...) {
              MC<-match.call()
              OutPlots<-vector(mode="list",length=length(object))
              for (i in seq_along(object)) {
                MC[2]<-object[[i]]
                OutPlots[[i]]<-eval(MC)
              }
              switch(output,
                     list={names(OutPlots)<-getNames(object,name.class="code")
                       return(OutPlots)},
                     vector=return(unlist(OutPlots) ) )
            })



setMethod(f="getPlotNames", signature=c(object="NPSForVeg"),
  function(object,...){
  switch(type,
     all = XPlots<-object@Plots,
     active= XPlots<-object@Plots[object@Plots$Location_Status=="Active",], 
     retired= XPlots<-object@Plots[object@Plots$Location_Status=="Retired",],
     stop("getPlotNames type not recognized")
    )
  if(all(!is.na(visits))) XPlots<-XPlots[XPlots$Event_Count %in% visits,]  
  
  if(all(!is.na(years))) XPlots<-XPlots[XPlots$Plot_Name %in% object@Events$Plot_Name[object@Events$Event_Year %in% years],]
  
  if(all(!is.na(subparks))) XPlots<-XPlots[XPlots$Subunit_Code %in% subparks,]
  
  return(XPlots$Plot_Name)
  
  }
)




