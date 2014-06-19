#' @title getPlots
#' 
#' @description Returns the contents of the \code{Plots} slot of an NPSForVeg object. The retunred data can be filtered to meet various criteria.
#' 
#' @param object An NPSForVeg object or a list of such objects.
#' @param type One of three options that indicate what type of plots are to be considered. Must be in quotes. Options are:
#' \describe{
#' \item{"active"}{The default. Only returns data for plots which are listed as active in the /code{Plots$Location_Status} field.} 
#' \item{"all"}{The default. retruns data from all types of plots.}
#' \item{"retired"}{Only returns data from plots which are listed as retired in the \code{Plots$Location_Status} field. }
#' }
#' @param visits A numeric vector. Returns only data from plots where the number of plot visits matches one of the values in \code{visits} The number of visits to a plot is determined by the \code{Event_Count} column in the \code{Events} slot. 
#' @param years A numeric vector. Returns only plot data from plots where the yeare the plot was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Event_Year} column in the \code{Events} slot. 
#' @param subparks A character vector. Returns only data from plots where the subpark the plot is in matches one of the values in \code{subparks} The subpark a plot is locaed in is determined by the \code{SubUnit_Code} column in the \code{Plots} slot. 
#'  @param output Either "dataframe" (the default) or "list". Note that this must be in qutoes. Determines the type of output from the function.
#' 
#'  @details This function returns plot data either from a single NPSForVeg object or a list of such objects. The default output is a data.frame. However, if \code{object} is a list and \code{output} is "list" then a list of data.frames will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NPSForVeg object. 
#'  
#'  @export


setGeneric(name="getPlots",function(object,type = "active",visits=NA, years=NA, plots=NA, subparks=NA, output="dataframe"){standardGeneric("getPlots")}, signature="object")


setMethod(f="getPlots", signature=c(object="list"),
          function(object,type,visits,years,plots,subparks,output) {
#             MC<-match.call()
#              OutPlots<-vector(mode="list",length=length(object))
#             for (i in seq_along(object)) {
#                MC[2]<-object[[i]]
#                OutPlots[[i]]<-eval(MC)
#              }
#              switch(output,
#                     list={names(OutPlots)<-getNames(object,name.class="code")
#                       return(OutPlots)},
#                     dataframe=return( do.call("rbind",OutPlots) )
#              )
#            })
            OutPlots<-lapply(X=object, FUN=getPlots, type=type, visits=visits, years=years, plots=plots, subparks=subparks)
            switch(output,
              list={names(OutPlots)<-getNames(object,name.class="code")
                    return(OutPlots)},
                dataframe=return(do.call("rbind",OutPlots))
            )
})


setMethod(f="getPlots", signature=c(object="NPSForVeg"),
  function(object,type,visits,years,plots,subparks,output){
  switch(type,
     all = XPlots<-object@Plots,
     active= XPlots<-object@Plots[object@Plots$Location_Status=="Active",], 
     retired= XPlots<-object@Plots[object@Plots$Location_Status=="Retired",],
     stop("getPlots type not recognized")
    )
  if(all(!is.na(visits))) XPlots<-XPlots[XPlots$Event_Count %in% visits,]
  
  if(all(!is.na(years))) XPlots<-XPlots[XPlots$Plot_Name %in% object@Events$Plot_Name[object@Events$Event_Year %in% years],]
  
  if(all(!is.na(plots))) XPlots<-XPlots[XPlots$Plot_Name %in% plots,]
  
  if(all(!is.na(subparks))) XPlots<-XPlots[XPlots$Subunit_Code %in% subparks,]
  
  return(XPlots)
  
  }
)




