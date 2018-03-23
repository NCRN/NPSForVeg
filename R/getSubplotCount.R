#' @title getSubplotCount
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct

#' @description This function gets the the counts of quadrats or microplots sampled in the "Events" slot of an NPSForVeg object.
#' 
#' @param object  Either an NPSForVeg object of a list of such objects.
#' @param group Indicates which plant groups the area should correspond to. Acceptable values are "herbs", and "seedlings"
#' @param subtype Indicates the type of area to be returned, and should be enclosed in quotes. Acceptable values are 
#'\describe{
#'  \item{"all"}{(the default) Returns the total area sampled per plot event for that group.}
#'  \item{"single"}{Returns the area of one of the subsamples (eg. microplot or quadrat).}
#'  \item{"count"}{A count of the number of the number of subplots/microplots/quadrats that the group is monitored in for each plot event.}
#'}
#' 
#' @return For a single NPSForVeg object getSubplotCount returns a vector of length one. For a list of such object it returns a vector as long as the list. 
#' 
#' @export

### This calculates the areas for various kinds of plots in m^2

setGeneric(name="getSubplotCount",function(object,group,years=NA,plots=NA,subtype=NA,output="list",...){standardGeneric("getSubplotCount")},signature=c("object") )

setMethod(f="getSubplotCount", signature="list",
  function(object,group,years,plots,subtype,...) 
  {OutSubplots<-lapply(X=object, FUN=getSubplotCount,group=group,subtype=subtype,years=years,plots=plots)
  switch(output,
    list=return(OutSubplots),
    vector=return(unlist(OutSubplots)) #this isn't working right for me yet.
    )
  })

setMethod(f="getSubplotCount", signature="NPSForVeg",
  function(object,group,years,plots,subtype,...){

    XSubplots<-getEvents(object, plots, years)

    switch(group,
      seedlings = switch (subtype,
        all= return(object@SeedPlotSize[2]*XSubplots$nummicro),
        single= return(object@SeedPlotSize[2]),
        count= return(XSubplots$nummicro) 
      ),

      herbs = switch (subtype,
        all= return(object@HPlotSize[2]*XSubplots$numquads),
        single= return(object@HPlotSize[2]),
        count= return(XSubplots$numquads)
      )
    )
    if(!sum(is.na((plots))>0) &!sum(is.na(years))==0) XSubplots<-(XSubplots[XSubplots$Plot_Name %in% plots,])
    if(!sum(is.na((years))>0) &!sum(is.na(plots))==0) XSubplots<-(XSubplots[XSubplots$Event_Year %in% years,])
     })

