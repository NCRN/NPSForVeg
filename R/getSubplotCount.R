#' @title getSubplotCount
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate rename select

#' @description This function gets the the counts and/or areas of quadrats or microplots sampled in the "Events" slot of an NPSForVeg object.
#' 
#' @param object  Either an NPSForVeg object of a list of such objects.
#' @param group Indicates which plant groups the area should correspond to. Acceptable values are "trees","saplings","seedlings", and "herbs"
#' @param subtype Indicates the type of area to be returned, and should be enclosed in quotes. Acceptable values are 
#'\describe{
#'  \item{"all"}{(the default) Returns the total area and number of subplots sampled per plot event for that group.}
#'  \item{"area"}{ Returns the total area sampled per plot event for that group.}
#'  \item{"count"}{A count of the number of subplots/microplots/quadrats that the group is monitored in for each plot event.}
#'}
#' 
#' @param ... Additional arguments passed on to \code{getEvents}. 
#' @return A \code{data.frame} with the columns corresponding to the plot name, event year,number of subplots sampled, and area of the 
#' sampled subplots.
#' 
#' @export

### This calculates the areas for various kinds of plots in m^2

setGeneric(name="getSubplotCount",function(object,group,subtype='all',...){standardGeneric("getSubplotCount")},signature=c("object") )

setMethod(f="getSubplotCount", signature="list",
  function(object,group,subtype,...) 
  {OutSubplots<-lapply(X=object, FUN=getSubplotCount,group=group,subtype=subtype, ...)
  return(rbindlist(OutSubplots)) 
  
  })

setMethod(f="getSubplotCount", signature="NPSForVeg",
  function(object,group,subtype,...){
    
    
    XSubplots<-getEvents(object=object,...)


    XSubplots<-switch(group,
      trees = switch (subtype,
        area=XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=getArea(object, group, type="all")),
        count=XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, type="count")),
      all=XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, type="count"),
          SubPlotArea=getArea(object, group, type="all"))),

      saplings = switch (subtype,
        area= if("numSapPlots" %in% names(XSubplots)) XSubplots %>% mutate(SubPlotArea=numSapPlots * getArea(object, group,type="single")) %>%
              dplyr::select(Plot_Name, Event_Year, SubPlotArea) else XSubplots %>%
              dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=getArea(object,group, type="all")),

        count= if ("numSapPlots" %in% names(XSubplots)) XSubplots %>% rename(numSubPlots=numSapPlots) %>% 
              dplyr::select(Plot_Name,Event_Year,numSubPlots) else
              XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, type="count")),

        all= if("numSapPlots" %in% names(XSubplots)) XSubplots %>% 
          mutate(numSubPlots=numSapPlots,SubPlotArea=numSapPlots*getArea(object, group, type="single")) %>%
          dplyr::select(Plot_Name,Event_Year,numSubPlots, SubPlotArea) else
          XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, "count"),
            SubPlotArea=getArea(object, group, "all"))
      ),
      seedlings = switch (subtype,
        area= if("numSeedPlots" %in% names(XSubplots)) XSubplots %>% mutate(SubPlotArea=numSeedPlots * getArea(object, group,type="single")) %>%
          dplyr::select(Plot_Name, Event_Year, SubPlotArea) else XSubplots %>%
          dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=getArea(object,group, type="all")),
                         
        count= if ("numSeedPlots" %in% names(XSubplots)) XSubplots %>% rename(numSubPlots=numSeedPlots) %>% 
          dplyr::select(Plot_Name,Event_Year,numSubPlots) else
          XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, type="count")),
                         
        all= if("numSeedPlots" %in% names(XSubplots)) XSubplots %>% 
          mutate(numSubPlots=numSeedPlots,SubPlotArea=numSeedPlots*getArea(object, group, type="single")) %>%
          dplyr::select(Plot_Name,Event_Year,numSubPlots, SubPlotArea) else
          XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, "count"),
              SubPlotArea=getArea(object, group, "all"))
      ),
      herbs = switch (subtype,
        area= if("numHerbPlots" %in% names(XSubplots)) XSubplots %>% mutate(SubPlotArea=numHerbPlots * getArea(object, group,type="single")) %>%
          dplyr::select(Plot_Name, Event_Year, SubPlotArea) else XSubplots %>%
          dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=getArea(object,group, type="all")),
                         
        count= if ("numHerbPlots" %in% names(XSubplots)) XSubplots %>% rename(numSubPlots=numHerbPlots) %>% 
          dplyr::select(Plot_Name,Event_Year,numSubPlots) else
          XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, type="count")),
                         
        all= if("numHerbPlots" %in% names(XSubplots)) XSubplots %>% 
          mutate(numSubPlots=numHerbPlots,SubPlotArea=numHerbPlots*getArea(object, group, type="single")) %>%
          dplyr::select(Plot_Name,Event_Year,numSubPlots, SubPlotArea) else
          XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=getArea(object, group, "count"),
            SubPlotArea=getArea(object, group, "all"))
      )
    )

    return(XSubplots)
  })

