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
#' @return For a single NPSForVeg object getSubplotCount returns a data.frame. 
#' 
#' @export

### This calculates the areas for various kinds of plots in m^2

setGeneric(name="getSubplotCount",function(object,group,years=NA,plots=NA,subtype=NA,...){standardGeneric("getSubplotCount")},signature=c("object") )

setMethod(f="getSubplotCount", signature="list",
  function(object,group,years,plots,subtype,...) 
  {OutSubplots<-lapply(X=object, FUN=getSubplotCount,group=group,subtype=subtype,years=years,plots=plots)
    return(do.call("rbind",OutSubplots)) 
    
  })

setMethod(f="getSubplotCount", signature="NPSForVeg",
  function(object,group,years,plots,subtype,...){

    XSubplots<-object@Events
    if(!sum(is.na((years))>0) & !sum(is.na(plots))>0) {XSubplots<-XSubplots %>% filter(Plot_Name %in% plots , Event_Year %in% years)}
    if(!sum(is.na((plots))>0) & !sum(is.na(years))==0) {XSubplots<-XSubplots %>% filter(Plot_Name %in% plots)}
    if(!sum(is.na((years))>0) & !sum(is.na(plots))==0) {XSubplots<-XSubplots %>% filter(Event_Year %in% years)}
    

    switch(group,
      saplings = switch (subtype,
        all= XSubplots<-ifelse("numSapPlots" %in% names(XSubplots), return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year,numSapPlots) %>%     
                        mutate(SapPlotAll=numSapPlots*object@SapPlotSize[2]) %>% select(-numSapPlots)), 
                        return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(SapPlotAll=object@SapPlotSize[1]*object@SapPlotSize[2]))),
        single= return(object@SapPlotSize[2]),
        count= XSubplots<-ifelse("numSapPlots" %in% names(XSubplots), return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year,numSapPlots)),
                        return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(numSapPlots=object@SapPlotSize[1])))),
      
      seedlings = switch (subtype,
        all= XSubplots<-ifelse("numSeedPlots" %in% names(XSubplots), return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year,numSeedPlots) %>%     
                        mutate(SeedPlotAll=numSeedPlots*object@SeedPlotSize[2]) %>% select(-numSeedPlots)), 
                        return(XSubplots %>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(SeedPlotAll=object@SeedPlotSize[1]*object@SeedPlotSize[2]))),
        single= return(object@SeedPlotSize[2]),
        count= XSubplots<-ifelse("numSeedPlots" %in% names(XSubplots), return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year,numSeedPlots)),
          return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(numSeedPlots=object@SeedPlotSize[1])))),
      
      herbs = switch (subtype,
        all= XSubplots<-ifelse("numHerbPlots" %in% names(XSubplots), return(XSubplots%>% select(Event_ID,Plot_Name,Event_Year,numHerbPlots) %>%     
                        mutate(HerbPlotAll=numHerbPlots*object@HPlotSize[2]) %>%  select(-numHerbPlots)), 
                        return(XSubplots %>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(HerbPlotAll=object@HPlotSize[1]*object@HPlotSize[2]))),
        single= return(object@HerbPlotSize[2]),
        count= XSubplots<-ifelse("numHerbPlots" %in% names(XSubplots), return(object@Events%>% select(Event_ID,Plot_Name,Event_Year,numHerbPlots)),
                        return(XSubplots %>% select(Event_ID,Plot_Name,Event_Year) %>% mutate(numHerbPlots=object@HPlotSize[1])))))
        
    return(XSubplots)
     })

