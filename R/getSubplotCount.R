#' @title getSubplotCount
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct

#' @description This function gets the the counts of quadrats or microplots sampled in the "Events" slot of an NPSForVeg object.
#' 
#' @param object  Either an NPSForVeg object of a list of such objects.
#' @param group Indicates which plant groups the area should correspond to. Acceptable values are "saplings","seedlings", and "herbs"
#' @param subtype Indicates the type of area to be returned, and should be enclosed in quotes. Acceptable values are 
#'\describe{
#'  \item{"all"}{(the default) Returns the total area and number of subplots sampled per plot event for that group.}
#'  \item{"area"}{(the default) Returns the total area sampled per plot event for that group.}
#'  \item{"count"}{A count of the number of subplots/microplots/quadrats that the group is monitored in for each plot event.}
#'}
#' 
#' @return For a single NPSForVeg object getSubplotCount returns a data.frame. 
#' 
#' @export

### This calculates the areas for various kinds of plots in m^2

setGeneric(name="getSubplotCount",function(object,group,years=NA,plots=NA,subtype='all',...){standardGeneric("getSubplotCount")},signature=c("object") )

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
        area= XSubplots<-ifelse("numSapPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSapPlots) %>%     
                        mutate(SubPlotArea=numSapPlots*object@SapPlotSize[2]) %>% dplyr::select(-numSapPlots)), 
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=object@SapPlotSize[1]*object@SapPlotSize[2]))),

        count= XSubplots<-ifelse("numSapPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSapPlots) %>% 
                        mutate(numSubPlots=numSapPlots) %>% dplyr::select(-numSapPlots)),
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@SapPlotSize[1]))),

        all= XSubplots<-ifelse("numSapPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSapPlots) %>%     
                        mutate(numSubPlots=numSapPlots,SubPlotArea=numSapPlots*object@SapPlotSize[2]) %>% dplyr::select(-numSapPlots)), 
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@SapPlotSize[2],
                        SubPlotArea=object@SapPlotSize[1]*object@SapPlotSize[2])))
        ),
      
      seedlings = switch (subtype,
        area= XSubplots<-ifelse("numSeedPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSeedPlots) %>%     
                        mutate(SubPlotArea=numSeedPlots*object@SeedPlotSize[2]) %>% dplyr::select(-numSeedPlots)), 
                        return(XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=object@SeedPlotSize[1]*object@SeedPlotSize[2]))),
        
        count= XSubplots<-ifelse("numSeedPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSeedPlots) %>% 
                        mutate(numSubPlots=numSeedPlots) %>% dplyr::select(-numSeedPlots)),
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@SeedPlotSize[1]))),
      
        all= XSubplots<-ifelse("numSeedPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numSeedPlots) %>%     
                        mutate(numSubPlots=numSeedPlots,SubPlotArea=numSeedPlots*object@SeedPlotSize[2]) %>% dplyr::select(-numSeedPlots)), 
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@SeedPlotSize[2],
                        SubPlotArea=object@SeedPlotSize[1]*object@SeedPlotSize[2])))),
      
      herbs = switch (subtype,
        area= XSubplots<-ifelse("numHerbPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numHerbPlots) %>%     
                        mutate(SubPlotArea=numHerbPlots*object@HPlotSize[2]) %>%  dplyr::select(-numHerbPlots)), 
                        return(XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(SubPlotArea=object@HPlotSize[1]*object@HPlotSize[2]))),
        
        count= XSubplots<-ifelse("numHerbPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numHerbPlots) %>% 
                        mutate(numSubPlots=numHerbPlots) %>% dplyr::select(-numHerbPlots)),
                        return(XSubplots %>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@HPlotSize[1]))),
      
        all= XSubplots<-ifelse("numHerbPlots" %in% names(XSubplots), return(XSubplots%>% dplyr::select(Plot_Name,Event_Year,numHerbPlots) %>%     
                        mutate(numSubPlots=numHerbPlots, SubPlotArea=numHerbPlots*object@HPlotSize[2]) %>% dplyr::select(-numHerbPlots)), 
                        return(XSubplots%>% dplyr::select(Plot_Name,Event_Year) %>% mutate(numSubPlots=object@HPlotSize[2],
                        SubPlotArea=object@HPlotSize[1]*object@HPlotSize[2]))))
      )
        
    return(XSubplots)
     })

