#' @title SiteXSpec
#' 
#' @import plyr
#' @import data.table
#' @import reshape2
#' 
#' @description Produces a Site X Species matrix. Each cell can contain a count, a measure of size or 1/0 indicating presence/abscence
#' 
#' @param object either an object of class \code{NPSForVeg} or a list of such objects
#' @param groups  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings","shrubs", "shseedlings" (indicated shrub seedlings), "vines" or "herbs'.
#' @param years Defaults to \code{NA}. A numeric vector indicating which years should be included. This is passed on to \code{\link{getPlants}}.
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is passed on to \code{\link{getPlants}}.
#' @param values Determines the data contained in the Site X Species matrix. Possible values are:
#' \describe{
#' \item{"count"}{The default. Each cell will incude a count of the number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the nubmer of plants, for vines, it the number of trees a vine species grows on, and for herbs it will be the number of quadrats the plant occurs in.}
#' \item{"size"}{For trees and saplings this is the total basal area. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the total percent cover across all quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
#' \item{"presab"}{Produces a presrnce/absence matrix. When a plant species is present in a given plot the corresponding cell value will be one, otherwise it is zero.}
#' }
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be a single large matrix, containing all sites and species from all \code{NPSForVeg} objects. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot. 
#' @param species A character vector of names. Defaults to \code{NA}. When not \code{NA} only species included in \code{species} will be included in the matrix. If a name is in \code{species}, but is not present in the data, it will not appear in the matrix. 
#' @param Total Logical value. Determine if a "Total" column will be included in the matrix. Defaults to TRUE.
#' @param ... Other arguments passed on to \code{\link{getPlants}}
#' 
#' @details This function will first call \code{\link{getPlants}} to retrieve the plant data. Then a SiteXSpecies matrix will be created. Values in the cells are determined by the option selected with \code{values}. Each row corresponds to a different plot and each column to a different species. 
#' 
#' Note that \code{species} can be a vector of common or Latin names. If common names are used then \code{common=TRUE} must be included in the function call. 
#' 
#' @export
#' 



setGeneric(name="SiteXSpec",function(object,group,years=NA, cycles=NA,values="count",output="dataframe",species=NA,Total=TRUE,...){standardGeneric("SiteXSpec")}, signature="object")

setMethod(f="SiteXSpec", signature=c(object="list"),
         function(object,...){
            switch(output,
              dataframe={
                TempPark<-make(object,ParkCode="TEMPOBJ", ShortName="TempObj",LongName="Temp park for SiteXSpec", Network="SiteSpec")
                return(SiteXSpec(object=TempPark, group=group, years=years, cycles=cycles, species=species, Total=Total,values=values,...))
              },
            
              list={
                OutList<-llply(.dat=object, .fun=SiteXSpec, group=group,years=years,cycles=cycles, species=species,...)
                names(OutList)<-getNames(object,name.class="code")
                return(OutList)
                }
         )
})

setMethod(f="SiteXSpec", signature=c(object="NPSForVeg"), 
          function(object, group, years, cycles, values, species, ...){
            
            XPlants<-data.table( getPlants(object=object,group=group,years=years,cycles=cycles,species=species,...))
            XPlots<-getPlotNames(object=object,years=years,type="all")
            XPlants[,fPlot:=factor(Plot_Name, levels=XPlots)]
            
            
            switch(values, 
              count= OutData<-dcast.data.table(setkey(XPlants,fPlot,Latin_Name)[CJ (unique(levels(fPlot)), unique(Latin_Name)), .N], 
                                                formula=fPlot~Latin_Name, value.var="N"),
              size={
                switch(group,
                  
                  trees=,saplings=OutData<-dcast.data.table( setkey(XPlants,fPlot,Latin_Name)[CJ (unique(levels(fPlot)), unique(Latin_Name)),
                                      sum(SumLiveBasalArea_cm2)],formula=fPlot~Latin_Name,value.var="V1", drop=FALSE),
                         
                         seedlings=,shseedlings=OutData<-dcast.data.table( setkey(XPlants,fPlot,Latin_Name)[CJ (unique(levels(fPlot)), 
                                      unique(Latin_Name)),sum(Height)],formula=fPlot~Latin_Name,value.var="V1", drop=FALSE),
                  
                         herbs=OutData<-dcast.data.table( setkey(XPlants,fPlot,Latin_Name)[CJ (unique(levels(fPlot)), unique(Latin_Name)),
                                      sum(Percent_Cover)],formula=fPlot~Latin_Name,value.var="V1", drop=FALSE),
                  
                         shrubs=,vines=stop("Cannot do a size based site x species matrix - no size measurement avaialable")
              )},
              presab={OutData<-dcast.data.table(setkey(XPlants,fPlot,Latin_Name)[CJ (unique(levels(fPlot)), unique(Latin_Name)), .N], 
                                       formula=fPlot~Latin_Name, value.var="N")
                    for(xx in seq_along(OutData)) {set(OutData, i=which(is.numeric(OutData[[xx]]) & as.numeric(OutData[[xx]])>0), j=xx, value=1)}
                    
              },
              stop("values type not recognized")
            )  
            
            for(i in seq_along(OutData)) { set(OutData,i=which(is.na(OutData[[i]])), j=i, value=0 ) }  #removes NA's
              
            setnames(OutData,"fPlot","Plot_Name")
            OutData[, Plot_Name:=as.character(Plot_Name)]
            
            if(Total){OutData[,Total:=rowSums(.SD), .SDcol=-1]}
            
            return(as.data.frame(OutData))
  })






