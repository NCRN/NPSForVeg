#' @title dens
#' 
#' @include NPSForVeg_Class_def.R
#' @importFrom MASS glm.nb 
#' @importFrom boot boot
#' @importFrom dplyr arrange filter
#' @importFrom magrittr %>% 
#' 
#' @description calculates  mean "density" in abundance, size and occupancy and 95\% confidence intervals.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a \code{list} of such objects.
#' @param group A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shrubs" "shseedlings"(indicated shrub seedlings), "vines", or "herbs'. 
#' @param years Defaults to \code{NA}. A required numeric vector indicating which years should be included.
#' @param values Defaults to "count". Passed on to \code{\link{SiteXSpec}} Indicates the type of measurement which is calculated by the SiteXSpec function. Options are:
#' \describe{
#' \item{"count"}{The default. The number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants, for vines, it the number of trees a vine species grows on, and for herbs it will be the number of quadrats the plant occurs in.}
#' \item{"size"}{For trees and saplings this is the total basal area per plot per ha. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the average percent cover across all sampled quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
#' \item{"presab"}{A presence-absence matrix. When a plant species is present in a given plot, the corresponding cell value will be 1. Otherwise it will be 0.}
#' }
#' @param density Logical Value, Only used when \code{values="count"}. Defaults to \code{TRUE}, presents results on per hectare basis. \code{FALSE} presents results on a per plot basis. For \code{values="presab"} this is always treated as \code{FALSE} and results are always a proportion of plots.
#' @param species Character vector, defaults to \code{NA}. Passed on to \code{\link{SiteXSpec}} and from there to \code{\link{getPlants}} to limit hte output to one or more designated species.
#' @param plots Character vecltor, defaults to \code{NA}. When not \code{NA} indicates which plots should be used in the analysis. Otherwise all plots are sused
#' @param area A character vector. Used when \code{values="size"} Determines if the values in the output are on a per plot or per area basis. This only works with counts, 
#' basal areas, and length of seedlings.
#' \describe{
#' \item{"plot"}{The default. Each cell will be ona per plot basis. The only option for \code{values="presab"}.}
#' \item{"ha"}{Values will be on a per hectare basis}
#' \item{"ac"}{values will be in a per acre basis}
#' }
#' @param plotarea Single number. Defaults to \code{NA}. Used when the input object is a \code{list} and the ouput is a \code{data.frame} and is on a per plot basis.
#' This  indicates how big the total sampled area per plots is assuemed to be. If \code{NA}#' the plot size from the first object in the list is used.   
#' @param subplots Single number. Defaults to \code{NA}. Used when the input object is a \code{list} and the ouput is a \code{data.frame} and values are "presab". 
#' This  indicates how may subplot are assumed to be sampled. If \code{NA} the number of subplots from the first object in the list is used.
#' @param subplotarea Vector of numbers. Indicates how many subplots were sampled for each plot. If left as \code{NA} will be automatically determined using \code{getSubplotCount}
#' @param subplotnumber Vector of number Inidcates how large an area was sampled for each plot. If left as \code{NA} will be automatically determined using \code{getSubplotCount}
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe",the default, indicates the \code{NSPForVeg} objects should combined and treated as a single dataset and return a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} with resutls from a  single \code{NSPForVeg} object, and each element is named based on that object's \code{ParkCode} slot.
#' @param ... Any additional arguments which are valid for either \code{SiteXSpec} or \code{getPlants}.
#' 
#' @details  This function calculates the mean and 95\% confidence values for a given measurement for one or more object of class \code{NPSForVeg}. The raw data for this calculation is acquired by calling \code{SiteXSpec} which will in turn call \code{getPlants}. The method of calculating the results depends on the \code{values} argument. If \code{values="count"} the data is assuemd to follow a negative binomial distribution. The mean and the intervals are calculated using the \code{glm.nb} function in the \pkg{MASS} package. Note that when a species if found in few of the plots, this function will often return a \code{warning}. 
#' If \code{values="size"} no distribution is assumed for the underlying data. Instead, confidence intervals and determined using 1000 bootstrap replicates. Note that if a species is relatively rare this can lead to the 95% bootstrap confidence interval including 0.
#' If \code{values="presab"} data is treat as being binomial and confidence intervals are detemined using \code{glm}.  
#' 
#' Complications arise when the sampling effort is uneven in the data you are analyzing. This can happen in a variety of ways. If a 
#' subplot is not sampled during an event, perhaps due to a safety concern, the the analysis need to take into account that some of the
#' data is missing. Similarly, if the design of the plot changes, adding or remving subplots or changing the area sampled similar problems occur.
#' Finally, if you are combining data from  areas with differnt plot designs, this also must be taken into account.
#' 
#' When working with a single park, or a list of parks when \code{output="list"}, the function will use an offset of area sampled when 
#' \code{values = "count"}, will use input data on a per ha or per acre basis (determined by \code{area}) when \code{values="size"} and 
#' will use an offset based on subplots sampled when \code{values="presab"}. Areas and subplots sampled are specified using the
#' \code{subplotarea} and \code{subplotnumber} argurments, but will be automatically determined with a call to \code{getSubplotCount}
#' if left as \code{NA}
#'
#' When the input is a list, \code{output="list"} data from muliple parks are combined. Differences in sampled area are accounted for
#' in the same manner as above. However, if the output is desired on a per plot basis rather than a per area basis (always the case when
#' \code{values="preasb"}), then it is necessary to assume a particular plot size or number of subplots. This is particularly true
#' when combining parks with different areas sampled. This is done through the \code{plotarea} and \code{subplots} arguments. 
#' 
#' @return Returns a \code{data.frame},or a \code{list} or them, which indicate the mean and 95\% confidence intervals of the requested measurement.
#' 
#' @export


#############################################
#####  dens()  estimates the density or per plot abundance and 95% CIs
##### uses negative binomial modeling from the MASS package
###############################################

setGeneric(name="dens",function(object,group, years, values="count", density=TRUE,species=NA, plots=NA, area="count", plotarea=NA, subplots=NA,
                                subplotarea=NA, subplotnumber=NA, output="dataframe",...){standardGeneric("dens")}, signature="object")

setMethod(f='dens', signature=c(object="list"),
          function(object,...){
            plotarea<-if(is.na(plotarea)) getArea(object[[1]],group=group, type="all" ) else plotarea
            subplots<-if (is.na(subplots)) getArea(object=object[[1]], group=group, type="count") else subplots
            switch(output,
                   list={OUT<-lapply(object,FUN=dens,group=group,years=years,values=values, density=density, species=species, plots=plots,
                                     area=area, plotarea=plotarea, subplots=subplots, subplotarea=subplotarea, subplotnumber=subplotnumber, ...)
                         names(OUT)<-getNames(object,"code")
                    },
                   
                    dataframe={
                      ObjectData<-SiteXSpec(object=object, group=group, years=years, plots=plots, area=area, values=values, species=species,...)
                      subplot<-getSubplotCount(object=object, group=group, years=years, plots=plots, subtype="all") %>% arrange(Plot_Name)
                      OUT<-dens(ObjectData, group=group, values=values, density=density,  plotarea=getArea(object=object, group=group, type="all"),
                                subplots=getArea(object=object, group=group, type="count"), 
                                subplotarea=subplot$SubPlotArea, subplotnumber=subplot$numSubPlots)
                    }
            )
            return(OUT)
})

setMethod(f="dens", signature=c(object="NPSForVeg"), 
  function(object,...){
    ObjectData<-SiteXSpec(object=object,group=group,years=years,plots=plots,area=area,values=values, species=species,...)
    subplot<-getSubplotCount(object=object, group=group,years=years, plots=plots, subtype='all') %>% arrange(Plot_Name)
    
    OUT<-dens(ObjectData, group=group, values=values, density=density, plotarea=getArea(object=object, group=group, type="all"),
             subplots=getArea(object=object, group=group, type="count"), subplotarea=subplot$SubPlotArea, 
              subplotnumber=subplot$numSubPlots)
              
              
    return(OUT)
})

setMethod(f="dens", signature=c(object="data.frame"), 
  function(object,...){
    
    object$subplotarea<-subplotarea
    object$numSubPlots<-subplotnumber
    TempData<-object %>% filter((subplotarea>0 | is.na(subplotarea)) & (numSubPlots>0 | is.na(numSubPlots))) 
    SpeciesNames<-names(TempData)[!names(TempData) %in% c("Plot_Name","subplotarea","numSubPlots")]
    OutData<-data.frame(Latin_Name=SpeciesNames, Mean=NA, Lower.95=NA, Upper.95=NA)
    if(nrow(TempData)==0){return(TempData)}
    
    switch(values,
      count={
        if(group !="herbs"){
          NBS<-lapply(X=SpeciesNames, FUN=function(Y,Z){ if(sum(Z[[Y]])==0) return(NULL) else  glm.nb(Z[[Y]]~1+offset(log(Z$subplotarea)))}, Z=TempData)
          OutData$Mean<-sapply(X=NBS,FUN=function(x){if (is.null(x)) NA else exp(coef(x)+log(10000))}) #this is per ha 
          OutData[3:4]<-matrix(sapply(X=NBS, FUN=function(x){if(is.null(x)) NA else suppressMessages(exp(confint(x)+log(10000)))}),ncol=2,byrow=TRUE) #again per ha
        
            if(!density ) {
            OutData[2:4]<-OutData[2:4]*(plotarea/10000)
          }
        }
        
        if(group=="herbs"){
          NBS<-lapply(X=SpeciesNames, FUN=function(Y,Z){ if(sum(Z[[Y]])==0) return(NULL) else  glm.nb(Z[[Y]]~1+offset(log(Z$numSubPlots)))}, Z=TempData)
          OutData$Mean<-sapply(X=NBS,FUN=function(x){if (is.null(x)) NA else exp(coef(x)+log(subplots))}) 
          OutData[3:4]<-matrix(sapply(X=NBS, FUN=function(x){if(is.null(x)) NA else suppressMessages(exp(confint(x)+log(subplots)))}),ncol=2,byrow=TRUE) 
        }
      },
                   
      size={
        OutData$Mean<-sapply(X=SpeciesNames, FUN=function(X,Z){mean(Z[[X]])}, Z=TempData)
        SzBoot<-list()
        SzBoot<-lapply(
          X=SpeciesNames, Z=TempData, FUN=function(X,Z){
            BSZ<-boot(data=Z[[X]],statistic=function(X,Y) {mean(X[Y])}, R=1000 )
            quantile(BSZ$t, c(0.025, 0.975))
        })
        OutData[3:4]<-matrix(unlist(SzBoot),ncol=2,byrow=TRUE)  
      },
                   
      presab={
        TempData$Total<-NULL
        SpeciesNames<-SpeciesNames[!SpeciesNames=="Total"]
        OutData<-OutData[OutData$Latin_Name!="Total",]
        BIN<-lapply(X=SpeciesNames, FUN=function(Y,Z) {if(sum(Z[[Y]])==0) return(NULL) else glm(Z[[Y]]~1+offset(log(Z$numSubPlots)), family=binomial)}, Z=TempData)
        OutData$Mean<-sapply(X=BIN,FUN=function(x){if (is.null(x)) NA else coef(x)+log(subplots)})
        OutData[3:4]<-matrix(sapply(X=BIN, FUN=function(Z){suppressMessages(confint(Z))+log(subplots)}),ncol=2,byrow=TRUE )
        OutData[2:4]<-plogis(as.matrix(OutData[2:4]))
      },
                   
      stop("value type not recognized")
    )
            
    OutData[2:4]<-round(OutData[2:4],digits=3)
            
    return(OutData) 
})
