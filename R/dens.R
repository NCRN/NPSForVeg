#' @title dens
#' 
#' @include NPSForVeg_Class_def.R
#' @importFrom MASS glm.nb 
#' @importFrom boot boot
#' 
#' @description calculates  mean "density" in abundance, size and occupancy and 95\% confidence intervals.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a \code{list} of such objects.
#' @param group A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shrubs" "shseedlings"(indicated shrub seedlings), "vines", or "herbs'. 
#' @param years Defaults to \code{NA}. A required numeric vector indicating which years should be included.
#' @param values Defaults to "count". Passed on to \code{\link{SiteXSpec}} Indicates the type of measurement which is calculated by the SiteXSpec function. Options are:
#' \describe{
#' \item{"count"}{The default. The number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants, for vines, it the number of trees a vine species grows on, and for herbs it will be the number of quadrats the plant occurs in.}
#' \item{"size"}{For trees and saplings this is the total basal area per plot. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the average percent cover across all quadrats that were sampled. For shrubs and vines there is no defined size and the function will terminate with an error.}
#' \item{"presab"}{A presence-absence matrix. When a plant species is present in a given plot, the corresponding cell value will be 1. Otherwise it will be 0.}
#' }
#'@param density Logical Value, defaults to \code{TRUE}, presents results on per hectare basis. \code{FALSE} presents results on a per plot basis. For \code{values="presab"} this is always treated as \code{FALSE} and results are always a proportion of plots.
#'@param species Character vector, defaults to \code{NA}. Passed on to \code{\link{SiteXSpec}} and from there to \code{\link{getPlants}} to limit hte output to one or more designated species.
#' @param  output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe",the default, indicates the \code{NSPForVeg} objects should combined and treated as a single dataset and return a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} with resutls from a  single \code{NSPForVeg} obeject, and each element is named based on that object's \code{ParkCode} slot. 
#' @param ... Any additional arguments which are valid for either \code{SiteXSpec} or \code{getPlants}.
#' 
#' @details  This function calculates the mean and 95\% confidence values for a given meauserment for one or more object of class \code{NPSForVeg}. The raw data for this calculation is acquried by calling \code{SiteXSpec} which will in turn call \code{getPlants}. The method of calculating the results depends on the \code{values} argument. If \code{values="count"} the data is assuemd to follow a negative binomial distriburtion. The mean and the intervals are calculated using the \code{glm.nb} function in the \pkg{MASS} package. Note that when a species if found in few of the plots, this function will often return a \code{warning}. 
#' If \code{values="size"} no distirbution is assuemd for the underlying data. Instead, confidence intervals and detemrined using 1000 bootstrap replicates. Note that if a species is realtively rare this can lead to the 95% bootstrap confidence interval including 0.
#' If \code{values="presab"} data is treat as being binomial and confidence intervals are detemined using \code{glm}.  
#' 
#' @return Returns a \code{data.frame},or a \code{list} or them, which indicate the mean and 95\% condfidence intervals of the requested measurement.  
#' 
#' 
#' @export





#############################################
#####  dens()  estaiamtes the denstiy or per plot abundnace and 95% CIs
##### uses negative binomial modeling from the MASS package
###############################################



setGeneric(name="dens",function(object,group,years,values="count", density=TRUE,species=NA, output="dataframe",...){standardGeneric("dens")}, signature="object")

setMethod(f='dens', signature=c(object="list"),
          function(object,...){
            switch(output,
                   list={OUT<-lapply(object,FUN=dens,group=group,years=years,values=values, density=density, species=species,...)
                         names(OUT)<-getNames(object,"code")},
                   dataframe={TempDens<-make(object=object,ParkCode="TEMPDENS",ShortName="Temporary Park",LongName="Temp object made by dens()",
                              Network="TEMPDENSNET")
                              OUT<-dens(TempDens, group=group, years=years, values=values, density=density,species=species, ...)}
            )
            return(OUT)
})



setMethod(f="dens", signature=c(object="NPSForVeg"), 
         function(object,...){
          TempData<-SiteXSpec(object=object,group=group,years=years,values=values, species=species,...)
          OutData<-data.frame(Latin_Name=names(TempData[-1]),Mean=NA,Lower.95=NA, Upper.95=NA)
          
          switch(values,
                 count={
                    NBS<-lapply(X=names(TempData[-1]), FUN=function(Y,Z){glm.nb(Z[[Y]]~1)}, Z=TempData[-1] )
                    OutData$Mean<-sapply(X=NBS,coef)
                    OutData[3:4]<-matrix(sapply(X=NBS, FUN=function(x){suppressMessages(confint(x))}),ncol=2,byrow=TRUE)
                    OutData[2:4]<-exp(OutData[2:4])
                  },
                  size={
                    if(group=="herbs") 
                    OutData$Mean<-sapply(X=names(TempData[-1]), FUN=function(X,Z){mean(Z[[X]])}, Z=TempData[-1])
                    SzBoot<-list()
                    SzBoot<-lapply(
                      X=names(TempData[-1]), Z=TempData[-1], FUN=function(X,Z){
                      BSZ<-boot(data=Z[[X]],statistic=function(X,Y) {mean(X[Y])}, R=1000 )
                      quantile(BSZ$t, c(0.025, 0.975))
                      })
                      
                    OutData[3:4]<-matrix(unlist(SzBoot),ncol=2,byrow=TRUE)  
                 },
                 presab={
                   TempData$Total<-NULL
                   OutData<-OutData[OutData$Latin_Name!="Total",]
                   BIN<-lapply(X=names(TempData[-1]), FUN=function(Y,Z) {glm(Z[[Y]]~1, family=binomial)}, Z=TempData[-1])
                   OutData$Mean<-sapply(X=BIN,coef)
                   OutData[3:4]<-matrix(sapply(X=BIN, FUN=function(Z){suppressMessages(confint(Z))}),ncol=2,byrow=TRUE )
                   OutData[2:4]<-plogis(as.matrix(OutData[2:4]))
                 },
                 stop("value type not recognized")
          )
          if(density & group!="herbs" & values!="presab") {
            OutData[2:4]<-OutData[2:4]*10000/getArea(object,group,type="all")
          }
          OutData[2:4]<-round(OutData[2:4],digits=3)
          return(OutData) 
})







