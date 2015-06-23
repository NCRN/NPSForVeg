#' @title ChangeMatrix
#' 
#' @import plyr
#' 
#' @description Produces a matrix which indcates the change in some measure of plant abundance between two time periods
#' 
#' @param object either an object of class \code{NPSForVeg} or a list of such objects
#' @param groups  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings","shrubs", "shseedlings" (indicated shrub seedlings), "vines" or "herbs'.
#' @param years1 A numeric vector indicating the first set of years to be included. 
#' @param years2 A numeric vector indicating the second set of years to be included. 
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be a single large \code{data.frame}, containing all sites and species from all \code{NPSForVeg} objects. "list" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot. 
#' @param species A character vector of names. Defaults to \code{NA}. When not \code{NA} only species included in \code{species} will be included in the matrix. If a name is in \code{species}, but is not present in the data, it will not appear in the output. 
#' @param plots A character vector of plot names. Defaults to \code{NA}. When not \code{NA} only plots included in \code{plots} will be included in the matrix. If a plot name is in \code{plots}, but is not present in the data, it will not appear in the output.
#' @param type Passed to \code{\link{getPlots}}. One of three options that indicate what type of plots are to be considered. Must be in quotes. Options are:
#' \describe{
#' \item{"active"}{The default. Only returns names of plots which are listed as active in the \code{Plots$Location_Status} field.} 
#' \item{"all"}{The default. Returns names from all types of plots.}
#' \item{"retired}{Only returns names from plots which are listed as retired in the \code{Plots$Location_Status} field.}
#' }
#' @param values Determines the data contained in the Site X Species matrix. Possible values are:
#' \describe{
#' \item{"count"}{The default. Each cell will include a count of the number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants. For vines, it is the number of trees a vine species grows on. For herbs it will be the number of quadrats the plant occurs in.}
#' \item{"size"}{For trees and saplings this is the total basal area. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the total percent cover across all quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
#' \item{"presab"}{Produces a presence/absence matrix. When a plant species is present in a given plot the corresponding cell value will 1, otherwise it is 0.}
#' }
#' @param ... Other arguments passed on to \code{\link{SiteXSpec}}
#' 
#' 
#' @details This function will first call \code{\link{SiteXSpec}} to create  site by species matrices for each of the two time periods. The output matrix will then be created by subtracting the values from time 1 from the values from time 2. While it is generally assumed that the two time periods do not overlap and that \code{years2} are later than \code{years1}, this is not enforced and the user can enter any sets of years they like.
#' 
#' Additional arguments accepted by \code{\link{SiteXSpec}} or \code{\link{getPlants}} can be used as well. Currently using the \code{cycles} argument will result in errors. 
#' Note that \code{species} can be a vector of common or Latin names. If common names are used then \code{common=TRUE} must be included in the function call. 
#' 
#' @include NPSForVeg_Class_def.R
#' @export
#' 

###################################################
##### ChangeMatrix()
##### Makes a Site X Species matrix which reflects changes between two time periods
##################################################

setGeneric(name="ChangeMatrix",function(object,group,years1,years2,plots=NA,type="active",species=NA,values="count",
                                        output="dataframe",...){standardGeneric("ChangeMatrix")}, signature="object")

setMethod(f="ChangeMatrix", signature=c(object="list"), 
    function(object, ...){
      switch(output,
        dataframe={
          TempPark<-make(object,ParkCode="TEMPOBJ", ShortName="TempObj",LongName="Temp park for SiteXSpec", Network="SiteSpec")
            return(ChangeMatrix(object=TempPark, group=group, years1=years1, years2=years2, plots=plots, type=type, species=species,values=values, ...))
        },

        list={
            OutList<-llply(.dat=object, .fun=ChangeMatrix, group=group, years1=years1, years2=years2, plots=plots, type=type, species=species, values=values,...)
            names(OutList)<-getNames(object,name.class="code")
            return(OutList)
        }
      )
})


setMethod(f="ChangeMatrix", signature=c(object="NPSForVeg"), 
         function(object,group,years1,years2,plots,type,species,values,...){
           
          
           #find plots common to both time periods
           PlotsUse<-if(!is.na(plots)) {plots} else { 
                            intersect(getPlotNames(object=object,type=type,years=years1),
                            getPlotNames(object=object, type=type, years=years2)
                            )}
           #find plants common to both time periods
           SpeciesUse<-sort(
             if(!is.na(species)) {species} else {
              unique(getPlants(object=object,group=group,years=c(years1,years2), plots=PlotsUse, ...)$Latin_Name )
             }
           )
           
           ########## get the initial matrices
           Matrix1<-SiteXSpec(object=object,group=group,years=years1,species=SpeciesUse,plots=PlotsUse,values=values,... )
           
           Matrix2<-SiteXSpec(object=object,group=group,years=years2,species=SpeciesUse,plots=PlotsUse,values=values,... )
           
           ########## Make the  change matrix
           
           Matrix3<-data.frame(matrix(NA,nrow=length(PlotsUse),ncol=length(SpeciesUse)+1))
           
           # column names, Plot_Names and Total are easy
           colnames(Matrix3)<-c("Plot_Name",SpeciesUse)
           Matrix3[1]<-PlotsUse
           if(exists("Total",Matrix1) & exists("Total",Matrix2)) {Matrix3$Total=Matrix2$Total-Matrix1$Total}
           
           #Existor is a vector which indicates if a species in just in Matrix1 =-1, just Matrix2 =1, or both =0
           
           Existor<- -1*sapply(SpeciesUse,FUN=exists,Matrix1) + sapply(SpeciesUse,FUN=exists,Matrix2)
           Matrix3[,names(Existor[Existor==0])]=Matrix2[,names(Existor[Existor==0])]-Matrix1[,names(Existor[Existor==0])]
           Matrix3[,names(Existor[Existor==-1])] =  -Matrix1[,names(Existor[Existor==-1])]
           Matrix3[,names(Existor[Existor==1])] =  Matrix2[,names(Existor[Existor==1])]

           
           
           return(Matrix3)                
           
   ## need to use the arguement to get plants and get plotnames to figure out what the two SxS should be like
   ## then make the two sxs and then subtract
        })