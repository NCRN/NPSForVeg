#' @title IV
#'
#' @description  Calculates "importance values" from forest data
#'
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param group A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shseedlings"(indicated shrub seedlings), or "herbs'. Currently not implemented for "vines" or "shrubs".
#' @param years years Defaults to \code{NA}. A numeric vector indicating which years should be included. This is passed on to \code{\link{getPlants}}.
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe",the default, indicates the output from all of \code{NSPForVeg} objects should be a data.frame combining all data as if all objects were a single park. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NSPForVeg} object, and each element is named based on that object's \code{ParkCode} slot.
#' @param ... Other arguments passed to \code{\link{SiteXSpec}}. If not used by that function the arguments will be passed to \code{\link{getPlants}}.
#'
#' @return A data.frame with the IV values for each species. Values are returned for the three separate components of IV (Density, Size and Distribution), as well as the Total. The values for each of the tree components will sum to one, so the the total will sum to three.
#'
#' @details This function is used to calculate forestry importance values in conjunction with \code{\link{calcIV}}. One or more objects of class \code{NPSForVeg} are used. The arguments to the function are passed on to \code{\link{SiteXSpec}} and from there to \code{\link{getPlants}}, so any argument which is accepted by either of these functions (eg. \code{size.min} or \code{crown}) is valid. \code{getPlants} is used to acquire the relevant data.
#'    \code{SiteXSpec} is then used to create three site X species matrices - one which measures abundance per plot, one which is a measure of size per plot and one which is a presence/absence matrix. For trees, saplings and seedlings abundance is measured as the number of stems per plot, whereas for herb abundance is the number of quadrats a species is found in. Size is measured as basal area for trees and saplings, height for seedlings and percent cover for herbs.
#'    Once the matrices are created they are passed to \code{\link{calcIV}} which calculates the IV values.
#'
#' @include NPSForVeg_Class_def.R
#'
#' @examples
#' \dontrun{
#' ncrn <- importNCRN("C:/Data/")
#' tree_iv <- IV(ncrn, group = 'trees', years = 2021:2024)
#'
#' midn <- importMIDN("C:/Data/")
#' bowa_iv <- IV(midn[[2]], group = 'seedlings', years = 2021:2024)
#'
#' }
#'
#'
#' @export

setGeneric(name="IV",function(object,group,years=NA,output="dataframe",...){standardGeneric("IV")}, signature="object")

setMethod(f="IV", signature=c(object="list"),
          function(object, group, years, output,...){
            switch(output,
                   list={Out<-lapply(object,IV,group=group,years=years,...)
                         names(Out)<-getNames(object,name.class="code")
                         return(Out)},
                    dataframe={
                      CountData<-SiteXSpec(object=object,group=group,years=years,values="count",Total=FALSE,...)
                      SizeData<-SiteXSpec(object=object,group=group,years=years,values="size",Total=FALSE,...)
                      PresData<-SiteXSpec(object=object,group=group,years=years,values="presab",Total=FALSE,...)
                      return(calcIV(InCount=CountData, InSize=SizeData, InPres=PresData))
                   })
  })


setMethod(f="IV", signature=c(object="NPSForVeg"),
          function(object, group, years,...){

            CountData<-SiteXSpec(object=object,group=group,years=years,values="count",Total=FALSE,...)
            SizeData<-SiteXSpec(object=object,group=group,years=years,values="size",Total=FALSE,...)
            PresData<-SiteXSpec(object=object,group=group,years=years,values="presab",Total=FALSE,...)
            return(calcIV(InCount=CountData, InSize=SizeData, InPres=PresData))
})
