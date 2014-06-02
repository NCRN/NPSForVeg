#' @title getPlants
#' 
#' @description Retrieves plant data from an \code{NPSForVeg} object.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param group  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shrubs" "shseedlings"(indicated shrub seedlings), "vines", or "herbs'. 
#' @param status  A requried character string indicating if user wants data from living or dead plants. Used only for trees and saplings. values from the agrument are matched to the \code{Trees$Tree_Status} field in the \code{Tree} slot, or the \code{Saplings$Sapling_Status} field in the \code{Saplings} slot.  Acceptable options are:
#' \describe{
#' \item{"alive"}{The default. Includes any plant with a status of "Alive", "Alive Standing", "Alive Broken", "Alive Leaning" or "Alive Fallen"}
#' \item{"dead"}{Includes any plant with a status of "Dead"," "Dead Fallen" or "Dead Standing"}
#' \item{"other"}{Includes any plant with a status of "Missing" or "Downgraded to Non-Sampled" }
#' \item{"all"}{Includes all plants}
#' }
#' @param species  Defaults to \code{NA}. A character vector of Latin names. When included only those species are selected. This is determined by matching the appropriate \code{Latin_Name} field to the \code{species}
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is determined by matching the appropriate \code{Cycle} field to the \code{cycles}
#' @param years  Defaults to \code{NA}. A numeric vector indicating which years should be included. This is determined by matching the appropriate \code{Sample_Year} field to \code{years}
#' @param plots Defaults to \code{NA} A character vector indicating which plots should be included. This is determined by mathing the appropriate \code{Plot_Name} field to \code{plots}.
#' @param crown Defaults to \code{NA}. A character vector indicating which crown classes should be included. This is determined by mathing the appropriate \code{crown_Description} field to \code{crown}. Options include "Co-dominant", "Dominant", "Edge Tree", "Intermediate", "Light Gap Exploiter","Open-grown" and 'Overtopped".
#' @param size.min Defaults to \code{NA}. A single numeric value that indciates the minimum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpeted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param size.max Defaults to \code{NA}. A single numeric value that indciates the maximum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpeted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param BA.min Defaults to \code{NA}. A single numeric value that indciates the minimum basal area a plant must have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumBasalArea_cm2} field. 
#' @param BA.max Defaults to \code{NA}. A single numeric value that indciates the maximum basal area a plant may have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumBasalArea_cm2} field. 
#' @param host.tree Defaults to \code{NA}. Only meaningful when \code{group} is "vines". A character vector containing Latin names of host trees. Only vines occuirng in those hosts will be returned. Determined by matching the \code{Host_Latin_Name} in the \code{Vines} slot. 
#' @param in.crown Defaults to \code{FALSE}. Only meaningful when \code{group} is "vines". When \code{TRUE} only vines which have reached the crown of the tree are returned. Determined by checking if the \code{Condition} field in the \code{Vines} slot has the text "Vines in the crown".
#' @param common Defaults to \code{FALSE}. Indicated if common names should be used rather than Latin names. Common names are determined using the \code{getCommons} funciton.
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe", the default, indicates the ouput from all of NSPForVeg objects should be combined into a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} froma  single NSPForVeg obeject, and each element is named based on that objects \code{ParkCode} slot. 
#' 
#' @details This fuction extracts data on plants from an NPSForVeg object. This function is called by other analysis and graphing functions. \code{getPlants} has a vaiety of arguments that narrow the response down to plants that match critera such as Latin name, size, crown class etc.
#' 
#' @export



setGeneric(name="getPlants",function(object,group,...,status="alive", species=NA, cycles=NA, years=NA, plots=NA, crown=NA, size.min=NA, size.max=NA, BA.min=NA, BA.max=NA, host.tree=NA, in.crown=FALSE, common=FALSE, output="dataframe"){standardGeneric("getPlants")}, signature="object")

setMethod(f="getPlants", signature=c(object="list"),
          function(object,group,...){
  
          MC<-match.call()
          XPlants<-vector(mode="list",length=length(object))
          for (i in seq_along(object)) {
            MC[2]<-object[[i]]
            XPlants[[i]]<-eval(MC)
          }
          switch(output,
                 list={names(XPlants)<-getNames(object,name.class="code")
                   return(XPlants)},
                 dataframe=return(do.call("rbind",XPlants) ))
})


setMethod(f="getPlants", signature=c(object="NPSForVeg"), 
          function(object,group,...){
            switch(group,
                seedlings=XPlants<-(object@Seedlings),
                shseedlings=XPlants<-(object@ShSeedlings),
                trees={
                  XPlants<-object@Trees  
                    switch(status,
                         all = XPlants<-XPlants,
                         alive = XPlants<-(XPlants[XPlants$Tree_Status %in% c("Alive Standing", "Alive Broken", "Alive Leaning", 
                                                                         "Alive Fallen","Alive"),]),
                         dead = XPlants<-(XPlants[XPlants$Tree_Status %in% c("Dead","Dead Fallen", "Dead Standing"),]),
                         other = XPlants<-(XPlants[XPlants$Tree_Status %in% c("Missing","Downgraded to Non-Sampled"),]),
                         stop("Unknown Plant Status"))},
                saplings= ,shrubs={if(group=="saplings") XPlants<-object@Saplings else XPlants<-object@Shrubs  
                  switch(status,
                        all=XPlants<-XPlants,
                        alive=XPlants<-(XPlants[XPlants$Sapling_Status %in% c("Alive Standing", "Alive Broken", "Alive Leaning", 
                                                                         "Alive Fallen","Alive"),]),
                        dead=XPlants<-(XPlants[XPlants$Sapling_Status %in% c("Dead","Dead Fallen", "Dead Standing"),]),
                        other=XPlatns<-(XPlants[XPlants$Sapling_Status %in% c("Missing","Downgraded to Non-Sampled"),]),
                        stop("Unknown Plant Status"))},
                vines=XPlants<-object@Vines,
                herbs=XPlants<-object@Herbs,
            stop("Unknown Plant Type")
            )
            if(!sum(is.na(species))>0) XPlants<-XPlants[XPlants$Latin_Name %in% species,]
            
            if(!sum(is.na(cycles))>0) XPlants<-XPlants[XPlants$Cycle %in% cycles,]
            
            if(!sum(is.na(years))>0) XPlants<-XPlants[XPlants$Sample_Year %in% years,]
            
            if(!sum(is.na(plots))>0) XPlants<-XPlants[XPlants$Plot_Name %in% plots,]
            
            if(!sum(is.na(crown))>0 & group=="trees") XPlants<-XPlants[XPlants$Crown_Description %in% crown,]
            
            if(!is.na(size.min)) switch(group,
                                      trees=,saplings= XPlants<-XPlants[XPlants$Equiv_DBH_cm>=size.min,],
                                      seedlings=,shseedlings= XPlants<-XPlants[XPlants$Height>=size.min,],
                                      herbs=XPlants<-XPlants[XPlants$Percent_Cover>=size.min,])
            
            if(!is.na(size.max)) switch(group,
                                        trees=,saplings= XPlants<-XPlants[XPlants$Equiv_DBH_cm<=size.max,],
                                        seedlings=,shseedlings= XPlants<-XPlants[XPlants$Height<=size.max,],
                                        herbs=XPlants<-XPlants[XPlants$Percent_Cover<=size.max,])
            
            if(!is.na(BA.min)) switch(group,
                                        trees=,saplings= XPlants<-XPlants[XPlants$SumBasalArea_cm2>=BA.min,],
                                        seedlings=,shseedlings= stop("Seedlings don't have a basal area"),
                                        herbs=stop("Herbs don't have a basal area"))
            
            if(!is.na(BA.max)) switch(group,
                                        trees=,saplings= XPlants<-XPlants[XPlants$SumBasalArea_cm2<=BA.max,],
                                        seedlings=,shseedlings= stop("Seedlings don't have a basal area"),
                                        herbs=stop("Herbs don't have a basal area"))
            
            if(!sum(is.na(host.tree))>0 & group=="vines") XPlants<-XPlants[XPlants$Host_Latin_Name %in% host.tree,]
            
            if(in.crown & group =="vines") XPlants<-XPlants[XPlants$Condition=="Vines in the crown",]
            
            if(common){ XPlants$Latin_Name<-getPlantNames(object=object,names=XPlants$Latin_Name)}
            return(XPlants)
})






