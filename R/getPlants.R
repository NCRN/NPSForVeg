#' @title getPlants
#' 
#' @description Retrieves plant data from an \code{NPSForVeg} object.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param group  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shrubs" "shseedlings"(indicates shrub seedlings), "vines", or "herbs". 
#' @param status  A requried character string indicating if user wants data from living or dead plants. Used only for trees, saplings and shrubs. Values of this argument are matched to the \code{Status} field in the \code{Tree}, \code{Saplings} or \code{Shrubs} slot.  Acceptable options are:
#' \describe{
#' \item{"alive"}{The default. Includes any plant with a status of "Alive", "Alive Standing", "Alive Broken", "Alive Leaning" ,"Alive Fallen","AB","AF","AL","AM","AS","RB","RF","RL","RS" or "TR"}
#' \item{"dead"}{Includes any plant with a status of "Dead"," "Dead Fallen", "Dead - Human Action", "Dead Leaning", "Dead Missing", "Dead Standing", "Dead - Too Small","DB","DC","DF","DL","DM","DS" or "DX"}
#' \item{"other"}{Includes any plant with a status of "Missing", "Missing - Presumed Dead", "Missing - Uncertain" , "Downgraded to Non-Sampled","ES","EX","NL","XO", "XP" or"XS" }
#' \item{"all"}{Includes all plants}
#' }
#' @param species  Defaults to \code{NA}. A character vector of Latin names. When included only those species are selected. This is determined by matching the appropriate \code{Latin_Name} field to  \code{species}
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is determined by matching the appropriate \code{Cycle} field to \code{cycles}
#' @param years  Defaults to \code{NA}. A numeric vector indicating which years should be included. This is determined by matching the appropriate \code{Sample_Year} field to \code{years}
#' @param plots Defaults to \code{NA} A character vector indicating which plots should be included. This is determined by macthing the appropriate \code{Plot_Name} field to \code{plots}.
#' @param crown Defaults to \code{NA}. A character vector indicating which crown classes should be included. This is determined by matching the appropriate \code{crown_Description} field to \code{crown}. Options include "Co-dominant", "Dominant", "Edge Tree", "Intermediate", "Light Gap Exploiter","Open-grown" and "Overtopped".
#' @param size.min Defaults to \code{NA}. A single numeric value that indicates the minimum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_Live_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpeted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param size.max Defaults to \code{NA}. A single numeric value that indciates the maximum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_Live_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpeted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param BA.min Defaults to \code{NA}. A single numeric value that indicates the minimum basal area a plant must have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumLiveBasalArea_cm2} field. 
#' @param BA.max Defaults to \code{NA}. A single numeric value that indicates the maximum basal area a plant must have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumLiveBasalArea_cm2} field. 
#' @param host.tree Defaults to \code{NA}. Only meaningful when \code{group} is "vines". A character vector containing Latin names of host trees. Only vines occuring in those hosts will be returned. Determined by matching the \code{Host_Latin_Name} in the \code{Vines} slot. 
#' @param in.crown Defaults to \code{FALSE}. Only meaningful when \code{group} is "vines". When \code{TRUE} only vines which have reached the crown of the tree are returned. Determined by checking if the \code{Condition} field in the \code{Vines} slot has the text "Vines in the crown".
#' @param common Defaults to \code{FALSE}. Indicates if common names should be used rather than Latin names. Common names are determined using the \code{getCommons} function.
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be combined into a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that objects \code{ParkCode} slot. 
#' 
#' @details This function extracts data on plants from an \code{NPSForVeg} object. This function is called by other analysis and graphing functions. \code{getPlants} has a variety of arguments that narrow the response down to plants that match critera such as Latin name, size, crown class etc.
#' 
#' @export



setGeneric(name="getPlants",function(object,group,status="alive", species=NA, cycles=NA, years=NA, plots=NA, crown=NA, size.min=NA, size.max=NA, BA.min=NA, BA.max=NA, host.tree=NA, in.crown=FALSE, common=FALSE, decay=NA, output="dataframe"){standardGeneric("getPlants")}, signature="object")



setMethod(f="getPlants", signature="list",
          function(object,group,status,species,cycles,years,plots,crown,size.min,size.max,BA.min, BA.max,host.tree,in.crown,common,decay,output) 
          {OutPlants<-lapply(X=object, FUN=getPlants, group=group, status=status, species=species, cycles=cycles, years=years, plots=plots, crown=crown,size.min=size.min, size.max=size.max, BA.min=BA.min, BA.max=BA.max, host.tree=host.tree, in.crown=in.crown, common=common,decay=decay,output=output)
           switch(output,
                  list=return(OutPlants),
                  dataframe=return(do.call("rbind",OutPlants))
           )
          })


setMethod(f="getPlants", signature=c(object="NPSForVeg"), 
          function(object,group,status,species,cycles,years,plots,crown,size.min,size.max,BA.min,BA.max,host.tree,in.crown,common,decay, output){
            switch(group,
                seedlings=XPlants<-(object@Seedlings),
                shseedlings=XPlants<-(object@ShSeedlings),
                trees=XPlants<-object@Trees,  
                saplings= XPlants<-object@Saplings,
                shrubs=XPlants<-object@Shrubs,
                vines=XPlants<-object@Vines,
                herbs=XPlants<-object@Herbs,
                cwd=XPlants<-object@CWD,
          stop("Unknown Plant Type")
            )
            
            if(group=="trees" | group=="saplings" | group=="shrubs") {
               switch(status,
                            all=XPlants<-XPlants,
                            alive=XPlants<-(XPlants[XPlants$Status %in% c("Alive Standing", "Alive Broken", "Alive Leaning", "Alive Missed",
                                                                          "Alive Fallen","Alive","AB","AF","AL","AM","AS","RB","RF","RL","RS","TR"),]),
                            dead=XPlants<-(XPlants[XPlants$Status %in% c("Dead","Dead Leaning","Dead Missing", 
                              "Dead Standing", "Dead Missed", "Dead Broken", "DB","DL","DM","DS"),]),
                            other=XPlants<-(XPlants[XPlants$Status %in% c("Missing","Missing - Presumed Dead","Missing - Uncertain",
                              "Dead Fallen","Dead - Human Action","Dead - Too Small","Dead Cut","DC","DF","DX",
                              "Excluded - Other","Excluded - Off Plot","Excluded - Shrank","Excluded", "Other",
                                                                          "Downgraded to Non-Sampled","ES","EX","NL","XO","XP","XS"),]),
                            stop("Unknown Plant Status"))
            }
            
            
            if(!sum(is.na(species))>0) XPlants<-XPlants[XPlants$Latin_Name %in% species,]
            
            if(!sum(is.na(cycles))>0) XPlants<-XPlants[XPlants$Cycle %in% cycles,]
            
            if(!sum(is.na(years))>0) XPlants<-XPlants[XPlants$Sample_Year %in% years,]
            
            if(!sum(is.na(plots))>0) XPlants<-XPlants[XPlants$Plot_Name %in% plots,]
            
            if(!sum(is.na(crown))>0 & group=="trees") XPlants<-XPlants[XPlants$Crown_Description %in% crown,]
            
            if(!is.na(size.min)) switch(group,
                                      trees=,saplings= XPlants<-XPlants[XPlants$Equiv_Live_DBH_cm>=size.min & !is.na(XPlants$Equiv_Live_DBH_cm),],
                                      seedlings=,shseedlings= XPlants<-XPlants[XPlants$Height>=size.min & !is.na(XPlants$Height),],
                                      herbs=XPlants<-XPlants[XPlants$Percent_Cover>=size.min & !is.na(XPlants$Percent_Cover),])
            
            if(!is.na(size.max)) switch(group,
                                        trees=,saplings= XPlants<-XPlants[XPlants$Equiv_Live_DBH_cm<=size.max  & !is.na(XPlants$Equiv_Live_DBH_cm),],
                                        seedlings=,shseedlings= XPlants<-XPlants[XPlants$Height<=size.max & !is.na(XPlants$Height),],
                                        herbs=XPlants<-XPlants[XPlants$Percent_Cover<=size.max & !is.na(XPlants$Percent_Cover),])
            
            if(!is.na(BA.min)) switch(group,
                                    trees=,saplings= XPlants<-XPlants[XPlants$SumLiveBasalArea_cm2>=BA.min & !is.na(XPlants$SumLiveBasalArea_cm2),],
                                    seedlings=,shseedlings= stop("Seedlings don't have a basal area"),
                                    herbs=stop("Herbs don't have a basal area"))
            
            if(!is.na(BA.max)) switch(group,
                                    trees=,saplings= XPlants<-XPlants[XPlants$SumLiveBasalArea_cm2<=BA.max & !is.na(XPlants$SumLiveBasalArea_cm2),],
                                    seedlings=,shseedlings= stop("Seedlings don't have a basal area"),
                                    herbs=stop("Herbs don't have a basal area"))
            
            if(!sum(is.na(host.tree))>0 & group=="vines") XPlants<-XPlants[XPlants$Host_Latin_Name %in% host.tree,]
            
            if(!anyNA(decay) & group=='cwd') XPlants<-XPlants[XPlants$Decay_Class %in% decay,]
                        
            if(in.crown & group =="vines") XPlants<-XPlants[XPlants$Condition=="Vines in the crown",]
            
            if(common){ XPlants$Latin_Name<-getPlantNames(object=object,names=XPlants$Latin_Name)}
            return(XPlants)
})






