#' @title getPlants
#'
#' @importFrom dplyr filter mutate
#' @importFrom purrr map map2
#'
#' @description Retrieves plant data from an \code{NPSForVeg} object.
#'
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param group  A required character string indicating which group of plants should be selected. Options are: "trees", "saplings",
#' "seedlings", "shrubs" "shseedlings"(indicates shrub seedlings), "vines", "herbs", or "cwd'.
#' @param status  A required character string indicating if user wants data from living or dead plants. Used only for trees, saplings and shrubs. Values of this argument are matched to the \code{Status} field in the \code{Tree}, \code{Saplings} or \code{Shrubs} slot.  Acceptable options are:
#' \describe{
#' \item{"alive"}{The default. Includes any plant with a status of "Alive", "Alive Standing", "Alive Broken", "Alive Leaning" ,"Alive Fallen", "Recruit Standing", "Recruit Broken",
#' "Recruit Leaning", "Recruit Fallen", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", or "RS"}
#' \item{"dead"}{Includes any plant with a status of "Dead", "Dead Fallen", "Dead - Human Action", "Dead Leaning", "Dead Missing", "Dead Standing", "Dead - Too Small","DB","DC","DF","DL","DM","DS" or "DX"}
#' \item{"snag"}{Standing dead tree only, Includes any plant with a status of "Dead", "Dead - Human Action", "Dead Leaning", "Dead Missing", "Dead Standing", "Dead - Too Small","DB","DL","DM",or"DS"}
#' \item{"other"}{Includes any plant with a status of "Missing", "Missing - Presumed Dead", "Missing - Uncertain" , "Downgraded to Non-Sampled","ES","EX","NL","XO", "XP" or"XS" }
#' \item{"all"}{Includes all plants}
#' }
#' @param species  Defaults to \code{NA}. A character vector of Latin names. When included only those species are selected. This is determined by matching the appropriate \code{Latin_Name} field to  \code{species}
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is determined by matching the appropriate \code{Cycle} field to \code{cycles}
#' @param years  Defaults to \code{NA}. A numeric vector indicating which years should be included. This is determined by matching the appropriate \code{Sample_Year} field to \code{years}
#' @param plots Defaults to \code{NA} A character vector indicating which plots should be included. This is determined by matching the appropriate \code{Plot_Name} field to \code{plots}.
#' @param tag Defaults to \code{NA}. A numeric vector indicating which tags should be included. This is determined by matching the \code{Tag} field to \code{tag} for the vegetation data.
#' @param crown Defaults to \code{NA}. A character vector indicating which crown classes should be included. This is determined by matching the appropriate \code{crown_Description} field to \code{crown}. Options include "Co-dominant", "Dominant", "Edge Tree", "Intermediate", "Light Gap Exploiter","Open-grown" and "Overtopped".
#' @param stems Defaults to \code{NA}. A numeric vector indicating the number of stems a plant can have an be included in the output. Deteremien by matching to the \code{Stems} field in the vegetation data.
#' @param size.min Defaults to \code{NA}. A single numeric value that indicates the minimum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_Live_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpreted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param size.max Defaults to \code{NA}. A single numeric value that indicates the maximum plant size that should be included. For trees and saplings this will be interpreted as diameter in the \code{Equiv_Live_DBH_cm} field, for tree and shrub seedlings it is interpreted as height in the \code{Height} field and for herbs it is interpreted as percent cover in the \code{Percent_Cover} field. This has no effect on shrubs and vines.
#' @param BA.min Defaults to \code{NA}. A single numeric value that indicates the minimum basal area a plant must have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumLiveBasalArea_cm2} field.
#' @param BA.max Defaults to \code{NA}. A single numeric value that indicates the maximum basal area a plant must have to be included. This is only implemented for trees and saplings, and is interpreted as basal area in the \code{SumLiveBasalArea_cm2} field.
#' @param host.tree Defaults to \code{NA}. Only meaningful when \code{group} is "vines". A character vector containing Latin names of host trees. Only vines occurring in those hosts will be returned. Determined by matching the \code{Host_Latin_Name} in the \code{Vines} slot.
#' @param in.crown Defaults to \code{FALSE}. Only meaningful when \code{group} is "vines". When \code{TRUE} only vines which have reached the crown of the tree are returned. Determined by checking if the \code{Condition} field in the \code{Vines} slot has the text "Vines in the crown".
#' @param decay Defaults to \code{NA}. Only meaningful when \code{group} is "cwd". Only CWD with that decay class will be returned.
#' @param common Defaults to \code{FALSE}. Indicates if common names should be used rather than Latin names. Common names are determined using the \code{getCommons} function. Only works when \code{obejct} is an \code{NPSForVeg} object of a \code{list} of such objects
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be combined into a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that objects \code{ParkCode} slot.
#'
#' @details This function extracts data on plants from an \code{NPSForVeg} object. This function is called by other analysis and graphing functions. \code{getPlants} has a variety of arguments that narrow the response down to plants that match criteria such as Latin name, size, crown class etc.
#'
#' getPlants
#' @examples
#' \dontrun{
#' netn <- importNETN("C:/NETN/R_Dev/data/NPSForVeg/NETN")
#'
#' acad_plots <- getPlotNames(netn, parks = "ACAD")
#' acad_trees <- getPlants(netn, group = "trees", status = "alive") |>
#'   dplyr::filter(Plot_Name %in% acad_trees)
#'
#' canopy_trees <- getPlants(netn, group = "trees",
#'   crown = c("Dominant", "Co-dominant", "Intermediate"))
#'
#' MABI_saplings <- getPlants(netn, group = "saplings",
#'   status = "alive", years = 2021:2024) |> dplyr::filter(Unit_Code == "MABI")
#'
#' MORR_exotic_herbs <- getPlants(netn, group = "herbs") |>
#'   dplyr::filter(Exotic == TRUE) |>
#'   dplyr::filter(Unit_Code == "MORR")
#'
#' cwd <- getPlants(netn, group = "cwd", years = 2021:2024)
#'
#' }
#'
#'
#' @export



setGeneric(name="getPlants",function(object, group, status="alive", species=NA, cycles=NA, years=NA, plots=NA,
                                     tag=NA, crown=NA, stems=NA, size.min=NA, size.max=NA,
                                     BA.min=NA, BA.max=NA, host.tree=NA, in.crown=FALSE, decay=NA, common=FALSE,
                                     output="dataframe"){standardGeneric("getPlants")}, signature="object")


setMethod(f="getPlants", signature="list",
          function(object, group, status, species, cycles, years, plots, tag,
                   crown, stems, size.min, size.max, BA.min, BA.max, host.tree,
                   in.crown, decay, common,output){


           DataPull<-function(object, group){
             switch(group,
                    seedlings = object@Seedlings,
                    shseedlings = object@ShSeedlings,
                    trees = object@Trees,
                    saplings = object@Saplings,
                    shrubs = object@Shrubs,
                    vines = object@Vines,
                    herbs = object@Herbs,
                    cwd = object@CWD,
                    stop("Unknown Plant Type", call. = FALSE)
            )}

            XPlants<-map(.x=object, .f=~DataPull(.x, group=group))

            OutPlants <- switch(output,
                                list = map(.x=XPlants, .f= ~ getPlants(.x,group,status,species,
                                          cycles,years,plots,tag,crown,stems,size.min,size.max,
                                          BA.min, BA.max,host.tree,in.crown,decay)),

                                dataframe = getPlants(object = bind_rows(XPlants), group,status,species,
                                    cycles,years,plots,tag, crown, stems,size.min,size.max,
                                    BA.min, BA.max,host.tree,in.crown,decay)
                                )



            OutPlants <-if(common) switch(output,
                                list = map2(.x=OutPlants, .y=object, .f= ~ mutate(.x, Latin_Name = getPlantNames(object=.y,names=.x$Latin_Name))),

                                dataframe = mutate(OutPlants, Latin_Name=getPlantNames(object=object[[1]],names=OutPlants$Latin_Name))
            ) else (OutPlants)


            return(OutPlants)


          })


setMethod(f="getPlants", signature=c(object="NPSForVeg"),
          function(object,group,status,species,cycles,years,plots,tag, crown,stems, size.min,size.max,BA.min,BA.max,host.tree,in.crown,decay,common, output){

            XPlants<- switch(group,
                seedlings = object@Seedlings,
                shseedlings = object@ShSeedlings,
                trees = object@Trees,
                saplings = object@Saplings,
                shrubs = object@Shrubs,
                vines = object@Vines,
                herbs = object@Herbs,
                cwd = object@CWD,
          stop("Unknown Plant Type", call. = FALSE)
            )

           XPlants <- getPlants(object=XPlants, group=group, status=status, species=species, cycles=cycles, years=years, plots=plots,
                                 tag=tag, crown=crown, stems=stems, size.min=size.min, size.max=size.max,
                                 BA.min=BA.min, BA.max=BA.max, host.tree=host.tree, in.crown=in.crown, decay=decay)

            if(common){ XPlants$Latin_Name<-getPlantNames(object=object,names=XPlants$Latin_Name)}
            return(XPlants)
})



setMethod(f="getPlants", signature="data.frame",
          function(object, group, status, species, cycles, years, plots, tag,
                   crown, stems, size.min, size.max, BA.min, BA.max, host.tree,
                   in.crown, decay, common)
          {


            if(group=="trees" | group=="saplings" | group=="shrubs") {
              switch(status,
                     all=object<-object,
                     alive=object<-filter(object, Status %in% c("Alive Standing", "Alive Broken", "Alive Leaning", "Alive Missed",
                                                                   "Alive Fallen","Alive", "Recruit Standing", "Recruit Broken",
                                                                   "Recruit Leaning", "Recruit Fallen",
                                                                   "AB","AF","AL","AM","AS","RB","RF","RL","RS")),
                     dead=object<-filter(object, Status %in% c("Dead","Dead Leaning","Dead Missing",
                                                                  "Dead Fallen","Dead - Human Action","Dead - Too Small","Dead Cut","DC","DF","DX",
                                                                  "Dead Standing", "Dead Missed", "Dead Broken", "DB","DL","DM","DS")),
                     snag= object<-filter(object, Status %in% c("Dead","Dead Leaning","Dead Missing","Dead - Human Action",
                                                                   "Dead - Too Small", "Dead Standing", "Dead Missed", "Dead Broken", "DB","DL","DM","DS")),
                     other=object<-filter(object, Status %in% c("Missing","Missing - Presumed Dead","Missing - Uncertain",
                                                                   "Excluded - Other","Excluded - Off Plot","Excluded - Shrank","Excluded", "Other",
                                                                   "Downgraded to Non-Sampled","ES","EX","NL","XO","XP","XS")),
                     stop("Unknown Plant Status"))
            }

            if(!anyNA(species)) object <- filter(object, Latin_Name %in% species)

            if(!anyNA(cycles)) object <- filter(object, Cycle %in% cycles)

            if(!anyNA(years)) object <- filter(object, Sample_Year %in% years)

            if(!anyNA(plots)) object <- filter(object, Plot_Name %in% plots)

            if(!anyNA(tag)) object <- filter(object, Tag %in% tag)

            if(!anyNA(crown) & group=="trees") object <- filter(object, Crown_Description %in% crown)

            if(!anyNA(stems)) object <- filter(object, Stems %in% stems)

            if(!is.na(size.min)) switch(group,
                                        trees=object<-if (status %in% c("dead","snag")){
                                          filter(object, Equiv_Dead_DBH_cm>=size.min & !is.na(Equiv_Dead_DBH_cm)) } else {
                                            filter(object, Equiv_Live_DBH_cm>=size.min & !is.na(Equiv_Live_DBH_cm))},
                                        saplings= object<-filter(object, Equiv_Live_DBH_cm>=size.min & !is.na(Equiv_Live_DBH_cm)),
                                        seedlings=,shseedlings= object<-filter(object, Height>=size.min & !is.na(Height)),
                                        herbs=object<-filter(object, Percent_Cover>=size.min & !is.na(Percent_Cover)))

            if(!is.na(size.max)) switch(group,
                                        trees=object<-if (status %in% c("dead","snag")){
                                          filter(object, Equiv_Dead_DBH_cm<=size.max & !is.na(Equiv_Dead_DBH_cm)) } else {
                                            filter(object, Equiv_Live_DBH_cm<=size.max & !is.na(Equiv_Live_DBH_cm))},
                                        saplings= object<-filter(object, Equiv_Live_DBH_cm<=size.max  & !is.na(Equiv_Live_DBH_cm)),
                                        seedlings=,shseedlings= object<-filter(object, Height<=size.max & !is.na(Height)),
                                        herbs=object<-filter(object, Percent_Cover<=size.max & !is.na(Percent_Cover)))

            if(!is.na(BA.min)) switch(group,
                                      trees=object<-if (status %in% c("dead","snag")){
                                        filter(object, SumDeadBasalArea_cm2>=BA.min & !is.na(SumDeadBasalArea_cm2)) } else {
                                          filter(object,  SumLiveBasalArea_cm2>=BA.min & !is.na(SumLiveBasalArea_cm2))},
                                      saplings= object<-filter(object, SumLiveBasalArea_cm2>=BA.min & !is.na(SumLiveBasalArea_cm2)),
                                      seedlings=,shseedlings= stop("Seedlings don't have a basal area", call. = FALSE),
                                      herbs=stop("Herbs don't have a basal area", call. = FALSE))

            if(!is.na(BA.max)) switch(group,
                                      trees=object<-if (status %in% c("dead","snag")){
                                        filter(object, SumDeadBasalArea_cm2<=BA.max & !is.na(SumDeadBasalArea_cm2)) } else {
                                          filter(object,  SumLiveBasalArea_cm2<=BA.max & !is.na(SumLiveBasalArea_cm2))},
                                      saplings= object<-filter(object, SumLiveBasalArea_cm2<=BA.max & !is.na(SumLiveBasalArea_cm2)),
                                      seedlings=,shseedlings= stop("Seedlings don't have a basal area", call. = FALSE),
                                      herbs=stop("Herbs don't have a basal area", call. = FALSE))

            if(!anyNA(host.tree)>0 & group=="vines") object<-filter(object, Host_Latin_Name %in% host.tree)

            if(!anyNA(decay) & group=='cwd') object <- filter(object, Decay_Class %in% decay)

            if(in.crown & group =="vines") object<-filter(object, Condition=="Vines in the crown")

     return(object)
})




