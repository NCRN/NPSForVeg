#' @include NPSForVeg_Class_def.R
#' @title importCSV
#' 
#' @importFrom purrr map
#' @importFrom dplyr filter
#' 
#' @description  This function imports data from the standard .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: MetaData, 
#' Plots, Events and CommonNames. The optional .csv files are: Trees, Saplings, Seedlings, Shrubs, Shrub_Seedlings, Vines, and Herbs.
#' 
#' @param Dir  The directory where the data is found. Path should not have a trailing slash.
#' 
#' @return Returns a \code{list} of \code{NPSForVeg} objects, each object is a park listed in the MetaData.csv file, named using the park code from that file.
#' 
#' @export
#' 


importCSV<-function(Dir){
  
  #### Check which data files are present so function knows which slots to fill
  InFiles<-list.files(Dir)

  MetaData<-read.csv(paste(Dir,"MetaData.csv",sep="/"),as.is=T, header=T)
  InPlots<-read.csv(paste(Dir,"Plots.csv",sep="/"),as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  
  InEvents<-read.csv(paste(Dir,"Events.csv",sep="/"),as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  InCycles <- if(any("Cycles.csv" %in% InFiles)) read.csv(paste(Dir,"Cycles.csv",sep="/"), as.is=T, header=T) else NA
  
  InTrees<- if(any("Trees.csv" %in% InFiles)) read.csv(paste(Dir,"Trees.csv",sep="/"),as.is=T, header=T) else NA
  InSaps<- if(any("Saplings.csv" %in% InFiles)) read.csv(paste(Dir,"Saplings.csv",sep="/"),as.is=T, header=T) else NA
  InSeeds<- if(any("Seedlings.csv" %in% InFiles)) read.csv(paste(Dir,"Seedlings.csv",sep="/"),as.is=T, header=T) else NA
  InShrubs<- if(any("Shrubs.csv" %in% InFiles)) read.csv(paste(Dir,"Shrubs.csv",sep="/"),as.is=T, header=T) else NA
  InShSeeds<- if(any("Shrub_Seedlings.csv" %in% InFiles)) read.csv(paste(Dir,"Shrub_Seedlings.csv",sep="/"),as.is=T, header=T) else NA
  InVines<- if(any("Vines.csv" %in% InFiles)) read.csv(paste(Dir,"Vines.csv",sep="/"),as.is=T, header=T) else NA
  InHerbs<-  if(any("Herbs.csv" %in% InFiles) )read.csv(paste(Dir,"Herbs.csv",sep="/"),as.is=T, header=T) else NA
  InCommons<- read.csv(paste(Dir,"CommonNames.csv",sep="/"), as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""]
  InCommons$TSN<-as.character(InCommons$TSN)


  
 #### MakeObj is a function that will create a new object from input data by filtering the data for each park and putting it int he correct slot
  MakeObj<-function(ParkCode, MetaData, InPlots, InEvents, InCycles, InTrees, InSaps, InSeeds, InShrubs, InShSeeds, InVines, InHerbs, InCommons){
    
    MD<-MetaData %>% filter(ParkCode==ParkCode)
    NewObj<-new("NPSForVeg", 
             ParkCode=ParkCode, 
             ShortName=MD$ShortName, 
             LongName=MD$LongName, 
             Network=MD$Network,
             
             TPlotSize=c(MD$TPlotNum,MD$TPlotSize),
             SapPlotSize=c(MD$SapPlotNum,MD$SaplPlotSize), 
             SeedPlotSize=c(MD$SeedPlotNum,MD$SeedPlotSize), 
             ShrubPlotSize=c(MD$ShrubPlotNum, MD$ShrubPlotSize), 
             ShSeedPlotSize=c(MD$ShSeedPlotNum,MD$ShSeedPlotSize), 
             VPlotSize=c(MD$VPlotNum,MD$VPlotSize),
             HPlotSize=c(MD$HPlotNum,MD$HPlotSize),
             Cycles=InCycles,
             
             Plots=filter(InPlots, Unit_Code==ParkCode), 
             Events=filter(InEvents, Unit_Code==ParkCode),
             
             Trees= if(is.data.frame(InTrees)) filter(InTrees,Unit_Code==ParkCode) else NA, 
             Saplings=if(is.data.frame(InSaps)) filter(InSaps, Unit_Code==ParkCode) else NA, 
             Seedlings=if(is.data.frame(InSeeds)) filter(InSeeds, Unit_Code==ParkCode) else NA, 
             Shrubs=if(is.data.frame(InShrubs)) filter(InShrubs, Unit_Code==ParkCode) else NA, 
             ShSeedlings=if(is.data.frame(InShSeeds)) filter(InShSeeds, Unit_Code==ParkCode) else NA,
             Vines=if(is.data.frame(InVines)) filter(InVines, Unit_Code==ParkCode) else NA,
             Herbs=if(is.data.frame(InHerbs)) filter(InHerbs, Unit_Code==ParkCode) else NA,
             Commons=InCommons)
  }
  
  OutList<-map(.x=MetaData$ParkCode, .f=MakeObj, MetaData=MetaData, 
               InPlots=InPlots, 
               InEvents=InEvents, 
               InCycles=InCycles, 
               InTrees=InTrees, 
               InSaps=InSaps,
               InSeeds=InSeeds, 
               InShrubs=InShrubs,
               InShSeeds=InShSeeds, 
               InVines=InVines, 
               InHerbs=InHerbs,
               InCommons=InCommons)
    
  names(OutList)<-MetaData$ParkCode
  
  return(OutList)
    
  }
