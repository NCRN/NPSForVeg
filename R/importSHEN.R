#' @include NPSForVeg_Class_def.R
#' @title importSHEN
#' 
#' @description  This function imports data from Shenandoah's .csv files and saves it as an \code{NPSForVeg} object. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, Shrubs, Shrub_Seedlings, Herbs and CommonNames.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns 1 \code{NPSForVeg} object for the park.
#' 
#' @export
#' 


importSHEN<-function(Dir){

  InPlots<-read.csv(paste(Dir,"Plots.csv",sep="/"),as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  
  InEvents<-read.csv(paste(Dir,"Events.csv",sep="/"),as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  
  InTrees<-read.csv(paste(Dir,"Trees.csv",sep="/"),as.is=T, header=T)
  InSaps<-read.csv(paste(Dir,"Saplings.csv",sep="/"),as.is=T, header=T)
  InSeeds<-read.csv(paste(Dir, "Seedlings.csv",sep="/"),as.is=T, header=T)
  InShrubs<-read.csv(paste(Dir,"Shrubs.csv",sep="/"),as.is=T, header=T)
  InShSeeds<-read.csv(paste(Dir,"Shrub_Seedlings.csv",sep="/"),as.is=T, header=T)
  InHerbs<-read.csv(paste(Dir,"Herbs.csv",sep="/"),as.is=T, header=T)
  InCommons<-read.csv(paste(Dir,"CommonNames.csv",sep="/"), as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""]
  InCommons$TSN<-as.character(InCommons$TSN)

  
 SHEN<-new("NPSForVeg", 
           ParkCode="SHEN", 
           ShortName="Shenandoah", 
           LongName="Shenandoah National Park", 
           Network="SHEN", 
           
           TPlotSize=c(1, 576),
           SapPlotSize=c(2, 48), 
           SeedPlotSize=c(2, 6), 
           ShrubPlotSize=c(2, 48), 
           ShSeedPlotSize=c(2, 6), 
           HPlotSize=c(1,576),
           
           Plots=InPlots[InPlots$Unit_Code=="SHEN",], 
           Events=InEvents[InEvents$Unit_Code=="SHEN",],
           
           Trees=InTrees[InTrees$Unit_Code=="SHEN",], 
           Saplings=InSaps[InSaps$Unit_Code=="SHEN",], 
           Seedlings=InSeeds[InSeeds$Unit_Code=="SHEN",], 
           Shrubs=InShrubs[InShrubs$Unit_Code=="SHEN",], 
           ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="SHEN",],
           Herbs=InHerbs[InHerbs$Unit_Code=="SHEN",],
           Commons=InCommons)
  
  return(SHEN)
}

