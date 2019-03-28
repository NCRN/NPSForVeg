#' @title importNETN
#' 
#' @description  This function imports data from the standard NETN .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, and CommonNames.
#' 
#' @param Dir  Path to the directory where the data is found, in quotes. Path should not have a trailing slash.
#' 
#' @return Returns a list with 8 \code{NPSForVeg} objects, one for each park.
#' 
#' @export
#' 


importNETN<-function(Dir){

  InPlots<-read.csv(paste(Dir,"Plots.csv",sep="/"),as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  
  InEvents<-read.csv(paste(Dir,"Events.csv", sep="/"),as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  
  InTrees<-read.csv(paste(Dir,"Trees.csv",sep="/"),as.is=T, header=T)
  InSaps<-read.csv(paste(Dir,"Saplings.csv",sep="/"),as.is=T, header=T)
  InSeeds<-read.csv(paste(Dir,"Seedlings.csv",sep="/"),as.is=T, header=T)
  #InShrubs<-read.csv("Shrubs.csv",as.is=T, header=T)
  #InShSeeds<-read.csv("Shrub_Seedlings.csv",as.is=T, header=T)
  #InVines<-read.csv("Vines.csv",as.is=T, header=T)
  InHerbs<-read.csv(paste(Dir,"Herbs.csv",sep="/"),as.is=T, header=T)
  InCommons<-read.csv(paste(Dir, "CommonNames.csv",sep="/"), as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""]
  InCommons$TSN<-as.character(InCommons$TSN)
  InCWD<-read.csv(paste(Dir,'CWD.csv',sep="/"), as.is=T, header=T)
  
  ACAD<-new("NPSForVeg", 
            ParkCode="ACAD", 
            ShortName="Acadia", 
            LongName="Acadia National Park", 
            Network="NETN", 
            
            TPlotSize=c(1,15*15),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,15*15),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="ACAD",], 
            Events=InEvents[InEvents$Unit_Code=="ACAD",],
            
            Trees=InTrees[InTrees$Unit_Code=="ACAD",], 
            Saplings=InSaps[InSaps$Unit_Code=="ACAD",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="ACAD",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="ACAD",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="ACAD",],
            #Vines=InVines[InVines$Unit_Code=="ACAD",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="ACAD",],
            CWD=InCWD[InCWD$Unit_Code=='ACAD',],
            Commons=InCommons)
  
  MABI<-new("NPSForVeg", 
            ParkCode="MABI", 
            ShortName="Marsh-Billings-Rockefeller", 
            LongName="Marsh-Billings-Rockefeller National Historical Park", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="MABI",], 
            Events=InEvents[InEvents$Unit_Code=="MABI",],
            
            Trees=InTrees[InTrees$Unit_Code=="MABI",], 
            Saplings=InSaps[InSaps$Unit_Code=="MABI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="MABI",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="MABI",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="MABI",],
            #Vines=InVines[InVines$Unit_Code=="MABI",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="MABI",],
            CWD=InCWD[InCWD$Unit_Code=='MABI',],
            Commons=InCommons)
  
  MIMA<-new("NPSForVeg", 
            ParkCode="MIMA", 
            ShortName="Minute Man", 
            LongName="Minute Man National Historical Park", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="MIMA",], 
            Events=InEvents[InEvents$Unit_Code=="MIMA",],
            
            Trees=InTrees[InTrees$Unit_Code=="MIMA",], 
            Saplings=InSaps[InSaps$Unit_Code=="MIMA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="MIMA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="MIMA",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="MIMA",],
            #Vines=InVines[InVines$Unit_Code=="MIMA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="MIMA",],
            CWD=InCWD[InCWD$Unit_Code=='MIMA',],
            Commons=InCommons)
  
  MORR<-new("NPSForVeg", 
            ParkCode="MORR", 
            ShortName="Morristown", 
            LongName="Morristown National Historical Park", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="MORR",], 
            Events=InEvents[InEvents$Unit_Code=="MORR",],
            
            Trees=InTrees[InTrees$Unit_Code=="MORR",], 
            Saplings=InSaps[InSaps$Unit_Code=="MORR",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="MORR",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="MORR",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="MORR",],
            #Vines=InVines[InVines$Unit_Code=="MORR",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="MORR",],
            CWD=InCWD[InCWD$Unit_Code=='MORR',],
            Commons=InCommons)
  
  ROVA<-new("NPSForVeg", 
            ParkCode="ROVA", 
            ShortName="Roosevelt-Vanderbilt", 
            LongName="Roosevelt-Vanderbilt National Historic Sites", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="ROVA",], 
            Events=InEvents[InEvents$Unit_Code=="ROVA",],
            
            Trees=InTrees[InTrees$Unit_Code=="ROVA",], 
            Saplings=InSaps[InSaps$Unit_Code=="ROVA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="ROVA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="ROVA",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="ROVA",],
            #Vines=InVines[InVines$Unit_Code=="ROVA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="ROVA",],
            CWD=InCWD[InCWD$Unit_Code=='ROVA',],
            Commons=InCommons)
  
  SAGA<-new("NPSForVeg", 
            ParkCode="SAGA", 
            ShortName="Saint-Gaudens", 
            LongName="Saint-Gaudens National Historic Site", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="SAGA",], 
            Events=InEvents[InEvents$Unit_Code=="SAGA",],
            
            Trees=InTrees[InTrees$Unit_Code=="SAGA",], 
            Saplings=InSaps[InSaps$Unit_Code=="SAGA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="SAGA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="SAGA",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="SAGA",],
            #Vines=InVines[InVines$Unit_Code=="SAGA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="SAGA",],
            CWD=InCWD[InCWD$Unit_Code=='SAGA',],
            Commons=InCommons)
  
  SARA<-new("NPSForVeg", 
            ParkCode="SARA", 
            ShortName="Saratoga", 
            LongName="Saratoga National Historical Park", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="SARA",], 
            Events=InEvents[InEvents$Unit_Code=="SARA",],
            
            Trees=InTrees[InTrees$Unit_Code=="SARA",], 
            Saplings=InSaps[InSaps$Unit_Code=="SARA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="SARA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="SARA",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="SARA",],
            #Vines=InVines[InVines$Unit_Code=="SARA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="SARA",],
            CWD=InCWD[InCWD$Unit_Code=='SARA',],
            Commons=InCommons)
  
  WEFA<-new("NPSForVeg", 
            ParkCode="WEFA", 
            ShortName="Weir Farm", 
            LongName="Weir Farm National Historic Site", 
            Network="NETN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*2*2), 
            SeedPlotSize=c(3, pi*2*2), 
            ShrubPlotSize=c(3, pi*2*2), 
            ShSeedPlotSize=c(3, pi*2*2), 
            #VPlotSize=c(1,20*20),#not active for NETN
            HPlotSize=c(8,1),
            
            Plots=InPlots[InPlots$Unit_Code=="WEFA",], 
            Events=InEvents[InEvents$Unit_Code=="WEFA",],
            
            Trees=InTrees[InTrees$Unit_Code=="WEFA",], 
            Saplings=InSaps[InSaps$Unit_Code=="WEFA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="WEFA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="WEFA",], 
            #ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="WEFA",],
            #Vines=InVines[InVines$Unit_Code=="WEFA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="WEFA",],
            CWD=InCWD[InCWD$Unit_Code=='WEFA',],
            Commons=InCommons)
  return(c(ACAD,MABI,MIMA,MORR,ROVA,SAGA,SARA,WEFA))
}




