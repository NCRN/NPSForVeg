#' @include NPSForVeg_Class_def.R
#' @title importMIDN
#' 
#' @description  This function imports data from the standard MIDN .csv files and saves it as \code{NPSForVeg} objects. The required 
#' .csv files are: Plots, Events, Cycles, Cylces2, Trees, Saplings, Seedlings, Vines, Herbs, CWD and CommonNames.
#' 
#' @param Dir  The directory where the data is found. Path should not have a trailing slash.
#' 
#' @return Returns a list with 12 \code{NPSForVeg} objects, one for each park.
#' 
#' @export
#' 

importMIDN<-function(Dir){

  InPlots<-read.csv(paste(Dir,"Plots.csv",sep="/"),as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")

  InEvents<-read.csv(paste(Dir,"Events.csv", sep="/"),as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  InCycles<-read.csv(paste(Dir,"Cycles.csv",sep="/"), as.is=T, header=T)
  InCycles2<-read.csv(paste(Dir,"Cycles2.csv",sep="/"), as.is=T, header=T)

  InTrees<-read.csv(paste(Dir,"Trees.csv",sep="/"),as.is=T, header=T)
  InSaps<-read.csv(paste(Dir,"Saplings.csv",sep="/"),as.is=T, header=T)
  InSeeds<-read.csv(paste(Dir,"Seedlings.csv",sep="/"),as.is=T, header=T)
  #InShrubs<-read.csv(paste(Dir,"Shrubs.csv",sep="/"),as.is=T, header=T)
  #InShSeeds<-read.csv(paste(Dir,"Shrub_Seedlings.csv",sep="/"),as.is=T, header=T)
  InVines<-read.csv(paste(Dir,"Vines.csv",sep="/"),as.is=T, header=T)
  InHerbs<-read.csv(paste(Dir,"Herbs.csv",sep="/"),as.is=T, header=T)
  InCommons<-read.csv(paste(Dir,"CommonNames.csv",sep="/"), as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""]
  InCommons$TSN<-as.character(InCommons$TSN)
  InCWD<-read.csv(paste(Dir,"CWD.csv",sep="/"), as.is=T, header=T) 
  
  
  APCO<-new("NPSForVeg", 
            ParkCode="APCO", 
            ShortName="Appomattox", 
            LongName="Appomattox Court House National Historical Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20), 
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="APCO",], 
            Events=InEvents[InEvents$Unit_Code=="APCO",],
            
            Trees=InTrees[InTrees$Unit_Code=="APCO",], 
            Saplings=InSaps[InSaps$Unit_Code=="APCO",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="APCO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="APCO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="APCO",],
            Vines=InVines[InVines$Unit_Code=="APCO",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="APCO",],
            CWD=InCWD[InCWD$Unit_Code=="APCO",],
            Commons=InCommons)
  

  BOWA<-new("NPSForVeg", 
            ParkCode="BOWA", 
            ShortName="Booker T Washington", 
            LongName="Booker T Washington National Monument", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3),
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="BOWA",], 
            Events=InEvents[InEvents$Unit_Code=="BOWA",],
    
            Trees=InTrees[InTrees$Unit_Code=="BOWA",], 
            Saplings=InSaps[InSaps$Unit_Code=="BOWA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="BOWA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="BOWA",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="BOWA",],
            Vines=InVines[InVines$Unit_Code=="BOWA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="BOWA",],
            CWD=InCWD[InCWD$Unit_Code=="BOWA",],
            Commons=InCommons)
  
  COLO<-new("NPSForVeg", 
            ParkCode="COLO", 
            ShortName="Colonial", 
            LongName="Colonial National Historical Park", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="COLO",], 
            Events=InEvents[InEvents$Unit_Code=="COLO",],
    
            Trees=InTrees[InTrees$Unit_Code=="COLO",], 
            Saplings=InSaps[InSaps$Unit_Code=="COLO",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="COLO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="COLO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="COLO",],
            Vines=InVines[InVines$Unit_Code=="COLO",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="COLO",],
            CWD=InCWD[InCWD$Unit_Code=="COLO",],
            Commons=InCommons)
  
  
  FRSP<-new("NPSForVeg", 
            ParkCode="FRSP", 
            ShortName="Fredericksburg & Spotsylvania", 
            LongName="Fredericksburg and Spotsylvania National Military Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,

            Plots=InPlots[InPlots$Unit_Code=="FRSP",], 
            Events=InEvents[InEvents$Unit_Code=="FRSP",],
    
            Trees=InTrees[InTrees$Unit_Code=="FRSP",], 
            Saplings=InSaps[InSaps$Unit_Code=="FRSP",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="FRSP",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="FRSP",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="FRSP",],
            Vines=InVines[InVines$Unit_Code=="FRSP",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="FRSP",],
            CWD=InCWD[InCWD$Unit_Code=="FRSP",],
            Commons=InCommons)
  
  GETT<-new("NPSForVeg", 
            ParkCode="GETT", 
            ShortName="Gettysburg", 
            LongName="Gettysburg National Military Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1),
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="GETT",], 
            Events=InEvents[InEvents$Unit_Code=="GETT",],
    
            Trees=InTrees[InTrees$Unit_Code=="GETT",], 
            Saplings=InSaps[InSaps$Unit_Code=="GETT",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="GETT",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="GETT",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="GETT",],
            Vines=InVines[InVines$Unit_Code=="GETT",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="GETT",],
            CWD=InCWD[InCWD$Unit_Code=="GETT",],
            Commons=InCommons)
  
  GEWA<-new("NPSForVeg", 
            ParkCode="GEWA", 
            ShortName="George Washington Birthplace", 
            LongName="George Washington Birthplace National Monument", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3),
            SeedPlotSize=c(12,1),
            ShrubPlotSize=c(3, pi*3*3),
            ShSeedPlotSize=c(12,1),
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles2,
            
            Plots=InPlots[InPlots$Unit_Code=="GEWA",], 
            Events=InEvents[InEvents$Unit_Code=="GEWA",],
    
            Trees=InTrees[InTrees$Unit_Code=="GEWA",], 
            Saplings=InSaps[InSaps$Unit_Code=="GEWA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="GEWA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="GEWA",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="GEWA",],
            Vines=InVines[InVines$Unit_Code=="GEWA",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="GEWA",],
            CWD=InCWD[InCWD$Unit_Code=="GEWA",],
            Commons=InCommons)
  
  HOFU<-new("NPSForVeg", 
            ParkCode="HOFU", 
            ShortName="Hopewell Furnace", 
            LongName="Hopewell Furnace National Historic Site", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20), 
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="HOFU",], 
            Events=InEvents[InEvents$Unit_Code=="HOFU",],
    
            Trees=InTrees[InTrees$Unit_Code=="HOFU",], 
            Saplings=InSaps[InSaps$Unit_Code=="HOFU",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="HOFU",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="HOFU",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="HOFU",],
            Vines=InVines[InVines$Unit_Code=="HOFU",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="HOFU",],
            CWD=InCWD[InCWD$Unit_Code=="HOFU",],
            Commons=InCommons)
  
  PETE<-new("NPSForVeg", 
            ParkCode="PETE", 
            ShortName="Petersburg Battlefield", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20), 
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="PETE",], 
            Events=InEvents[InEvents$Unit_Code=="PETE",],
    
            Trees=InTrees[InTrees$Unit_Code=="PETE",], 
            Saplings=InSaps[InSaps$Unit_Code=="PETE",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="PETE",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="PETE",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="PETE",],
            Vines=InVines[InVines$Unit_Code=="PETE",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="PETE",],
            CWD=InCWD[InCWD$Unit_Code=="PETE",],
            Commons=InCommons)
  
  RICH<-new("NPSForVeg", 
            ParkCode="RICH", 
            ShortName="Richmond Battlefield", 
            LongName="Richmond National Battlefield Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1),
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1),
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="RICH",], 
            Events=InEvents[InEvents$Unit_Code=="RICH",],
    
            Trees=InTrees[InTrees$Unit_Code=="RICH",], 
            Saplings=InSaps[InSaps$Unit_Code=="RICH",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="RICH",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="RICH",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="RICH",],
            Vines=InVines[InVines$Unit_Code=="RICH",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="RICH",],
            CWD=InCWD[InCWD$Unit_Code=="RICH",],
            Commons=InCommons)
  
  SAHI<-new("NPSForVeg", 
            ParkCode="SAHI", 
            ShortName="Sagamore Hill", 
            LongName="Sagamore Hill National Historic Site", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles2,
            
            Plots=InPlots[InPlots$Unit_Code=="SAHI",], 
            Events=InEvents[InEvents$Unit_Code=="SAHI",],
    
            Trees=InTrees[InTrees$Unit_Code=="SAHI",], 
            Saplings=InSaps[InSaps$Unit_Code=="SAHI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="SAHI",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="SAHI",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="SAHI",],
            Vines=InVines[InVines$Unit_Code=="SAHI",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="SAHI",],
            CWD=InCWD[InCWD$Unit_Code=="SAHI",],
            Commons=InCommons)
  
  THST<-new("NPSForVeg", 
            ParkCode="THST", 
            ShortName="Thomas Stone", 
            LongName="Thomas Stone National Historic Site", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles2,
            
            Plots=InPlots[InPlots$Unit_Code=="THST",], 
            Events=InEvents[InEvents$Unit_Code=="THST",],
    
            Trees=InTrees[InTrees$Unit_Code=="THST",], 
            Saplings=InSaps[InSaps$Unit_Code=="THST",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="THST",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="THST",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="THST",],
            Vines=InVines[InVines$Unit_Code=="THST",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="THST",],
            CWD=InCWD[InCWD$Unit_Code=="THST",],
            Commons=InCommons)
  
  VAFO<-new("NPSForVeg", 
            ParkCode="VAFO", 
            ShortName="Valley Forge", 
            LongName="Valley Forge National Historical Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),
            SapPlotSize=c(3, pi*3*3), 
            SeedPlotSize=c(12,1), 
            ShrubPlotSize=c(3, pi*3*3), 
            ShSeedPlotSize=c(12,1), 
            VPlotSize=c(1,20*20),
            HPlotSize=c(12,1),
            Cycles=InCycles,
            
            Plots=InPlots[InPlots$Unit_Code=="VAFO",], 
            Events=InEvents[InEvents$Unit_Code=="VAFO",],
    
            Trees=InTrees[InTrees$Unit_Code=="VAFO",], 
            Saplings=InSaps[InSaps$Unit_Code=="VAFO",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="VAFO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="VAFO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="VAFO",],
            Vines=InVines[InVines$Unit_Code=="VAFO",], 
            Herbs=InHerbs[InHerbs$Unit_Code=="VAFO",],
            CWD=InCWD[InCWD$Unit_Code=="VAFO",],
            Commons=InCommons)
    
  return(c(APCO,BOWA,COLO,FRSP,GETT,GEWA,HOFU,PETE,RICH,SAHI,THST,VAFO))
}
