#' @title importNCRN
#' 
#' @description  This function imports data from the standard NCRN .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, Shrubs, Shrub_Seedlings, Vines, Herbs and CommonNames.
#' 
#' @param Dir  The directory where the data is found. Path should not have a trailing slash.
#' 
#' @return Returns 11 \code{NPSForVeg} objects, one for each park, named using the standard 4 letter park code (e.g. ANTI, CATO etc.) and a list, named NCRN, which contians all 11 objects.
#' 
#' @export
#' 


importNCRN<-function(Dir){
  
  InPlots<-read.csv(paste(Dir,"Plots.csv",sep="/"),as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  
  InEvents<-read.csv(paste(Dir,"Events.csv",sep="/"),as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  
  InTrees<-read.csv(paste(Dir,"Trees.csv",sep="/"),as.is=T, header=T)
  InSaps<-read.csv(paste(Dir,"Saplings.csv",sep="/"),as.is=T, header=T)
  InSeeds<-read.csv(paste(Dir,"Seedlings.csv",sep="/"),as.is=T, header=T)
  InShrubs<-read.csv(paste(Dir,"Shrubs.csv",sep="/"),as.is=T, header=T)
  InShSeeds<-read.csv(paste(Dir,"Shrub_Seedlings.csv",sep="/"),as.is=T, header=T)
  InVines<-read.csv(paste(Dir,"Vines.csv",sep="/"),as.is=T, header=T)
  InHerbs<-read.csv(paste(Dir,"Herbs.csv",sep="/"),as.is=T, header=T)
  InCommons<-read.csv(paste(Dir,"CommonNames.csv",sep="/"), as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""]
  InCommons$TSN<-as.character(InCommons$TSN)

  
 ANTI<-new("NPSForVeg", 
           ParkCode="ANTI", 
           ShortName="Antietam", 
           LongName="Antietam National Battlefield", 
           Network="NCRN", 
           
           TPlotSize=c(1,pi*15*15),
           SapPlotSize=c(3, pi*3*3), 
           SeedPlotSize=c(12,1), 
           ShrubPlotSize=c(3, pi*3*3), 
           ShSeedPlotSize=c(12,1), 
           VPlotSize=c(1,pi*15*15),
           HPlotSize=c(12,1),
           
           Plots=InPlots[InPlots$Unit_Code=="ANTI",], 
           Events=InEvents[InEvents$Unit_Code=="ANTI",],
           
           Trees=InTrees[InTrees$Unit_Code=="ANTI",], 
           Saplings=InSaps[InSaps$Unit_Code=="ANTI",], 
           Seedlings=InSeeds[InSeeds$Unit_Code=="ANTI",], 
           Shrubs=InShrubs[InShrubs$Unit_Code=="ANTI",], 
           ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="ANTI",],
           Vines=InVines[InVines$Unit_Code=="ANTI",], 
           Herbs=InHerbs[InHerbs$Unit_Code=="ANTI",],
           Commons=InCommons)
 
 
 CATO<-new("NPSForVeg", 
           ParkCode="CATO", 
           ShortName="Catoctin", 
           LongName="Catoctin Mountain Park", 
           Network="NCRN", 
           
           TPlotSize=c(1,pi*15*15),
           SapPlotSize=c(3, pi*3*3), 
           SeedPlotSize=c(12,1), 
           ShrubPlotSize=c(3, pi*3*3), 
           ShSeedPlotSize=c(12,1), 
           VPlotSize=c(1,pi*15*15),
           HPlotSize=c(12,1),
           
           Plots=InPlots[InPlots$Unit_Code=="CATO",], 
           Events=InEvents[InEvents$Unit_Code=="CATO",],
           
           Trees=InTrees[InTrees$Unit_Code=="CATO",], 
           Saplings=InSaps[InSaps$Unit_Code=="CATO",], 
           Seedlings=InSeeds[InSeeds$Unit_Code=="CATO",], 
           Shrubs=InShrubs[InShrubs$Unit_Code=="CATO",], 
           ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="CATO",],
           Vines=InVines[InVines$Unit_Code=="CATO",], 
           Herbs=InHerbs[InHerbs$Unit_Code=="CATO",],
           Commons=InCommons)

CHOH<-new("NPSForVeg", 
          ParkCode="CHOH", 
          ShortName="C&O Canal", 
          LongName="Chesapeake & Ohio Canal National Historical Park", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3, pi*3*3), 
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3, pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="CHOH",], 
          Events=InEvents[InEvents$Unit_Code=="CHOH",],
          
          Trees=InTrees[InTrees$Unit_Code=="CHOH",], 
          Saplings=InSaps[InSaps$Unit_Code=="CHOH",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="CHOH",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="CHOH",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="CHOH",],
          Vines=InVines[InVines$Unit_Code=="CHOH",], 
          Herbs=InHerbs[InHerbs$Unit_Code=="CHOH",],
          Commons=InCommons)

GWMP<-new("NPSForVeg", 
          ParkCode="GWMP", 
          ShortName="GW Parkway", 
          LongName="George Washington Memorial Parkway", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3, pi*3*3), 
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3, pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="GWMP",], 
          Events=InEvents[InEvents$Unit_Code=="GWMP",],
          
          Trees=InTrees[InTrees$Unit_Code=="GWMP",], 
          Saplings=InSaps[InSaps$Unit_Code=="GWMP",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="GWMP",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="GWMP",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="GWMP",],
          Vines=InVines[InVines$Unit_Code=="GWMP",], 
          Herbs=InHerbs[InHerbs$Unit_Code=="GWMP",],
          Commons=InCommons)

HAFE<-new("NPSForVeg", 
          ParkCode="HAFE", 
          ShortName="Harpers Ferry", 
          LongName="Harpers Ferry National Historical Park", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3, pi*3*3), 
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3, pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="HAFE",], 
          Events=InEvents[InEvents$Unit_Code=="HAFE",],
          
          Trees=InTrees[InTrees$Unit_Code=="HAFE",], 
          Saplings=InSaps[InSaps$Unit_Code=="HAFE",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="HAFE",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="HAFE",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="HAFE",],
          Vines=InVines[InVines$Unit_Code=="HAFE",], 
          Herbs=InHerbs[InHerbs$Unit_Code=="HAFE",],
          Commons=InCommons)

MANA<-new("NPSForVeg",
          ParkCode="MANA",
          ShortName="Manassas",
          LongName="Manassas National Battlefield Park", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="MANA",],
          Events=InEvents[InEvents$Unit_Code=="MANA",], 
          
          Trees=InTrees[InTrees$Unit_Code=="MANA",], 
          Saplings=InSaps[InSaps$Unit_Code=="MANA",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="MANA",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="MANA",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="MANA",],
          Vines=InVines[InVines$Unit_Code=="MANA",],
          Herbs=InHerbs[InHerbs$Unit_Code=="MANA",],
          Commons=InCommons) 

MONO<-new("NPSForVeg",
          ParkCode="MONO",
          ShortName="Monocacy",
          LongName="Monocacy National Battlefield", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="MONO",],
          Events=InEvents[InEvents$Unit_Code=="MONO",], 
          
          Trees=InTrees[InTrees$Unit_Code=="MONO",], 
          Saplings=InSaps[InSaps$Unit_Code=="MONO",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="MONO",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="MONO",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="MONO",],
          Vines=InVines[InVines$Unit_Code=="MONO",],
          Herbs=InHerbs[InHerbs$Unit_Code=="MONO",],
          Commons=InCommons) 

NACE<-new("NPSForVeg",
          ParkCode="NACE",
          ShortName="Nat.Cap.Parks - East",
          LongName="National Captial Parks-East", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="NACE",],
          Events=InEvents[InEvents$Unit_Code=="NACE",], 
          
          Trees=InTrees[InTrees$Unit_Code=="NACE",], 
          Saplings=InSaps[InSaps$Unit_Code=="NACE",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="NACE",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="NACE",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="NACE",],
          Vines=InVines[InVines$Unit_Code=="NACE",],
          Herbs=InHerbs[InHerbs$Unit_Code=="NACE",],
          Commons=InCommons) 

PRWI<-new("NPSForVeg",
          ParkCode="PRWI",
          ShortName="Prince William",
          LongName="Prince William Forest Park", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="PRWI",],
          Events=InEvents[InEvents$Unit_Code=="PRWI",],
          
          Trees=InTrees[InTrees$Unit_Code=="PRWI",], 
          Saplings=InSaps[InSaps$Unit_Code=="PRWI",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="PRWI",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="PRWI",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="PRWI",],
          Vines=InVines[InVines$Unit_Code=="PRWI",],
          Herbs=InHerbs[InHerbs$Unit_Code=="PRWI",],
          Commons=InCommons) 

ROCR<-new("NPSForVeg",
          ParkCode="ROCR",
          ShortName="Rock Creek",
          LongName="Rock Creek Park", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="ROCR",],
          Events=InEvents[InEvents$Unit_Code=="ROCR",], 
          
          Trees=InTrees[InTrees$Unit_Code=="ROCR",], 
          Saplings=InSaps[InSaps$Unit_Code=="ROCR",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="ROCR",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="ROCR",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="ROCR",],
          Vines=InVines[InVines$Unit_Code=="ROCR",],
          Herbs=InHerbs[InHerbs$Unit_Code=="ROCR",],
          Commons=InCommons) 

WOTR<-new("NPSForVeg",
          ParkCode="WOTR",
          ShortName="Wolf Trap",
          LongName="Wolf Trap National Park for the Performing Arts", 
          Network="NCRN", 
          
          TPlotSize=c(1,pi*15*15),
          SapPlotSize=c(3,pi*3*3),
          SeedPlotSize=c(12,1), 
          ShrubPlotSize=c(3,pi*3*3), 
          ShSeedPlotSize=c(12,1), 
          VPlotSize=c(1,pi*15*15),
          HPlotSize=c(12,1),
          
          Plots=InPlots[InPlots$Unit_Code=="WOTR",],
          Events=InEvents[InEvents$Unit_Code=="WOTR",], 
          
          Trees=InTrees[InTrees$Unit_Code=="WOTR",], 
          Saplings=InSaps[InSaps$Unit_Code=="WOTR",], 
          Seedlings=InSeeds[InSeeds$Unit_Code=="WOTR",], 
          Shrubs=InShrubs[InShrubs$Unit_Code=="WOTR",], 
          ShSeedlings=InShSeeds[InShSeeds$Unit_Code=="WOTR",],
          Vines=InVines[InVines$Unit_Code=="WOTR",],
          Herbs=InHerbs[InHerbs$Unit_Code=="WOTR",],
          Commons=InCommons)

  
  return(c(ANTI,CATO,CHOH,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
}

