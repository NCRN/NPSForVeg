
#' @title importERMN
#' 
#' @description  This function imports data from the standard ERMN .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, ERMNSoil,SoilChemistryVariables,and CommonNames.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns a list containing 8 \code{NPSForVeg} objects, one for each park, named using the standard 4 letter park code (e.g. ALPO, BLUE etc.) as part of a list.
#' 
#' @export
#' 


importERMN<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)  
  
  InPlots<-read.csv("Plots.csv",as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  
  InEvents<-read.csv("Events.csv",as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  InSoils<-read.csv("ERMNSoil.csv",as.is=T,header=T)
  InSoils$Sample_Date<-as.Date(as.character(InSoils$Date),format="%Y%m%d") #Add new slot for Soils
  InChemVars<-read.csv("SoilChemistryVariables.csv",as.is=T,header=T)
  
  InTrees<-read.csv("Trees.csv",as.is=T, header=T)
  InSaps<-read.csv("Saplings.csv",as.is=T, header=T)
  InSeeds<-read.csv("Seedlings.csv",as.is=T, header=T)
  InCommons<-read.csv("CommonNames.csv", as.is=T, header=T)
  InCommons$Common[InCommons$NCRN_Common!=""]<-InCommons$NCRN_Common[InCommons$NCRN_Common!=""] ##maybe change this to ERMN?
  InCommons$TSN<-as.character(InCommons$TSN)
  setwd(OldDir)
  
  
  ALPO<-new("NPSForVeg", 
            ParkCode="ALPO", 
            ShortName="Allegheny Portage", 
            LongName="Allegheny Portage Railroad National Historic Site", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="ALPO",], 
            Events=InEvents[InEvents$Unit_Code=="ALPO",],
            Trees=InTrees[InTrees$Unit_Code=="ALPO",], 
            Saplings=InSaps[InSaps$Unit_Code=="ALPO",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="ALPO",], 
            Soils=InSoils[InSoils$Unit_Code=="ALPO",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  BLUE<-new("NPSForVeg", 
            ParkCode="BLUE", 
            ShortName="Bluestone", 
            LongName="Bluestone National Scenic River", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="BLUE",], 
            Events=InEvents[InEvents$Unit_Code=="BLUE",],
            
            Trees=InTrees[InTrees$Unit_Code=="BLUE",], 
            Saplings=InSaps[InSaps$Unit_Code=="BLUE",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="BLUE",], 
            Soils=InSoils[InSoils$Unit_Code=="BLUE",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  DEWA<-new("NPSForVeg", 
            ParkCode="DEWA", 
            ShortName="Delaware Water Gap", 
            LongName="Delaware Water Gap National Recreation Area", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="DEWA",], 
            Events=InEvents[InEvents$Unit_Code=="DEWA",],
            
            Trees=InTrees[InTrees$Unit_Code=="DEWA",], 
            Saplings=InSaps[InSaps$Unit_Code=="DEWA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="DEWA",], 
            Soils=InSoils[InSoils$Unit_Code=="DEWA",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  FONE<-new("NPSForVeg", 
            ParkCode="FONE", 
            ShortName="Fort Necessity", 
            LongName="Fort Necessity National Battlefield", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="FONE",], 
            Events=InEvents[InEvents$Unit_Code=="FONE",],
            
            Trees=InTrees[InTrees$Unit_Code=="FONE",], 
            Saplings=InSaps[InSaps$Unit_Code=="FONE",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="FONE",], 
            Soils=InSoils[InSoils$Unit_Code=="FONE",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  FRHI<-new("NPSForVeg", 
            ParkCode="FRHI", 
            ShortName="Friendship Hill", 
            LongName="Friendship Hill National Historic Site", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="FRHI",], 
            Events=InEvents[InEvents$Unit_Code=="FRHI",],
            
            Trees=InTrees[InTrees$Unit_Code=="FRHI",], 
            Saplings=InSaps[InSaps$Unit_Code=="FRHI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="FRHI",], 
            Soils=InSoils[InSoils$Unit_Code=="FRHI",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  GARI<-new("NPSForVeg",
            ParkCode="GARI",
            ShortName="Gauley",
            LongName="Gauley River National Recreation Area", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="GARI",],
            Events=InEvents[InEvents$Unit_Code=="GARI",], 
            
            Trees=InTrees[InTrees$Unit_Code=="GARI",], 
            Saplings=InSaps[InSaps$Unit_Code=="GARI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="GARI",], 
            Soils=InSoils[InSoils$Unit_Code=="GARI",],
            ChemVars=InChemVars,
            Commons=InCommons) 
  
  JOFL<-new("NPSForVeg",
            ParkCode="JOFL",
            ShortName="Johnstown Flood",
            LongName="Johnstown Flood National Memorial", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="JOFL",],
            Events=InEvents[InEvents$Unit_Code=="JOFL",], 
            
            Trees=InTrees[InTrees$Unit_Code=="JOFL",],  
            Saplings=InSaps[InSaps$Unit_Code=="JOFL",],
            Seedlings=InSeeds[InSeeds$Unit_Code=="JOFL",], 
            Soils=InSoils[InSoils$Unit_Code=="JOFL",],
            ChemVars=InChemVars,
            Commons=InCommons) 
  
  NERI<-new("NPSForVeg",
            ParkCode="NERI",
            ShortName="New River",
            LongName="New River Gorge National River", 
            Network="ERMN", 
            
            TPlotSize=c(1,pi*15*15),
            SapPlotSize=c(4, pi*2*2), 
            SeedPlotSize=c(4, pi*2*2), 
            HPlotSize=c(12,1),
            
            Plots=InPlots[InPlots$Unit_Code=="NERI",],
            Events=InEvents[InEvents$Unit_Code=="NERI",], 
            
            Trees=InTrees[InTrees$Unit_Code=="NERI",], 
            Saplings=InSaps[InSaps$Unit_Code=="NERI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="NERI",], 
            Soils=InSoils[InSoils$Unit_Code=="NERI",],
            ChemVars=InChemVars,
            Commons=InCommons) 
  
  
  return(c(ALPO,BLUE,DEWA,FONE,FRHI,GARI,JOFL,NERI))
}
