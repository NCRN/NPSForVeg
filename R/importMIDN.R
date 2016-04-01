#' @title importMIDN
#' 
#' @description  This function imports data from the standard MIDN .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, Vines, MIDNSoil, SoilChemistryVariables, and CommonNames.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns a list with 12 \code{NPSForVeg} objects, one for each park.
#' 
#' @export
#' 

importMIDN<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)  
  
  InPlots<-read.csv("Plots.csv",as.is=T, header=T)
  InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
  InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
  InPlots$Plot_Name<-as.character(InPlots$Plot_Name)
  
  InEvents<-read.csv("Events.csv",as.is=T, header=T)
  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  InSoils<-read.csv("MIDNSoil.csv",as.is=T,header=T)
  InSoils$Sample_Date<-as.Date(as.character(InSoils$Date),format="%Y%m%d") #Add new slot for Soils
  InChemVars<-read.csv("SoilChemistryVariables.csv",as.is=T,header=T)
  
  InTrees<-read.csv("Trees.csv",as.is=T, header=T)
  InSaps<-read.csv("Saplings.csv",as.is=T, header=T)
  #InGrowthRates<-read.csv("treecalc.csv",as.is=T, header=T)
  InSeeds<-read.csv("Seedlings.csv",as.is=T, header=T)#  each row is individual seedling with height and browse
  #InSeedsQuad<-read.csv("Seedlings_Quad.csv",as.is=T, header=T)# quadrat level summary of tree seedlings
  InVines<-read.csv("Vines.csv",as.is=T, header=T)
  InCommons<-read.csv("CommonNames.csv", as.is=T, header=T)
  InCommons$TSN<-as.character(InCommons$TSN)
  setwd(OldDir)
  
  APCO<-new("NPSForVeg", 
            ParkCode="APCO", 
            ShortName="Appomattox", 
            LongName="Appomattox Court House National Historical Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="APCO",], 
            Events=InEvents[InEvents$Unit_Code=="APCO",],
            
            Trees=InTrees[InTrees$Unit_Code=="APCO",], 
            Saplings=InSaps[InSaps$Unit_Code=="APCO",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="APCO",],
            Seedlings=InSeeds[InSeeds$Unit_Code=="APCO",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="APCO",],
            Vines=InVines[InVines$Unit_Code=="APCO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="APCO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="APCO",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="APCO",],
            Soils=InSoils[InSoils$Unit_Code=="APCO",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  
  BOWA<-new("NPSForVeg", 
            ParkCode="BOWA", 
            ShortName="Booker T", 
            LongName="Booker T Washington National Monument", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="BOWA",], 
            Events=InEvents[InEvents$Unit_Code=="BOWA",],
            
            Trees=InTrees[InTrees$Unit_Code=="BOWA",], 
            Saplings=InSaps[InSaps$Unit_Code=="BOWA",],
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="BOWA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="BOWA",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="BOWA",],
            Vines=InVines[InVines$Unit_Code=="BOWA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="BOWA",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="BOWA",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="BOWA",],
            Soils=InSoils[InSoils$Unit_Code=="BOWA",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  COLO<-new("NPSForVeg", 
            ParkCode="COLO", 
            ShortName="Colonial", 
            LongName="Colonial National Historical Park", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="COLO",], 
            Events=InEvents[InEvents$Unit_Code=="COLO",],
            
            Trees=InTrees[InTrees$Unit_Code=="COLO",], 
            Saplings=InSaps[InSaps$Unit_Code=="COLO",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="COLO",] ,
            Seedlings=InSeeds[InSeeds$Unit_Code=="COLO",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="COLO",],
            Vines=InVines[InVines$Unit_Code=="COLO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="COLO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="COLO",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="COLO",],
            Soils=InSoils[InSoils$Unit_Code=="COLO",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  
  
  FRSP<-new("NPSForVeg", 
            ParkCode="FRSP", 
            ShortName="Fred Spotsy MP", 
            LongName="Fredericksburg and Spotsylvania National Military Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="FRSP",], 
            Events=InEvents[InEvents$Unit_Code=="FRSP",],
            
            Trees=InTrees[InTrees$Unit_Code=="FRSP",], 
            Saplings=InSaps[InSaps$Unit_Code=="FRSP",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="FRSP",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="FRSP",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="FRSP",],
            Vines=InVines[InVines$Unit_Code=="FRSP",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="FRSP",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="FRSP",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="FRSP",],
            Soils=InSoils[InSoils$Unit_Code=="FRSP",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  GETT<-new("NPSForVeg", 
            ParkCode="GETT", 
            ShortName="Gettysburg", 
            LongName="Gettysburg National Military Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="GETT",], 
            Events=InEvents[InEvents$Unit_Code=="GETT",],
            
            Trees=InTrees[InTrees$Unit_Code=="GETT",], 
            Saplings=InSaps[InSaps$Unit_Code=="GETT",],
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="GETT",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="GETT",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="GETT",],
            Vines=InVines[InVines$Unit_Code=="GETT",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="GETT",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="GETT",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="GETT",],
            Soils=InSoils[InSoils$Unit_Code=="GETT",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  
  GEWA<-new("NPSForVeg", 
            ParkCode="GEWA", 
            ShortName="Washington Birthplace", 
            LongName="George Washington Birthplace National Monument", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="GEWA",], 
            Events=InEvents[InEvents$Unit_Code=="GEWA",],
            
            Trees=InTrees[InTrees$Unit_Code=="GEWA",], 
            Saplings=InSaps[InSaps$Unit_Code=="GEWA",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="GEWA",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="GEWA",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="GEWA",],
            Vines=InVines[InVines$Unit_Code=="GEWA",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="ANTI",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="GEWA",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="GEWA",],
            Soils=InSoils[InSoils$Unit_Code=="GEWA",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  HOFU<-new("NPSForVeg", 
            ParkCode="HOFU", 
            ShortName="Hopewell Furnace", 
            LongName="Hopewell Furnace National Historic Site", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="HOFU",], 
            Events=InEvents[InEvents$Unit_Code=="HOFU",],
            
            Trees=InTrees[InTrees$Unit_Code=="HOFU",], 
            Saplings=InSaps[InSaps$Unit_Code=="HOFU",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="HOFU",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="HOFU",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="HOFU",],
            Vines=InVines[InVines$Unit_Code=="HOFU",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="HOFU",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="HOFU",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="HOFU",],
            Soils=InSoils[InSoils$Unit_Code=="HOFU",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  PETE<-new("NPSForVeg", 
            ParkCode="PETE", 
            ShortName="Petersburg Battlefield", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="PETE",], 
            Events=InEvents[InEvents$Unit_Code=="PETE",],
            
            Trees=InTrees[InTrees$Unit_Code=="PETE",], 
            Saplings=InSaps[InSaps$Unit_Code=="PETE",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="PETE",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="PETE",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="PETE",],
            Vines=InVines[InVines$Unit_Code=="PETE",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="PETE",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="PETE",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="PETE",],
            Soils=InSoils[InSoils$Unit_Code=="PETE",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  
  RICH<-new("NPSForVeg", 
            ParkCode="RICH", 
            ShortName="Richmond Battlefield", 
            LongName="Richmond National Battlefield Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="RICH",], 
            Events=InEvents[InEvents$Unit_Code=="RICH",],
            
            Trees=InTrees[InTrees$Unit_Code=="RICH",], 
            Saplings=InSaps[InSaps$Unit_Code=="RICH",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="RICH",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="RICH",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="RICH",],
            Vines=InVines[InVines$Unit_Code=="RICH",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="RICH",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="RICH",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="RICH",],
            Soils=InSoils[InSoils$Unit_Code=="RICH",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  
  SAHI<-new("NPSForVeg", 
            ParkCode="SAHI", 
            ShortName="Sagamore Hill", 
            LongName="Sagamore Hill National Historic Site", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="SAHI",], 
            Events=InEvents[InEvents$Unit_Code=="SAHI",],
            
            Trees=InTrees[InTrees$Unit_Code=="SAHI",], 
            Saplings=InSaps[InSaps$Unit_Code=="SAHI",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="SAHI",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="SAHI",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="SAHI",],
            Vines=InVines[InVines$Unit_Code=="SAHI",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="SAHI",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="SAHI",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="SAHI",],
            Soils=InSoils[InSoils$Unit_Code=="SAHI",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  THST<-new("NPSForVeg", 
            ParkCode="THST", 
            ShortName="Thomas Stone", 
            LongName="Thomas Stone National Historic Site", 
            Network="NCBN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="THST",], 
            Events=InEvents[InEvents$Unit_Code=="THST",],
            
            Trees=InTrees[InTrees$Unit_Code=="THST",], 
            Saplings=InSaps[InSaps$Unit_Code=="THST",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="THST",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="THST",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="THST",],
            Vines=InVines[InVines$Unit_Code=="THST",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="THST",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="THST",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="THST",],
            Soils=InSoils[InSoils$Unit_Code=="THST",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  VAFO<-new("NPSForVeg", 
            ParkCode="VAFO", 
            ShortName="Valley Forge", 
            LongName="Valley Forge National Historical Park", 
            Network="MIDN", 
            
            TPlotSize=c(1,20*20),# whole plot
            SapPlotSize=c(3, pi*3*3), #microplots
            SeedPlotSize=c(12,1), #quadrats
            ShrubPlotSize=c(12,1), #quadrats
            ShSeedPlotSize=c(12,1), #quadrats
            VPlotSize=c(1,20*20),# Not sure right now 
            HPlotSize=c(12,1),#quadrats
            
            Plots=InPlots[InPlots$Unit_Code=="VAFO",], 
            Events=InEvents[InEvents$Unit_Code=="VAFO",],
            
            Trees=InTrees[InTrees$Unit_Code=="VAFO",], 
            Saplings=InSaps[InSaps$Unit_Code=="VAFO",], 
            #Growthrates=InGrowthRates[InGrowthRates$Unit_Code=="VAFO",], 
            Seedlings=InSeeds[InSeeds$Unit_Code=="VAFO",], 
            #SeedlingsQuad=InSeedsQuad[InSeedsQuad$Unit_Code=="VAFO",],
            Vines=InVines[InVines$Unit_Code=="VAFO",], 
            #Shrubs=InShrubs[InShrubs$Unit_Code=="VAFO",], 
            #ShSeedlings=InShSeeds[InShSeeds$ Unit_Code=="VAFO",],
            #Herbs=InHerbs[InHerbs$Unit_Code=="VAFO",],
            Soils=InSoils[InSoils$Unit_Code=="VAFO",],
            ChemVars=InChemVars,
            Commons=InCommons)
  
  return(c(APCO,BOWA,COLO,FRSP,GETT,GEWA,HOFU,PETE,RICH,SAHI,THST,VAFO))
}
