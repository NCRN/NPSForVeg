#' @title make
#' 
#' @description makes a new \code{NPSForVeg} object from one or more existing objects
#' 
#' @param object Either an \code{NPSForVeg} object or a list of such objects
#' @param ParkCode The parkcode for the new \code{NPSForVeg} object
#' @param ShortName The short name for the new \code{NPSForVeg} object
#' @param LongName  The long name for the new \code{NPSForVeg} object 
#' @param Network  The network code for the new \code{NPSForVeg} object
#' @param plots A character vector with plot names. When specified, only data from the given plots will be included in the new \code{NPSForVeg} Object.
#' 
#' @details This function creates new \code{NPSForVeg} objects by combining two or more previously existing objects and/or by subsetting existing objects. If more than one object is provided then the data for these objects is combined. The areas associated with the plots (eg. the size and number of sapling microplots) is taken from the first object. WARNING:  If the objects have different sized plots then area based calcuations will be in error. Providing a \code{plots} argument will indicate which plots are in the new object. The user must indicate the new network code, park code and park names. 
#' 
#' @export



setGeneric(name="make",function(object,ParkCode,ShortName,LongName,Network,plots=NA,...){standardGeneric("make")}, signature="object")

setMethod(f='make', signature=c(object="list"),
      function(object,...){
        if( all( sapply(X=object,FUN=class)=="NPSForVeg" ) ){
          
          TrSize=c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"trees","count")==getArea(object[1],"trees","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"trees","count"))
             )  {TrSize[1]=getArea(object[1],"trees","count")}
          
          if(all(lapply(X=object,FUN=getArea,"trees","single")==getArea(object[1],"trees","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"trees","single"))
             ) {TrSize[2]=getArea(object[1],"trees","single")}
        
          SpSize=c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"saplings","count")==getArea(object[1],"saplings","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"saplings","count"))
             ){SpSize[1]=getArea(object[1],"saplings","count")}
          
          if(all(lapply(X=object,FUN=getArea,"saplings","single")==getArea(object[1],"saplings","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"saplings","single"))
             ){SpSize[2]=getArea(object[1],"saplings","single")}
              
          SdSize=c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"seedlings","count")==getArea(object[1],"seedlings","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"seedlings","count"))
             ) {SdSize[1]=getArea(object[1],"seedlings","count")}
          
          if(all(lapply(X=object,FUN=getArea,"seedlings","single")==getArea(object[1],"seedlings","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"seedlings","single"))
             ) {SdSize[2]=getArea(object[1],"seedlings","single")}
          
          ShSize<-c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"shrubs","count")==getArea(object[1],"shrubs","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"shrubs","count"))
             ) {ShSize[1]=getArea(object[1],"shrubs","count")}
          
          if(all(lapply(X=object,FUN=getArea,"shrubs","single")==getArea(object[1],"shrubs","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"shrubs","single"))
             ) {ShSize[2]=getArea(object[1],"shrubs","single")}
          
          ShSdSize<-c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"shseedlings","count")==getArea(object[1],"shseedlings","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"shseedlings","count"))
             ) {ShSdSize[1]=getArea(object[1],"shseedlings","count")}
          
          if(all(lapply(X=object,FUN=getArea,"shseedlings","single")==getArea(object[1],"shseedlings","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"shseedlings","single"))
             ) {ShSdSize[2]=getArea(object[1],"shseedlings","single")}
          
          VSize=c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"vines","count")==getArea(object[1],"vines","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"vines","count"))
             ){VSize[1]=getArea(object[1],"vines","count")}
          
          if(all(lapply(X=object,FUN=getArea,"vines","single")==getArea(object[1],"vines","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"vines","single"))
             ){VSize[2]=getArea(object[1],"vines","single")}
          
          HSize=c(NaN,NaN)
          if(all(lapply(X=object,FUN=getArea,"herbs","count")==getArea(object[1],"herbs","count")) &
             !anyNA(lapply(X=object,FUN=getArea,"herbs","count"))
             ){HSize[1]=getArea(object[1],"herbs","count")}
          
          if(all(lapply(X=object,FUN=getArea,"herbs","single")==getArea(object[1],"herbs","single")) &
             !anyNA(lapply(X=object,FUN=getArea,"herbs","single"))
             ) {HSize[2]=getArea(object[1],"herbs","single")}
          
          new("NPSForVeg",
              ParkCode=ParkCode,
              ShortName=ShortName,
              LongName=LongName,
              
              Network=Network,
              
              TPlotSize=TrSize,
              SapPlotSize=SpSize,
              SeedPlotSize=SdSize,
              ShrubPlotSize=ShSize,
              ShSeedPlotSize=ShSdSize,
              VPlotSize=VSize,
              HPlotSize=HSize,
              
              Plots=getPlots(object,type="all", plots=plots, output="dataframe"),
              Events=getEvents(object, plots=plots, output="dataframe"),
              Trees=getPlants(object,group="trees", status="all", plots=plots, output="dataframe"),
              Saplings=getPlants(object,group="saplings", status="all", plots=plots, output="dataframe"),
              Seedlings=getPlants(object,group="seedlings", status="all", plots=plots, output="dataframe"),
              Shrubs=getPlants(object,group="shrubs", status="all", plots=plots, output="dataframe"),
              ShSeedlings=getPlants(object,group="shseedlings", status="all", plots=plots, output="dataframe"),
              Vines=getPlants(object,group="vines", status="all", plots=plots, output="dataframe"),
              Herbs=getPlants(object,group="herbs", status="all",plots=plots, output="dataframe"),
              Commons=getCommons(object)
          )
          
          
        }
  })

setMethod(f='make', signature=c(object="NPSForVeg"),
          function(object,...){


            
            new("NPSForVeg",
              ParkCode=ParkCode,
              ShortName=ShortName,
              LongName=LongName,
  
              Network=Network,
  
              TPlotSize=c(getArea(object,"trees","count"),getArea(object,"trees","single")),
              SapPlotSize=c(getArea(object,"saplings","count"),getArea(object,"saplings","single")),
              SeedPlotSize=c(getArea(object,"seedlings","count"),getArea(object,"seedlings","single")),
              ShrubPlotSize=c(getArea(object,"shrubs","count"),getArea(object,"shrubs","single")),
              ShSeedPlotSize=c(getArea(object,"shseedlings","count"),getArea(object,"shseedlings","single")),
  
                Plots=getPlots(object,type="all",plots=plots),
                Events=getEvents(object,plots=plots),
                Trees=getPlants(object,group="trees", status="all", plots=plots),
                Saplings=getPlants(object,group="saplings", status="all", plots=plots),
                Seedlings=getPlants(object,group="seedlings", status="all", plots=plots),
                Shrubs=getPlants(object,group="shrubs", status="all", plots=plots),
                ShSeedlings=getPlants(object,group="shseedlings", status="all",plots=plots),
                Vines=getPlants(object,group="vines", status="all",plots=plots),
                Herbs=getPlants(object,group="herbs", status="all",plots=plots),
                Commons=getCommons(object)                                                 
            )  
})