#' @title IVplot
#' 
#' @importFrom lattice barchart strip.custom
#' @importFrom  data.table rbindlist
#' 
#' @description Makes a barchart of IV values
#' 
#' @param object Either a \code{data.frame} which is the output of \code{\link{IV}}, or an object of class \code{NPSForVeg}, or a \code{list} of objects of class \code{NPSForVeg}.
#' @param top  Defaults to \code{NA}. An integer. Indicates the number of species that should be included from each dataset in the graph (see \code{compare}). For a single dataset the species with the largest total IV values are selected. When multiple datasets are compared, the top species in each dataset are selected, which can result in more species than \code{top} included in the final graph - e.g. if \code{top=3} and two datasets are used,  up to six species can be included in the graph, depeding on the amount of overlap it the top three species. If \code{top=NA} all species are displayed.  
#' @param IVargs A required \code{list}, defaults to \code{NA}. The arguments in this list are passed to \code{\link{IV}} and are used to generate the data for the graph. Any argument that is valid for \code{IV} can be in this list, and should be written in arguement=value format e.g. \code{IVargs=list(group="trees", years=c(2010:2013))}
#' @param parts Logical value, defaults to \code{FALSE}. Indicates if the components of IV (Density, Size and Distribution) should be shown on the graph. 
#' @param compare A \code{list} of lists. Defaults to \code{NA}. This can be used to compare the IV values from several different datasets, such as trees from different parks, different time periods,  or trees, sapings and seedlings from the same park. Each list will be passed on to \code{\link{IV}}, and can contain any arguments that are valid for that function. Entries in the list should be written in argument=value format  e.g. \code{compare=list( list(object=CATO, group="saplings", years=c(2010:2013)), list(object=CATO, group="seedlings", years=c(2010:2013)) )}. Any number of datasets can be added using \code{compare}.
#' @param labels A character vector, defaults to \code{NA}. Labels for each dataset, only used when \code{compare} is not \code{NA}
#' @param colors Colors for the graph.
#' @param ... Other arguments passed to \code{barchart} in the \pkg{lattice} package.
#' 
#' @details This function uses the \code{barchart} function in the \pkg{lattice} package to create a horizontal barchart that displays IV values. The format of the display varies depending on the type of data to be displayed. If only a single dataset is considered (\code{compare=NA}), and \code{parts=FALSE}, then a simple horizontal bar chart is produced. If \code{parts=TRUE} a stacked bar chart is produced. Each component of IV: density, size and distribution, is shown in a different color. 
#' 
#' If two or more datasets are used, and \code{parts=FALSE} then all results are graphed in a single panel. Each dataset is a different color, and the legend uses the contents of \code{labels} to identify each dataset. If \code{parts=TRUE} then each dataset is plotted in a separate panel, and the panel title is determined by \code{labels}. Within each panel, the three components of IV will each have a separate color, which will be consistent between panels. 
#' 
#' @return Returns a barchart 
#' 
#' @seealso \code{\link{IV}}
#' 
#' @include NPSForVeg_Class_def.R
#' @export


setGeneric(name="IVplot",function(object,top=NA,IVargs=NA,parts=FALSE,compare=NA,labels=NA, colors=c("darkgreen","green","yellow"),...){standardGeneric("IVplot")}, signature="object")


setMethod(f="IVplot", signature=c(object="data.frame"),
          function(object,...){
            
            ifelse(is.null(object$which),IVdata<-object[order(-object$Total),], IVdata<-object[order(object$which,-object$Total),] )
            
            SpeciesUse<-c()
            if (!is.na(top)) {
              ifelse(is.null(IVdata$which),
                             SpeciesUse<-IVdata$Species[order(-IVdata$Total)][1:top],
                             SpeciesUse<-unique(unlist(tapply(IVdata$Species, INDEX=IVdata$which, FUN="[",i=1:top))))
              IVdata<-IVdata[IVdata$Species %in% SpeciesUse,]
            }
           IVdata$Species<-factor(IVdata$Species, levels=unique(IVdata$Species))
           IVdata$Species<-factor(IVdata$Species, levels=rev(levels(IVdata$Species))) 
           Result<-"Species" 
           Preds<-ifelse(parts, "Density+Size+Distribution", "Total")
           
           if(!is.null(IVdata$which) & parts){Preds<-paste(Preds,"which",sep="|")}
            
            BarForm<-formula(paste(Result,Preds,sep="~"))
              barchart(
                as.table=T,
                x=BarForm,
                data=IVdata,
                groups=if(!is.null(IVdata$which) & !parts){which<-factor(which, levels=rev(levels(which)))},
                stack=parts,
                horizontal=T,
                xlab=NULL,
                xlim=c(-.01, max((IVdata$Total)+.05)),
                strip=strip.custom(factor.levels=labels,bg="wheat"),
                col = if(parts){colors} else (rev(colors)),
                scales=list(alternating=1, tck=c(1,0),x=list(at=c(0,max(IVdata$Total)),labels=c("Low","High")),font=2),
                key={
                  if(parts) {list(space="bottom", 
                                  text=list(labels=c("Density","Size","Distribution")), 
                                  rectangles=list(col=colors),
                                  columns=3)} else
                 if(!parts & all(!is.na(compare)) ) {list(space="bottom", 
                                                          text=list(labels=labels), 
                                                          rectangles=list(col=colors),
                                                          columns=min(4,length(labels))) }
                } ,
                ...
              )
})

setMethod(f="IVplot",signature=c("NPSForVeg"),
           function(object,top,IVargs,parts,...){
              IV2<-list()
              TempIV<-do.call(IV, c(object,IVargs))
            
            if(all(!is.na(compare))){
              IV2[[1]]<-TempIV
              IV2[[1]]$which=1
                for(i in seq_along(compare) ){
                  IV2[[i+1]]<-do.call(IV,compare[[i]]) 
                  names(IV2)[i+1]<-names(compare[i])
                  IV2[[i+1]]$which=i+1
                }
              TempIV<-rbindlist(IV2)
              TempIV$which<-factor(TempIV$which)
            }
            
            IVplot(object=TempIV,top=top,parts=parts,labels=labels,compare=compare,colors=colors,...)
  })


setMethod(f="IVplot",signature=c("list"),
          function(object,top,IVargs,parts,...){

            TempIVPl<-make(object=object,ParkCode="TEMPDENS",ShortName="Temporary Park",LongName="Temp object made by dens()",
                           Network="TEMPDENSNET")
            IVplot(object=TempIVPl, IVargs=IVargs, top=top, parts=parts, compare=compare, labels=labels, colors=colors, ...)
          })

