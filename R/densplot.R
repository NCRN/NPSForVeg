#' @title densplot
#' 
#' @importFrom  lattice panel.arrows panel.xyplot xyplot
#' @importFrom  data.table rbindlist
#' 
#' @description Plots the mean and 95\% confidence intervals of abundance, size and distribution of species in an object of class \code{NPSForVeg}.
#' 
#' @param object Either a \code{data.frame} which is the output of the \code{dens} funciton, an object of class \code{NPSForVeg} or a \code{list} of such objects.
#' @param Total Logical, defaults to \code{TRUE}. Indicates if the the total values for all species, should be plotted. 
#' @param top Numeric, defaults to \code{NA}. When not \code{NA} only the number of species indicated in \code{top} will be used for each dataset (see \code{compare}). If several datasets are used, then more than \code{top} species can be plotted, if the most common species are not the same in all datasets.
#' @param densargs A required \code{list}, defaults to \code{NA}. The arguments in this list are passed to \code{\link{dens}} and are used to generate the data for the graph. Any argument that is valid for \code{dens} can be in this list, and should be written in argument=value format e.g. \code{densargs=list(group="trees", years=c(2010:2013))}
#' @param compare A \code{list} of lists. Defaults to \code{NA}. This can be used to compare the dens values from several different data sets, such as trees from different parks, different time periods, or trees, saplings and seedlings from the same park. Each list will be passed on to \code{\link{dens}}, and can contain any arguments that are valid for that function. Entries in the list should be written in argument=value format  e.g. \code{compare=list( list(object=CATO, group="saplings", years=c(2010:2013)), list(object=CATO, group="seedlings", years=c(2010:2013)) )} Any number of datasets can be added using \code{compare}.
#' @param labels A character vector, defaults to \code{NA}. Labels for each dataset, only used when \code{compare} is not \code{NA}
#' @param ... Other arguments passed to \code{xyplot} in the \pkg{lattice} package.
#' 
#' 
#' @details This function uses the \code{xyplot} function in the \pkg{lattice} package to create plots that display means and 95\% confidence intervals for a variety of measures of plants: - abundance, basal area, cover, and occupancy. The format of the display varies depending on the type of data to be displayed. If only a single dataset is considered (\code{compare=NA}), then each species will be displayed. 
#' 
#' If two or more datasets are used, then  each species is plotted in its own panel, with mean and CI for each dataset. In this case the legend uses the contents of \code{labels} to identify each dataset. 
#' If \code{top=0} and \code{Total=TRUE} then only the totals are displayed. If \code{top=NA} then all species are displayed. One or more particular species can be selected by including a \code{species=} argument to \code{densargs} and / or \code{compare}. 
#' 
#' @return A plot of means and 95\% confidence intervals.
#' 
#' @export


setGeneric(name="densplot",function(object,Total=TRUE,top=NA,densargs=NA,compare=NA,labels=NA,...){standardGeneric("densplot")}, signature="object")

setMethod(f="densplot", signature=c(object="data.frame"),
          function(object,...){
            
            if(!Total){object<-object[object$Latin_Name!="Total",]}
            
            if(is.null(object$which)) densdata<-object[order(-object$Mean),] else densdata<-object[order(object$which,-object$Mean),] 
            
            SpeciesUse<-c()
            
            if (!is.na(top)) {
              ifelse(is.null(densdata$which),
                     SpeciesUse<-densdata$Latin_Name[order(-densdata$Mean)][1:(top+Total)],
                     SpeciesUse<-unique(unlist(tapply( densdata$Latin_Name, INDEX=densdata$which, FUN="[",i=1:(top+Total), simplify=F ) )) )  
              densdata<-densdata[densdata$Latin_Name %in% SpeciesUse,]
              
            }
            
            densdata$Latin_Name<-factor(densdata$Latin_Name, levels=unique(densdata$Latin_Name)  )
            
            if(all(!is.na(labels)) & !is.null(densdata$which)) {levels(densdata$which)<-labels}
            
            panel.which.CI<-function(x,y,groups,subscripts,...){
              panel.xyplot(x,y,groups=groups,subscripts=subscripts,...)
              panel.arrows(x0=densdata$which[subscripts],
                         x1=densdata$which[subscripts],
                         y0=densdata$Lower.95[subscripts], 
                         y1=densdata$Upper.95[subscripts], 
                         angle=90, code=3, lwd=2)
            }  
            
            panel.CI<-function(x,y,groups,...){
              panel.xyplot(x,y,groups=groups, ...)
              panel.arrows(x0=densdata$Latin_Name,
                          x1= densdata$Latin_Name,
                          y0=densdata$Lower.95,
                          y1=densdata$Upper.95,
                          angle=90, code=3, lwd=2)
            }
          
            if(is.null(densdata$which) ) {Formula<-formula("Mean~Latin_Name") 
                                        Groups<-densdata$Latin_Name
                                        Panel<-panel.CI}
          
            if(!is.null(densdata$which) & length(unique(densdata$Latin_Name))>1 )  {Formula<-formula("Mean~which|Latin_Name")
                                                       Groups<-densdata$which
                                                       Panel<-panel.which.CI}
          
            if(!is.null(densdata$which) & length(unique(densdata$Latin_Name))==1)  {Formula<-formula("Mean~which")
                                                        Groups<-densdata$which
                                                        Panel<-panel.which.CI}
            xyplot(x=Formula,
                   data=densdata,
                   pch=16, 
                   cex=3,
                   xlab=NULL,
                   as.table=T,
                   groups=Groups,
                   ...,
                   scales=list(alternating=1, tck=c(1,0)),
                   prepanel=function (x,y,...)list(ylim=c(0,max(densdata$Upper.95, na.rm=T))),
                   par.settings=list(fontsize=list(text=16)),
                   panel=Panel
            )              
  })

setMethod(f="densplot",signature=c("NPSForVeg"),
          function(object, Total, top, densargs,...){
            dens2<-list()
            Tempdens<-do.call(dens, c(object,densargs))
            if(all(!is.na(compare))){
              dens2[[1]]<-Tempdens
              dens2[[1]]$which=1
              for(i in seq_along(compare) ){
                dens2[[i+1]]<-do.call(dens,compare[[i]]) 
                names(dens2)[i+1]<-names(compare[i])
                dens2[[i+1]]$which=i+1
              }
              Tempdens<-rbindlist(dens2)
              Tempdens$which<-factor(Tempdens$which)
              
            }
            densplot(object=Tempdens,Total=Total,top=top, labels=labels,...)
          })

setMethod(f="densplot",signature=c("list"),
          function(object,Total,top,densargs,...){
            
            TempdensPl<-make(object=object,ParkCode="TEMPDENS",ShortName="Temporary Park",LongName="Temp object made by dens()",
                           Network="TEMPDENSNET")
            densplot(object=TempdensPl, densargs=densargs,Total=Total, top=top, compare=compare, labels=labels, ...)
          })
