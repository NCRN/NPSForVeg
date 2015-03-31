#' @title mapPlants
#' 
#' @import leaflet
#' 
#' @description Produces an html map from a vector of plot names and a vector of corresponding values.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param plots A character vector of plot names. 
#' @param values  A vector of values to map. Can be either numberical data or categories.
#' @param maptype The type of base map to be used. Options are:
#'  \describe{
#'  \item{"basic"}{The default, uses the basic Park Tiles map, similar to the maps found in park brochures.}
#'  \item{"imagery"}{The Park Tiles satellite imagery map.}
#'  \item{"slate"}{The Park Tiles slate map. A very muted and grey base map.}
#' }
#' @param colorgroups The number of different colors to display for maps with a \code{colortype} of "bin" or "quantile", or a series of cut points to be used with \code{colortype="bin"} See discussion below.
#' @param radius The radius in meters, of the circle drawn on the map. When the map is zoomed out the circles will be visible regardless of the value chosen.
#' @param opacity Opacity of the circles on the map. A single number from 0 (completely transparent) to 1 (completely opaque). Defaults to 1.
#' @param colortype Indicates the method of assiging colors to each plot. Make use of the leaflet package's color funcitons:
#'  \describe{
#'    \item{"quantile"}{Divides the data into the number of groups indicated in \code{colorgroups} based on quantiles of the data. An approximately equal number of plots will be in each group.  Each group will be given a different color.}
#'    \item{"bin"}{Divides the data into groups based on the \code{cut} function. Like the "quantile" option in divides the plots into groups, but in this case each group should cover an approximately equal range in values of the data. Alternatively if \code{colorgroups} is a vector rather than a single number, the elements of the vector will be the cut points separating the groups to be mapped.}
#'    \item{"numeric"}{Colors the plots based on a smooth color ramp, rather than dividing into groups.}
#'    \item{"factor}{Used when \code{values} are categorical data.}
#'  }
#' @param colors  A character vector of one or more colors for the plots on the map. See discussion below.
#' 
#' @details  This function is serves as a wrapper for the leaflet package. It quickly creates a map by plotting the locations of the plots on the ParkTiles base map from the NPS. 
#' 
#' Several option exist for coloring the plots based on the \code{colorNumeric}, \code{colorBin}, \code{colorQuantile}, and \code{colorFactor} functions in leaflet.
#' 
#' quantile - This option will divide the plots into the equal quantiles based on the data in \code{values}. The \code{colorgroups} agrument indicates the number of gourps the plots will be divided into. For exmple if \code{colorgroups=4} then the plots with the bottom quarter of \code{values} will get one color, the second quarter of \code{values} will get a second color etc. An erorr may occur if it is not possible to divide the \code{value} vector in to the indicate number of groups. For example, if half of \code{values} is the same number, than it cannot be divided into thirds.
#' 
#' bin - This option also divides the plots into groups, but in this case the groups either cover an equal range in \code{values} or cover ranges specificed in \code{colorgroups}. If \code{colorgroups} is a single value, than that will indicate the number of groups to divide \code{values} into. For example, if \code{values} range from 1 to 100, and \code{colorgroups} is 4, the plots with values 1-25 will be one color, 26-50 a second and so on. If instead \code{colorgroups} is \code{c(-1,10,50,75,101)} then plots from 1-10 will be one color, 11-50 a second and so on. 
#' 
#' numeric - This option does not divide the plots into groups. Rather plots will have a smooth ramp of colors from the lowest to the highest value.
#' 
#' factor - This well color the plots when \code{values} is categorical data (e.g. soil or vegetation type) rather than numeric. Each category gets a different color. 
#' 
#'The \code{colors} agrument indicates which colors will be chosen for the plots. This is a charcter vector and can either be standard R color names ("blue", "green" etc.) or hexadecimal colors ("#0000FF","008000",etc). If the number of colors is equal to the number of groups that each group will get the corresponding color. If there are fewer colors than groups, than a colorramp will be created using the indicate colors and used for the plots. Typically maps made with the \code{"numeric"} option will make use of the color ramp.
#'
#'Currently there is no legend displayed with the map. However, if you click on a circle it will give you the Plot Name and the value associated with the plot.
#' 
#' @export

setGeneric(name="mapPlants",function(object,plots,values, maptype="basic", colorgroups=8,radius=30,opacity=1,colortype="quantile",
                                 colors=c("cyan","magenta4","orangered3"),...){standardGeneric("mapPlants")}, signature="object")

setMethod(f="mapPlants", signature=c(object="list"),
          function(object,plots,values,maptype,colorgroups,radius,opacity,colortype,colors,...){
                     TempPark<-make(object,ParkCode="TEMPOBJ", ShortName="TempObj",LongName="Temp park for SiteXSpec", Network="SiteSpec")
                     return(mapPlants(object=TempPark, plots=plots,values=values,maptype=maptype, colorgroups=colorgroups,radius=radius,
                                      opacity=opacity,colortype=colortype,colors=colors,...))
          })


setMethod(f="mapPlants", signature=c(object="NPSForVeg"),
          function(object,plots,values,...){

              ColRamp<-colorRampPalette(colors)
              
              BaseMap<-switch(maptype,
                              basic="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
                              imagery="//{s}.tiles.mapbox.com/v4/nps.map-n9nxe12m,nps.gdipreks,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
                              slate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q"
                              )
              
              MapCol<-switch(colortype,
                     quantile=colorQuantile(palette=ColRamp(colorgroups),n=colorgroups,domain=values),
                     bin=colorBin(palette=ColRamp(ifelse(length(colorgroups)==1,colorgroups,length(colorgroups)-1) ),
                                  bins=colorgroups,domain=c(min(values),max(values))),
                     numeric=colorNumeric(palette=ColRamp(length(values)),domain=c(min(values),max(values) )),
                     factor=colorFactor(palette=ColRamp(length(unique(values))),domain=values  )
              )
              
              ForMap<-leaflet() %>%
                addTiles(urlTemplate=BaseMap) 
              ForMap%>%
                addCircles(data=getPlots(object=object,plots=plots), color=MapCol(values), fillColor=MapCol(values), radius=radius,
                            opacity=opacity, fillOpacity=opacity, popup=paste(plots,":",as.character(values)))
})

