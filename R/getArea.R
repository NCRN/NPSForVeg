#' @title getArea
#' 
#' @description Retrieves the number and area of various plot sizes from an object of class NPSForVeg.
#' 
#' @param object Either an object of class NPSForVeg, or a list of objects of class NPSForVeg
#' @param group Indicates which plant groups the area should correspond to, as different groups are monitored on different size plots. The name of the group should be enclosed in quotes. Acceptable values are" "trees", "saplings", "seedlings","shrubs", "shseedlings","vines", "herbs".
#' @param type Indicates the type of area to be returned, and should be enclosed in quotes. Acceptable values are 
#'\describe{
#'  \item{"all"}{(the default) Returns the total area sampled per plot for that group.}
#'  \item{"single"}{Returns the area of one of the subsamples (eg. microplot or quadrat).If there is only one sample of that group, such as trees sample over an entire plot, than "single" and "all" will produce identical results}
#'  \item{"count"}{A count of the number of the number of subplots/microplots/quadrats that the group is monitored in.}
#'}
#' 
#' @return For a single NPSForVeg object getArea returns a vector of length one. For a list of such object it returns a vector as long as the list. 
#' 
#' @export


### This calculates the areas for various kinds of plots in m^2

setGeneric(name="getArea",function(object,group,type="all"){standardGeneric("getArea")},signature=c("object") )

setMethod(f="getArea", signature="list",
          function(object,group,type) 
            {sapply(object, FUN=getArea, group=group,type=type)})


setMethod(f="getArea", signature="NPSForVeg",
  function(object,group,type){
    switch(group,
      trees= switch (type,
              all=return(object@TPlotSize[1]*object@TPlotSize[2]),
              single=return(object@TPlotSize[2]),
              count=return(object@TPlotSize[1]) 
      ),
      saplings = switch (type,
                     all=return(object@SapPlotSize[1]*object@SapPlotSize[2]),
                     single=return(object@SapPlotSize[2]),
                     count=return(object@SapPlotSize[1]) 
      ), 
      seedlings = switch (type,
                        all=return(object@SeedPlotSize[1]*object@SeedPlotSize[2]),
                        single=return(object@SeedPlotSize[2]),
                        count=return(object@SeedPlotSize[1]) 
      ),
      shrubs = switch (type,
                         all=return(object@ShrubPlotSize[1]*object@ShrubPlotSize[2]),
                         single=return(object@ShrubPlotSize[2]),
                         count=return(object@ShrubPlotSize[1]) 
      ),          
      shseedlings = switch (type,
                     all=return(object@ShSeedPlotSize[1]*object@ShSeedPlotSize[2]),
                     single=return(object@ShSeedPlotSize[2]),
                     count=return(object@ShSeedPlotSize[1])
      ),
      vines = switch (type,
                            all=return(object@VPlotSize[1]*object@VPlotSize[2]),
                            single=return(object@VPlotSize[2]),
                            count=return(object@VPlotSize[1])
      ),
      herbs = switch (type,
                            all=return(object@HPlotSize[1]*object@HPlotSize[2]),
                            single=return(object@HPlotSize[2]),
                            count=return(object@HPlotSize[1])
      )
    )
  }
)