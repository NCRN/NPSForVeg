#' @include  NPSForVeg_Class_def.R
#' @title getCommons
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct filter
#' @importFrom rlang quo
#' 
#' @description gets the common names \code{data.frame} from one or more \code{NPSForVeg} objects. 
#' 
#' @param object Either a data.frame like that stored in the \code{Commons} slot, an \code{NPSForVeg} object or a list of such objects.
#' @param species A character vector of species Latin names. Will only return data from those species.
#' @param nativity Indicates the nativity of the desired species. Three options are available:
#' \describe{
#' \item{"native"}{Only native species are returned.}
#' \item{"exotic"}{Only exotic species are returned.}
#' \item{"all"}{The default. Species are returned regardless of nativity.}
#' }
#' 
#' @param output Used when "object" is a list, to determine if the output is a \code{data.frame} or a \code{list}. Can be either "dataframe" (the default), or "list". Needs to be in quotes.
#' 
#' @return Either a \code{data.frame} or a \code{list} containing the contents of the "Commons" slot for one or more \code{NPSForVeg} objects.
#'  
#' @details  When the "object" is a single \code{NPSForVeg} object this function will return the \code{data.frame} in the "Commons" slot.  
#' When "object" is a \code{list} and "output" is "dataframe" a combined \code{data.frame} with the "Commons" slot from all NPSForVeg 
#' elements is returned. This \code{data.frame} is filtered so that identical rows are not returned. If a species is present in more than
#' one of the \code{data.frame}s, and the metadata for the speices (e.g. native vs exotic) differs between them, then the species will 
#' be returned twice, once with each set of metadata.  If the "object" is a \code{list} and "output" is "list" then a \code{list} is
#' returned. Each element of the  \code{list} will be the "Commons" slot from one the input \code{NPForVeg} objects.  
#' 
#' @export

############### getCommons command for NPSForVeg class ###############
setGeneric(name="getCommons",function(object,species=NA,nativity="all",
                                      output="dataframe"){standardGeneric("getCommons")},signature=c("object") )


setMethod(f="getCommons", signature=c(object="list"),
          function(object,species,nativity, output){
            
            OutCommons<-lapply(object, getCommons, species, nativity)
            
            switch(output,
                   list=return(OutCommons),
                   dataframe=return( distinct(rbindlist(OutCommons) ))
            )
})  


setMethod(f="getCommons", signature=c(object="NPSForVeg"),
          function(object,species, nativity){
                  return( getCommons(object@Commons, species, nativity) )
 })

setMethod(f="getCommons", signature=c(object="data.frame"),
          function(object, species, nativity){
            
            ## create "filt" which will hold all of the filters to be applied
            filt<-list(quo(Latin_Name %in% species),  # species names
                       switch(nativity,                 # exotic or not
                              "all"=NA,
                              "native"= quo(!Exotic),
                              "exotic"=quo(Exotic)
                          )
                       
                       
            )
     
            ## get rid of filters that the user does not want
            filt<-filt[c(all(!is.na(species)),!nativity=="all") ]
     
            # filter data or just return the data.frame if there are no filters
            return( if(length(x)>0) filter(object, !!!filt) else object)
  })
