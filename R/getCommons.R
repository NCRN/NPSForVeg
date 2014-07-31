#' @title getCommons
#' 
#' @description gets the common names \code{data.frame} from one or more \code{NPSForVeg} objects. 
#' 
#' @param object Either a \code{NPSForVeg} object or a list of such objects.
#' @param output Used when "object" is a list, to determine if the output is a \code{data.frame} or a \code{list}. Can be either "dataframe" (the default), or "list". Needs to be in quotes.
#' 
#' @return Either a \code{data.frame} or a \code{list} containing the contents of the "Commons" slot for one or more \code{NPSForVeg} objects.
#'  
#' @details  When the "object" is a single \code{NPSForVeg} object this function will return the \code{data.frame} in the "Commons" slot  which should have the Latin and common names of the plants as well as the TSN Numbers. When "object" is a \code{list} and "output" is "dataframe" a combied \code{data.frame} with the "Commons" slot from all NPSForVeg elements is returned. If the "object" is a \code{list} and "output" is "list" then a \code{list} is returned. Each element of the \code{list} will be the "Commons" slot from one the input \code{NPForVeg} objects, and the name of each element corresponds to the name of the \code{NPSForVeg} object it comes from.  
#' 
#' @export

############### getCommons command for NPSForVeg class ###############
setGeneric(name="getCommons",function(object,output="dataframe"){standardGeneric("getCommons")},signature=c("object") )


setMethod(f="getCommons", signature=c(object="list"),
          function(object,output){
            OutCommons<-vector(mode="list",length=length(object))
            for (i in seq_along(object)) {
              OutCommons[[i]]<-getCommons(object[[i]])
            } 
            switch(output,
                   list={names(OutCommons)<-getNames(object,name.class="code")
                         return(OutPCommons)},
                   dataframe=return( do.call("rbind",OutCommons) )
            )
            
            } )  


setMethod(f="getCommons", signature=c(object="NPSForVeg"),
          function(object){
                  return(object@Commons)
          })
