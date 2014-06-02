#' @title getCommons
#' 
#' @description gets common names data.frame from one or more NPSForVeg objects. 
#' 
#' @param object Either a NPSForVeg object or a list of such objects.
#' @param output Used when "object" is a list to determine if the output is a data.frame or a list. Can be either "dataframe" (the default), or "list". Needs to be in quotes.
#' 
#' @return Either a data.frame or a list containing the contents of the "Commons" slot for one or more NPSForVeg objects.
#'  
#' @details  When the "object" is a single NPSForVeg object this funciton will return the data.frame in the "Commons" slot  which should have the Latin and common names of the plants. When "object" is a list and "output" is "dataframe" a combied data.frame with the "Commons" slot from all NPSForVeg elements in returned. If the "object" is a list and "output" is "list" then a list is returned. Each element of the list will be the "Commons" slot from one the input NPForVeg objects, and the name of each element corresponds to the name of the NPSForVeg object it comes from.  
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
