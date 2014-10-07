#' @title getNetwork
#' 
#' @description Returns the network code for a object of class \code{NPSForVeg}
#' 
#' @param object Either a \code{NPSForVeg} object, or a \code{list} of such objects
#' 
#' @return A characeter vector of network codes. For a single \code{NPSForVeg} object a single code is returned. For a \code{list} of objects, a network code for each object in the \code{list} is returned. 
#' 
#' @export
#' 



setGeneric(name="getNetwork",function(object){standardGeneric("getNetwork")} )


setMethod(f="getNetwork", signature="list",
          function(object) {sapply(object, FUN=getNetwork)}  )


setMethod(f="getNetwork", signature="NPSForVeg",
          function(object){object@Network}  )
