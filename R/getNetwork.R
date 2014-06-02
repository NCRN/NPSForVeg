#' @title getNetowrk
#' 
#' @description Returns the network code for a object of class NPSForVeg
#' 
#' @param object Either a NPSForVeg object, or a list of such objects
#' 
#' @return A characeter vector of network codes. For a single NPSForVeg object a single code is returned. For a list of objects, a network code for each object in the list is returned. 
#' 
#' @export
#' 



setGeneric(name="getNetwork",function(object){standardGeneric("getNetwork")} )


setMethod(f="getNetwork", signature="list",
          function(object) {sapply(object, FUN=getNetwork)}  )


setMethod(f="getNetwork", signature="NPSForVeg",
          function(object){object@Network}  )
