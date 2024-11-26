#' @title getNetwork
#'
#' @description Returns the network code for a object of class \code{NPSForVeg}
#'
#' @param object Either a \code{NPSForVeg} object, or a \code{list} of such objects
#'
#' @return A character vector of network codes. For a single \code{NPSForVeg} object a single code is returned. For a \code{list} of objects, a network code for each object in the \code{list} is returned.
#'
#' @examples
#' \dontrun{
#' netn <- importNETN("C:/NETN/R_Dev/data/NPSForVeg/NETN")
#'
#' nwk_pk <- cbind(network = getNetwork(netn),
#'                 parks = getNames(netn, name.class = 'code'))
#'
#' }
#'
#' @export
#'



setGeneric(name="getNetwork",function(object){standardGeneric("getNetwork")} )


setMethod(f="getNetwork", signature="list",
          function(object) {sapply(object, FUN=getNetwork)}  )


setMethod(f="getNetwork", signature="NPSForVeg",
          function(object){object@Network}  )
