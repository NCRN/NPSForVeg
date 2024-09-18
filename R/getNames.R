#' @title getNames
#'
#' @description Retreives park names from an NPSForVeg object of a list of such objects.
#'
#' @param object Either an NPSForVeg object or a list of such objects
#' @param name.class Type of name to return. One of three options, in quotes.
#' \describe{
#' \item{"short"}{ The default. Returnes the short name of the park}
#' \item{"long"}{Returns the long, format name of the park}
#' \item{"code"}{Returns the park code}
#' }
#' @return A character vector with one or more park names.
#'
#' @examples
#' \dontrun{
#'
#' ncrn <- importNCRN("C:/Data")
#'
#' # full park names
#' getNames(ncrn, name.class = "long")
#'
#' # park codes
#' getNames(ncrn, name.class = "code")
#' }
#'
#' @export

setGeneric(name="getNames",function(object,name.class="short"){standardGeneric("getNames")},signature=c("object") )


setMethod(f="getNames", signature=c(object="list"),
  function(object,name.class){
    sapply(object,FUN=getNames, name.class=name.class) } )


setMethod(f="getNames", signature=c(object="NPSForVeg"),
          function(object,name.class){
            switch(name.class,
            code = return(object@ParkCode),
            short = return(object@ShortName),
            long = return(object@LongName),
            stop("Unrecognized type in getNames")
            )
})

