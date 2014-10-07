#' @title getPlantNames
#' 
#' @import data.table
#' 
#' @description Translates Latin names to common names using TSN numbers
#' 
#' @param object An \code{NPSForVeg} object 
#' @param names a character vector of Latin Names, common names or TSN numbers (as a character vector) to be translated
#' @param out.style Either "common", "Latin", or "TSN". Indicates the type of name that should be returned. Defaults to "common"
#' @param in.style Either "common", "Latin", or "TSN". Indicates the type of name found in the "names" argument. 
#' @param output  Currently only accpets"dataframe". Determines the output type When \code{object} is a list. "dataframe", the default, indicates the   output from all of \code{NSPForVeg} objects should be combined into a single \code{data.frame}. When implemented "list" will return a   \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based    on that objects \code{ParkCode} slot. 

#' 
#' @return A character vector of names in the format speceified by \code{out.style}.
#' 
#' @details This function will covert the names in \code{names} to the style specified in \code{out.style}, using the data in the \code{Commons} slot of an object of class \code{NPSForVeg}. Note that TSN numbers are used to covert from one style of name to the other.  
#' 
#' @export


setGeneric(name="getPlantNames",function(object,names, out.style="common", in.style="Latin",output="dataframe"){standardGeneric("getPlantNames")},signature="object" )

setMethod(f="getPlantNames", signature="NPSForVeg",
    function(object,names,out.style, in.style) {
      in.TSN<-switch(in.style,
        TSN= names,
        Latin= data.table(object@Commons,key="Latin_Name") [i=as.character(names),j=TSN],
        common= data.table(object@Commons,key="NCRN_Common") [i=as.character(names),j=TSN]
      )
      switch(out.style,
        TSN= return(in.TSN),
        common= return(data.table(object@Commons,key="TSN")[i=in.TSN,j=Common]),
        Latin= return(data.table(object@Commons,key="TSN")[i=in.TSN,j=Latin_Name])
      )
})