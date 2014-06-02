#' @title getPlantName
#' 
#' @import data.table
#' 
#' @description Translates Latin names to common names
#' 
#' @param object an \code{NPSForVeg} object 
#' @param names a character vector of Latin names to be translated
#' @param style Currently only accepts "common", which is the default. May eventually allow a translation from common to Latin names
#' 
#' @return A character vector of common names
#' 
#' @details This funciton will match the Latin names in \code{names} with common names, using the data in the \code{Commons} slot of an object of class \code{NPSForVeg}. 
#' 
#' @export


setGeneric(name="getPlantNames",function(object,names,style="common"){standardGeneric("getPlantNames")},signature=c("object") )

setMethod(f="getPlantNames", signature="NPSForVeg",
          function(object,names,style) {
            switch(style,
                 common={
                   TempCom<-data.table(object@Commons,key="Latin_Name")
                   return(TempCom[i=as.character(names),j=Common]$Common)                 },
                 )
})