#' @include NPSForVeg_Class_def.R
#' 
#' @importFrom dplyr recode
#' @importFrom purrr map_chr
#' 
#' @title getPlantNames
#' 
#' @description Translates between Latin names, common names and TSN numbers
#' 
#' @param object An \code{NPSForVeg} object 
#' @param names a character vector of Latin names, common names or TSN numbers (as a character vector) to be translated
#' @param out.style Either "common", "Latin", or "TSN". Indicates the type of name that should be returned. Defaults to "common"
#' @param in.style Either "common", "Latin", or "TSN". Indicates the type of name found in the "names" argument. 
#' @param output  Currently does nothtin Will accepts "vector" or "list". Determines the output type When \code{object} is a list 
#'  When implemented "list" will return a \code{list} where each element of the list is a \code{data.frame} from a
#'   single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot. 
#' 
#' @return A character vector of names in the format specified by \code{out.style}.
#' 
#' @details This function will convert the names in \code{names} to the style specified in \code{out.style},
#'  using the data in the \code{Commons} slot of an object of class \code{NPSForVeg}. Note that if the data in the \code{Commons} slot
#'  has more than one entry for one or more of the input names, the fucniton will fail with an error.
#'   
#' 
#' @export


setGeneric(name="getPlantNames",function(object,names, out.style="common", in.style="Latin",output="vector"){standardGeneric("getPlantNames")},signature="object" )

setMethod(f="getPlantNames", signature="NPSForVeg",
    function(object,names,out.style, in.style) {
      
      return(getPlantNames(object=getCommons(object), names=names, out.style = out.style, in.style = in.style))
      
})

setMethod(f="getPlantNames", signature="data.frame",
          function(object,names,out.style="common", in.style="Latin") {

  # recode the text input to the correct column names
            
  in.col<-recode(in.style, Latin="Latin_Name", common="Common")     
  out.col<-recode(out.style, Latin="Latin_Name", common="Common")  

  # Prefitler the data so only the part that contains the correct names is examined 
  
  Prefilter<-object[object[[in.col]] %in% names,] 
  
  # Got through each name in input and get the correct ouptut
  Output<-map_chr(names, ~Prefilter[Prefilter[[in.col]] %in% .x, out.col] )
        
  return(Output) 
            
})