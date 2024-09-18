#' @include NPSForVeg_Class_def.R
#'
#' @importFrom dplyr recode
#' @importFrom purrr map_chr map2
#'
#' @title getPlantNames
#'
#' @description Translates between Latin names, common names and TSN numbers.
#'
#' @param object An \code{NPSForVeg} object
#' @param names a character vector of Latin names, common names or TSN numbers (as a character vector) to be translated, or when list of
#' \code{NPSForVeg} objects is passed to \code{object}, a list of plant names can be passes to \code{names}. See details below.
#' @param out.style Either "common", "Latin", or "TSN". Indicates the type of name that should be returned. Defaults to "common"
#' @param in.style Either "common", "Latin", or "TSN". Indicates the type of name found in the "names" argument.
#' @param output  Accepts "vector" or "list". Determines the output type When \code{object} is a list.
#'  When implemented "list" will return a \code{list} where each element of the list is a \code{data.frame} from a
#'   single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot.
#'
#' @return A character vector or list of names in the format specified by \code{out.style}.
#'
#' @details This function will convert the names in \code{names} to the style specified in \code{out.style},
#'  using the data in the \code{Commons} slot of an object of class \code{NPSForVeg}. Note that if the data in the \code{Commons} slot
#'  has more than one entry for one or more of the input names, or is not present, the function will fail with an error.
#'
#' If the \code{object} is a list, then the \code{names} can be a list as well. If so then then each element in the list of objects will
#' be matched with the corresponding element in the list of names, and \code{getPlantNames} will be run. When \code{output} is "vector" the
#' resulting character vectors will be concatenated using \code{unlist}. When \code{output} is "list" the the output will be a list with each
#' element being a character vector of names.
#'
#' The function will not accept a list of names when the \code{object} is a data.frame or an \code{NPSForVeg} object.
#'
#' @examples
#' \dontrun{
#' ncrn <- importNCRN("C:/Data/")
#'
#' getPlantNames(ncrn, names = "Quercus rubra", out.style = "common")
#'
#' }
#' @export


setGeneric(name="getPlantNames",function(object,names, out.style="common", in.style="Latin",output="vector"){standardGeneric("getPlantNames")},signature="object" )

setMethod(f="getPlantNames", signature="list",
          function(object,names,out.style, in.style, output) {
            OutData<-map2(.x=object, .y=names, .f=getPlantNames, out.style=out.style, in.style=in.style)

            switch(output,
                vector=return(unlist(OutData)),
                list=return(OutData)
            )
})




setMethod(f="getPlantNames", signature="NPSForVeg",
    function(object,names,out.style="common", in.style="Latin") {

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
  OutData<-map_chr(names, ~Prefilter[Prefilter[[in.col]] %in% .x, out.col] )

  return(OutData)

})
