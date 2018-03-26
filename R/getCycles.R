#' @include  NPSForVeg_Class_def.R
#' @title getCycles
#' 
#' @importFrom dplyr %>% filter rowwise
#' @importFrom rlang !!! quo
#' 
#' @description Gets the cycles \code{data.frame} from one or more \code{NPSForVeg} objects. 
#' 
#' @param object Either a data.frame like that stored in the \code{Cycles} slot, an \code{NPSForVeg} object or a list of such objects.
#' @param cycles A vector of cycle codes. Will only return data from those cycles.
#' @param years  A length 1 numeric vector containing one or more years. Will only return cycles that include those years.
#' 
#' @return Either a \code{data.frame} or a \code{list} containing the contents of the "Cycles" slot for one or more \code{NPSForVeg} objects.
#'  
#' @details  When the "object" is a \code{data.frame} or a single \code{NPSForVeg} object this function will return the \code{data.frame} in 
#' the "Cycles" \code{slot}. This \code{data.frame} will be filtered as specified in the other arguments. When "object" is a \code{list} 
#' then a \code{list} is returned. Each element of the \code{list} will be the filtered \code{data.frame} from "Cycles" \code{slot} from
#' the input \code{NPForVeg} objects.  
#' 
#' @export

setGeneric(name="getCycles",function(object,cycles=NA,years=NA){standardGeneric("getCycles")},signature=c("object") )


setMethod(f="getCycles", signature=c(object="list"),
          function(object,cycles,years){
            
            return(lapply(object, getCycles, cycles, years))
})  


setMethod(f="getCycles", signature=c(object="NPSForVeg"),
          function(object,cycles,years){
            
            return( getCycles(object@Cycles, cycles, years) )
})


setMethod(f="getCycles", signature=c(object="data.frame"),
          function(object,cycles, years){
            
  ## function to check if vector of years x is in range of years y
  CycleCheck<-function(x,y){    
    any(sapply(X=x, FUN= `%in%`, y))
  }
            
  ## create "filt" which will hold all of the filters to be applied
  filt<-list(quo(Cycle %in% cycles),  # numeirc code for cycle
    quo(CycleCheck(years, YearStart:YearEnd) )#cycle(s) whcich contain the given year
  )
            
  ## get rid of filters that the user does not want
  filt<-filt[c(all(!is.na(cycles)),all(!is.na(years)))]
             
  ## filter data or just return the data.frame if there are no filters
  return( if(length(filt)>0) object %>% rowwise %>% filter(!!!filt) %>% as.data.frame else object)
  
})
