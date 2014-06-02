#' @title calcIV
#' 
#' @description
#'| Calculates Importance Value from three Site by Sepcies data frames 
#'| One for abundance, one for basal area, one for presence-absence
#' 
#' @param InCount A data.frame which is a site x species matrix where each cell is the number of individuals of a given species in a given site
#' @param InSize A data.frame like InCount except each cell is a measure of the size of the speices such as total basal area or total seedling height
#' @param InPres a data.frame whihc is a presence absence matrix
#' @return Returns a data.frmae with the Improtance Valeus for each species, the columns are "Species" with the name of each specis, "Density" which is density IV, "Size" which is size IV, "Distribution" which is distribution IV and "Total" which is \eqn{Density + Size +Distribution}
#' @export

####################
## calcIV: a function to calculate importance values
#######################
calcIV<-function(InCount,InSize,InPres){

  OutData<-data.frame(Species=names(InCount[-1]),Density=NA,Size=NA,Distribution=NA,Total=NA)
  
  OutData$Density=colSums(InCount[-1])/sum(colSums(InCount[-1]))
  OutData$Size=colSums(InSize[-1])/sum(colSums(InSize[-1]))
  OutData$Distribution=colSums(InPres[-1])/sum(colSums(InPres[-1]))
  
  OutData$Total<-rowSums(OutData[,2:4])
  OutData[-1]<-round(OutData[-1],4)

  return(OutData)
}