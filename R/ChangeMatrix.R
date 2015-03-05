#' @title ChangeMatrix
#' 
#' @description Produces a matrix which indcates the change in some measure of plant abundance between two time periods
#' 

###################################################
##### ChangeMatrix()
##### Makes a Site X Species matrix which reflects changes between two time periods
##################################################

setGeneric(name="ChangeMatrix",function(object,group,years1,years2,plots=NA,type="active", density=TRUE,species=NA, output="dataframe",...){standardGeneric("ChangeMatrix")}, signature="object")

setMethod(f="ChangeMatrix", signature=c(object="NPSForVeg"), 
         function(object,group,years1,years2,plots,type,species,...){
           
          
           #find plots common to both time periods
           PlotsUse<-if(!is.na(plots)) {plots} else { 
                            intersect(getPlotNames(object=object,type=type,years=years1),
                            getPlotNames(object=object, type=type, years=years2)
                            )}
           #find plants common to both time periods
           SpeciesUse<-sort(
             if(!is.na(species)) {species} else {
              unique(getPlants(object=object,group=group,years=c(years1,years2), plots=PlotsUse)$Latin_Name)
             }
           )
           
           ########## get the initial matrices
           Matrix1<-SiteXSpec(object=object,group=group,years=years1,species=SpeciesUse,plots=PlotsUse,... )
           
           Matrix2<-SiteXSpec(object=object,group=group,years=years2,species=SpeciesUse,plots=PlotsUse,... )
           
           ########## Make the  change matrix
           
           Matrix3<-data.frame(matrix(NA,nrow=length(PlotsUse),ncol=length(SpeciesUse)+1))
           
           # column names, Plot_Names and Total are easy
           colnames(Matrix3)<-c("Plot_Name",SpeciesUse)
           Matrix3[1]<-PlotsUse
           if(exists("Total",Matrix1) & exists("Total",Matrix2)) {Matrix3$Total=Matrix2$Total-Matrix1$Total}
           
           #Existor is a vector which indicates if a species in just in Matrix1 =-1, just Matrix2 =1, or both =0
           
           Existor<- -1*sapply(SpeciesUse,FUN=exists,Matrix1) + sapply(SpeciesUse,FUN=exists,Matrix2)
           Matrix3[,names(Existor[Existor==0])]=Matrix2[,names(Existor[Existor==0])]-Matrix1[,names(Existor[Existor==0])]
           Matrix3[,names(Existor[Existor==-1])] =  -Matrix1[,names(Existor[Existor==-1])]
           Matrix3[,names(Existor[Existor==1])] =  Matrix2[,names(Existor[Existor==1])]

           
           
           return(Matrix3)                
           
   ## need to use the arguement to get plants and get plotnames to figure out what the two SxS should be like
   ## then make the two sxs and then subtract
        })