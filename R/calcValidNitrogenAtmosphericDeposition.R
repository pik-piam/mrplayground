#' @title calcValidNitrogenAtmosphericDeposition
#' @description Validation Script for Atmospheric nitrogen deposition
#'
#' @param datasource Bodirsky for own calculations based on Dentener et al, CEDS et al and self-calcualted emissions
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAtmosphericDeposition}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNitrogenAtmosphericDeposition")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNitrogenAtmosphericDeposition<-function(datasource="CEDS"){
  
  dep<-collapseNames(calcOutput("AtmosphericDeposition",datasource=datasource,aggregate = FALSE))
    
  dep0<-dimSums(dep,dim=c(3.1,3.2))
  getNames(dep0)<-paste0("Resources|Nitrogen|Terrestrial Atmospheric Deposition")
    
  dep2<-dimSums(dep,dim=c(3.1))
  getNames(dep2)<-paste0("Resources|Nitrogen|Terrestrial Atmospheric Deposition|+|",reportingnames(getNames(dep2)))
    
  dep3<-dimSums(dep,dim=c(3.2))
  getNames(dep3)<-paste0("Resources|Nitrogen|Terrestrial Atmospheric Deposition|++|",reportingnames(getNames(dep3)))
    
  out<-mbind(dep0,dep2,dep3)
    
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  getNames(out) <- paste0(getNames(out)," (Mt Nr/yr)")
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Atmospheric deposition of ammonia and nitrogen oxides on different land types.")
  )
}