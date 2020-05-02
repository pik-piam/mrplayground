#' @title calcValidSNUpE
#' @description Validation Script for Soil Nitrogen Uptake Efficiency (see Bodirsky 2012 for a definition)
#'
#' @param datasource Bodirsky for own calculations, Lassaletta2014 for a country dataset from 
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcSNUpE}}
#' 

#' @importFrom magpiesets reportingnames
calcValidSNUpE<-function(datasource="Bodirsky"){
  past<-findset("past")
  if(datasource=="Bodirsky"){
    snupe<-calcOutput("SNUpE",supplementary = TRUE,aggregate = FALSE)
    weight<-snupe$weight[,past,"constant"]
    snupe<-snupe$x[,past,"constant"]
    getNames(snupe)<-"Resources|Nitrogen|Soil Nitrogen Uptake Efficiency"
    getNames(weight)<-"Resources|Nitrogen|Soil Nitrogen Uptake Efficiency"
    out<-snupe
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
    weight <- add_dimension(weight, dim=3.1, add="scenario", nm="history")
  } else if (datasource=="Lassaletta2014"){
    budget<-readSource("Lassaletta2014",subtype="budget")
    budget[,,"fixation_crops"]=-budget[,,"fixation_crops"]
    inputs<- c("fertilizer","manure","deposition")
    outputs<-c("harvest","fixation_crops")
    snupe<-dimSums(budget[,,outputs])/dimSums(budget[,,inputs])
    weight<-dimSums(budget[,,inputs])
    getNames(snupe)<-"Resources|Nitrogen|Soil Nitrogen Uptake Efficiency"
    getNames(weight)<-"Resources|Nitrogen|Soil Nitrogen Uptake Efficiency"
    out<-snupe
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
    weight <- add_dimension(weight, dim=3.1, add="scenario", nm="history")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  return(list(x=out,
              weight=weight,
              unit="Percent",
              description="Cropland Soil Nitrogen Uptake Efficiency")
  )
}