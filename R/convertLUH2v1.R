#' @importFrom magclass getRegionList<-
#' @importFrom luscale groupAggregate
convertLUH2v1<-function(x,subtype){
  mapping <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
  getRegionList(x) <- rep("GLO",ncells(x))
  
  out <- groupAggregate(data = x,query = mapping,from="cell",to="iso",dim=1)

  out  <- toolCountryFill(out,fill=0)
  return(out)
}  