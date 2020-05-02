calcValidResiduesProduction<-function(datasource="own", detail=T){
  
  if(datasource=="FAO"){
    out<-calcOutput("ResProd",aggregate = FALSE)
    out<-dimSums(out,dim=3.2)
    out1<-setNames(collapseNames(out[,,"ag"][,,"dm"]),"Aboveground Crop Residues in dry matter (Mt DM)")
    out2<-setNames(collapseNames(out[,,"ag"][,,"nr"]),"Aboveground Crop Residues in nitrogen (Mt Nr/yr)")
  } else if(datasource=="IFA"){
    out<-readSource("IFA")
    out<-setNames(out,"Inorganic Nitrogen Fertilizer")
  } else stop("No data exist for the given datasource!")
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Inorganic Fertilizer application")
  )
}