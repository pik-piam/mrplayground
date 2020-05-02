calcValidFertilizer<-function(datasource="FAO", detail=T){
  
  if(datasource=="FAO"){
    out<-readSource("FAO","EmisAgSynthFerti")[,,c(
      "3102|Nitrogen Fertilizers (N total nutrients).Consumption_in_nutrients_(tonnes_of_nutrients)",
      "1360|Nitrogenous fertilizers.Consumption_in_nutrients_(tonnes)")]
    out<-dimSums(out,dim=c(3))/1000000
    out<-setNames(out,"Inorganic Nitrogen Fertilizer")
    out <- add_dimension(out, dim=3.2, add="model", nm="FAO")
  } else if(datasource=="IFA"){
    out<-readSource("IFA",subtype = "consumption")[,,"Grand Total N"]/1000
    out<-setNames(out,"Inorganic Nitrogen Fertilizer")
    out <- add_dimension(out, dim=3.2, add="model", nm="IFA")
  } else stop("No data exist for the given datasource!")
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Inorganic Nitrogen Fertilizer application")
  )
}