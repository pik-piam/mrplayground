calcValidNitrogenBudgetPlanet<-function(){
  past<-findset("past")
  emis<-calcOutput("EmissionInventory",
                   datasource="Nsurplus2",
                   mapping=NULL,
                   aggregate=FALSE)[,,c("no2_n","nh3_n")]
  
  emis<-emis[,,c("industry","transport","luc")]
  vcat(2,"luc should be replaced with own calculations")
  
  # inputs
  
  sectors<-dimSums(emis,dim=3.2)
  getNames(sectors)<-paste0("Resources|Nitrogen|Planetary Budget|Fixation and Release|",reportingnames(getNames(sectors)))
  
  # Natural and anthropogenic Biological Nitrogen Fixation
  bnf<-calcOutput("NitrogenBNF",aggregate = FALSE)
  bnf<-mbind(
    setNames(dimSums(bnf,dim=3.1),"Resources|Nitrogen|Planetary Budget|Fixation and Release|Terrestrial Biological Fixation"),
    setNames(bnf,paste0("Resources|Nitrogen|Planetary Budget|Fixation and Release|Terrestrial Biological Fixation|+|",reportingnames(getNames(bnf))))
  )
  
  # Natural rate oceans
  # Fertilizer
  fertilizer<-calcOutput("FertN",aggregate = FALSE)[,past,]
  getNames(fertilizer)<-paste0("Resources|Nitrogen|Planetary Budget|Fixation and Release|",reportingnames("fertilizer"))
  # SOM
  som<-calcOutput("SOMlossN",aggregate = FALSE)[,past,]
  getNames(som)<-paste0("Resources|Nitrogen|Planetary Budget|Fixation and Release|",reportingnames("som"))
  
  oceanfix<-calcOutput("NitrogenBudgetOcean",aggregate = FALSE)[,,"fixation_ocean"]
  getNames(oceanfix)<-paste0("Resources|Nitrogen|Planetary Budget|Fixation and Release|",reportingnames(getNames(oceanfix)))
  
  #cropland_losses<-calcOutput("NitrogenBudgetCropland")
  #cropland_emis<-calcOutput("EmisNitrogenPast")
  
  vcat(2,"Ammonia fixation for non-agricultural purposes missing. About 30 Tg")
  
  out<-mbind(
    sectors,
    bnf,
    fertilizer,
    som,
    oceanfix
  )
  
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm="own")
  names(dimnames(out))[3] <- "scenario.model.variable"
  unit="Mt Nr/yr"
  getNames(out) <- sub("\\|$","",getNames(out)) 
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Nitrogen fixation and release from long-term storage")
  )
}