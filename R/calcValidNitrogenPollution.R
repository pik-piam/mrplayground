#' @title calcValidNitrogenPollution
#' @description Validation Script for Nitrogen Budgets on Croplands
#'
#' @param datasource Bodirsky for own calculations, FAO for some N related parameters published in FAOSTAT.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcEmisNitrogenPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNitrogenPollution")
#' }
#' 
#' @importFrom magpiesets reportingnames summationhelper
calcValidNitrogenPollution<-function(datasource="Nsurplus"){
  
  if(datasource%in%c("IPCC","Nsurplus","Nsurplus2")){
    x<-calcOutput("EmisNitrogenPast",method=datasource,aggregate = FALSE)
    x<-dimOrder(x,c(2,1))
    
    getNames(x,dim=1)<-reportingnames(getNames(x,dim=1))
    getNames(x,dim=2)<-reportingnames(getNames(x,dim=2))
    
    names<-getNames(x,dim=2)
    
    tmp<-which(names%in%c("Cropland soil emissions","Pasture soil emissions","Animal waste management",
                           "Inorganic fertilizers","Manure applied to croplands",
                           "Crop residues recycled to croplands","Soil Organic Matter Loss",
                           "Rice cultivation"))
    names[tmp]<-paste0("Agriculture|",names[tmp])
    names[-tmp]<-names[-tmp]
    getNames(x,dim=2)<-names
    
    tmp2<-which(names%in%c("Non-agricultural soil emissions","Groundwater","Riparian Zones","Inland freshwater bodies","Oceans","Lightning"))
    names[tmp2]<-paste0("Natural and indirect emissions|",names[tmp2])
    names[-tmp2]<-names[-tmp2]
    getNames(x,dim=2)<-names
    
    
    out<-x
    
    
    getNames(out)<-sub(getNames(out),pattern = "\\.",replacement = "|")
    out<-summationhelper(out)
    out<-out[,,order(getNames(out))]
    
    out3<-dimSums(x,dim=3.2)
    out4<-dimSums(x[,,names[tmp]],dim=3.2)
    getNames(out4,dim=1)<-paste0("Agriculture|+|",getNames(out4,dim=1))
    out5<-dimSums(x[,,names[tmp2]],dim=3.2)
    getNames(out5,dim=1)<-paste0("Natural and indirect emissions|+|",getNames(out5,dim=1))
    
    out6<-dimSums(x,dim=3.1)
    getNames(out6,dim=1)<-paste0("Nitrogen surplus|+|",getNames(out6,dim=1))
    
    out<-mbind(out,out3,out4,out5,out6)

    getNames(out)<-paste0("Emissions|",getNames(out))
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  } else if (datasource=="FAO"){    
    #fertilizer
    fertilizer<-readSource("FAO","EmisAgSynthFerti")[,,c(
      "Emissions_(N2O)_(Gigagrams)",
      "Indirect_emissions_(N2O)_(Gigagrams)")]
    fertilizer<-dimSums(fertilizer,dim=c(3))/1000000
    fertilizer<-setNames(fertilizer,"Inorganic Nitrogen Fertilizer")
    
    names_x<-reportingnames(getNames(out))
    names(names_x)<-NULL
    getNames(out)<-paste0("Resources|Nitrogen|Cropland Budget|",names_x)
    
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  
  unit="Mt Nr/yr"
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  
  return(list(x=out,
              weight=NULL,
              unit= unit,
              description="Cropland Nitrogen Budget")
         )
}
