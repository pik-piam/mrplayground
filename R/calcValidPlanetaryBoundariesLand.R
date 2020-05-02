#' @title calcValidPlanetaryBoundariesLand
#' @description Provides comparison for the planetary boundary indicators for nitrogen
#' @param datasource datasource to compare to. Historical trajectories of the indicators, or the planetary boundary by the Rockstroem Paper 2009 or the Steffen et al paper 2015.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidPlanetaryBoundariesPhosphorus}},
#' \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidPlanetaryBoundariesLand",datasource="Rockstroem2009",aggregate=FALSE)
#' }
#' 
#' @importFrom magclass getSets



calcValidPlanetaryBoundariesLand<-function(datasource="Rockstroem2009"){
  if (datasource=="LUH2v2"){
    land<-calcOutput("LUH2v2",aggregate = FALSE)
    weight=dimSums(land,dim=3.1)
    out=land[,,"crop"]/weight
    getNames(out)="Planetary Boundary Indicators|Percentage of global land cover converted to cropland"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
  } else if (datasource=="FAO"){
    land<-calcOutput("FAOLand",aggregate = FALSE)
    if(land["SDN","y2011","6601|Land area"]==0){
      land=land[,c("y2011","y2012"),,invert=TRUE]
      vcat(verbosity=3,"FAO Land area not existent for all countries in 2011/2012. Years removed.")
    }
    weight=land[,,"6601|Land area"]
    out1=land[,,"6620|Arable land and Permanent crops"]/weight
    
    getNames(out)=c(
      "Planetary Boundary Indicators|Percentage of global land cover converted to cropland"
    )
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
    
  } else if (datasource=="Rockstroem2009"){
    landshare=new.magpie(names = "Planetary Boundary Indicators|Percentage of global land cover converted to cropland",years=findset("time"),fill = 0.15)
    weight=NULL
    landshare2<-landshare3<-landshare
    landshare2[,,]=0.15
    landshare3[,,]=0.2
    out <- mbind(
      add_dimension(landshare, dim=3.1, add="scenario", nm="boundary"),
      add_dimension(landshare2, dim=3.1, add="scenario", nm="boundary_low"),
      add_dimension(landshare3, dim=3.1, add="scenario", nm="boundary_up")
    )
    
  } else if (datasource=="Rockstroem2009"){
    landshare=new.magpie(names = "Planetary Boundary Indicators|Percentage of global land cover converted to cropland",years=findset("time"),fill = 0.15)
    weight=NULL
    landshare2<-landshare3<-landshare
    landshare2[,,]=0.15
    landshare3[,,]=0.2
    out <- mbind(
      add_dimension(landshare, dim=3.1, add="scenario", nm="boundary"),
      add_dimension(landshare2, dim=3.1, add="scenario", nm="boundary_low"),
      add_dimension(landshare3, dim=3.1, add="scenario", nm="boundary_up")
    )
    
  } else {stop("unknown datasource")}

  
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  return(list(x=out,
              weight=weight,
              min=0,
              max=1,
              unit="Mha/Mha",
              description="Planetary Boundary Indicator for Land cover. Differences to the original indicator are for the 2009 indicators, that we look total land instead of ice-free land (but exclude the antarctis).")
  )
}