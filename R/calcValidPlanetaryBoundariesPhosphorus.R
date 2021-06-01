#' @title calcValidPlanetaryBoundariesPhosphorus
#' @description Provides comparison for the planetary boundary indicators for phosphorus
#' @param datasource datasource to compare to. Historical trajectories of the indicators, or the planetary boundary by the Rockstroem Paper 2009 or the Steffen et al paper 2015.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidPlanetaryBoundariesPhosphorus",datasource="Rockstroem2009",aggregate=FALSE)
#' }
#' 
#' @importFrom magclass getSets


calcValidPlanetaryBoundariesPhosphorus<-function(datasource="Rockstroem2009"){
  warning("in preparation. function not yet usable")
  if (datasource=="Steffen2014"){

    out=calcOutput("NitrogenBudgetCropland")
    out=dimSums(out[,,])
    
    getNames(out)="Planetary Boundary Indicators|Industrial and intentional biological fixation of Nr"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
  } else if (datasource=="Steffen2014"){
    boundary=new.magpie(names = "Planetary Boundary Indicators|Industrial and intentional biological fixation of Nr",years=findset("time"),
                         fill = 4000)
    weight=NULL
    boundary2<-boundary3<-boundary4<-boundary
    boundary2[,,]=62
    boundary3[,,]=82
    boundary4[,,]=NA    
    boundary4[,"y2000",]=150
    out <- mbind(
      add_dimension(boundary, dim=3.1, add="scenario", nm="boundary"),
      add_dimension(boundary2, dim=3.1, add="scenario", nm="boundary_low"),
      add_dimension(boundary3, dim=3.1, add="scenario", nm="boundary_up"),
      add_dimension(boundary4, dim=3.1, add="scenario", nm="state")
    )
    
  } else {stop("unknown datasource")}
  
  
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  return(list(x=out,
              weight=weight,
              min=0,
              max=1,
              unit="km^3",
              description="Planetary Boundary Indicator for Land cover")
  )
}