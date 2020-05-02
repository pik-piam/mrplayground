calcValidPlanetaryBoundariesWater<-function(datasource="FAO"){
  if (datasource=="x"){

    out=0
    
    getNames(out)="Planetary Boundary Indicators|Consumptive blue water use"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="history")
  } else if (datasource=="Rockstroem2009"){
    landshare=new.magpie(names = "Planetary Boundary Indicators|",years=findset("time"),
                         fill = 4000)
    weight=NULL
    landshare2<-landshare3<-landshare4<-landshare
    landshare2[,,]=4000
    landshare3[,,]=6000
    landshare4[,,]=NA    
    landshare4[,"y2000",]=2600
    out <- mbind(
      add_dimension(landshare, dim=3.1, add="scenario", nm="boundary"),
      add_dimension(landshare2, dim=3.1, add="scenario", nm="boundary_low"),
      add_dimension(landshare3, dim=3.1, add="scenario", nm="boundary_up"),
      add_dimension(landshare4, dim=3.1, add="scenario", nm="state")
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