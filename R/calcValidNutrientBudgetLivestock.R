#' @title calcValidNutrientBudgetLivestock
#' @description Validation Script for Nitrogen Budgets for Livestock production
#'
#' @param datasource Bodirsky for own calculations
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidSNUpE}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNutrientBudgetLivestock")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNutrientBudgetLivestock<-function(datasource="Bodirsky",nutrient="nr"){
  
  if(datasource=="Bodirsky"){
    mb<-calcOutput("FAOmassbalance",aggregate = F)[,,nutrient]
    kli<-findset("kli")
    mb1<-collapseNames(mb[,,kli][,,"production"],collapsedim = 2)
    mb2<-dimSums(mb[,,c("feed_fish","feed_livst_chick","feed_livst_egg",      
                "feed_livst_milk","feed_livst_pig","feed_livst_rum" )],dim=3.1)
    
    getNames(mb2)<-substring(getNames(mb2),6)
    mb2<-mb2[,,kli]
    
    slaughtermass<-calcOutput("Slaughtermass",aggregate=FALSE)[,,kli][,,nutrient]
    mb3<-slaughtermass-mb1
    
    out<-mbind(
      add_dimension(mb1,dim = 3.1,nm = reportingnames("kli")),
      add_dimension(mb2,dim = 3.1,nm = reportingnames("excretion")),
      add_dimension(mb3,dim = 3.1,nm = reportingnames("slaughterwaste"))
    )
    out<-dimOrder(out,c(3,1,2))
    
    getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
    getNames(out,dim=3)<-reportingnames(getNames(out,dim=3))
    getNames(out,dim=2)<-paste0("Livestock Budget|",getNames(out,dim=2))
    
    out2<-dimSums(out,dim=3.3)
    
    dimnames(out)[[3]]<-gsub(dimnames(out)[[3]],pattern = "\\.",replacement = "|")
    out<-summationhelper(out)
    dimnames(out2)[[3]]<-gsub(dimnames(out2)[[3]],pattern = "\\.",replacement = "|")
    out<-mbind(out2,out)
    getNames(out,dim=1)<-paste0("Resources|",getNames(out,dim=1))
    out<-clean_magpie(out)

    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  if(nutrient=="dm"){unit="Mt DM/yr"
  } else if (nutrient=="nr"){unit="Mt Nr/yr"
  } else if (nutrient=="p"){unit="Mt P/yr"
  } else if (nutrient=="k"){unit="Mt K/yr"
  } else if (nutrient=="ge"){unit="PJ/yr"
  } else if (nutrient=="wm"){unit="Mt WM/yr"}
  
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Livestock Budget")
         )
}