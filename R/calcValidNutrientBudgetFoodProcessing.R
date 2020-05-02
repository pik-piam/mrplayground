#' @title calcValidNutrientBudgetFoodProcessing
#' @description Validation Script for Nitrogen Budgets for Food Processing (processing from fooduse to food, not processing from one product to another)
#'
#' @param datasource Bodirsky for own calculations
#' @param nutrient The nutrient in which the results shall be reported.
#' @param detail shall the funciton reportinghelper provide detailed commodities or only commodity groups?
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' \code{\link{calcNutrientBudgetFoodProcessing}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNutrientBudgetFoodProcessing")
#' }
#' 
#' @importFrom magpiesets reportingnames reporthelper
calcValidNutrientBudgetFoodProcessing<-function(datasource="Bodirsky",nutrient="nr",detail=FALSE){
  
  if(datasource=="Bodirsky"){
    budget<-calcOutput("NutrientBudgetFoodProcessing",aggregate = FALSE)[,,nutrient]
    budget<-dimOrder(budget,c(3,1,2))
    out<-reporthelper(budget,dim = 3.3,detail=detail)
    getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
    getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
    dimnames(out)[[3]]<-gsub(dimnames(out)[[3]],pattern = "\\.",replacement = "|")
    dimnames(out)[[3]]<-gsub(dimnames(out)[[3]],pattern = "All products",replacement = "")
    dimnames(out)[[3]]<-gsub(dimnames(out)[[3]],pattern = "\\|\\|",replacement = "|")
    out<-summationhelper(out)
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
              description="Nitrogen Budgets for Food Processing (processing from fooduse to food, not processing from one product to another)")
         )
}