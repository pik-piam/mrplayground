#' @title calcValidNutrientWasteBudget
#' @description Report waste flows of nutrients from differents sources, including Household waste, slaughterwaste and Processingwaste
#'
#' @param datasource Bodirsky for own calculations
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidNutrientBudgetSewage}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNutrientWasteBudget")
#' }
#' 
#' @importFrom magpiesets findset
#' @importFrom magpiesets reportingnames

calcValidNutrientWasteBudget<-function(datasource="Bodirsky",nutrient="nr"){
  if (length(nutrient)!=1) {stop("nutrient should only contain one element")}
  if(datasource=="Bodirsky"){
    
    out<-calcOutput("NutrientWasteBudget",aggregate = FALSE,nutrient=nutrient)
    getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
    getNames(out)<-paste0("Resources|",reportingnames(nutrient),"|Waste|+|",getNames(out))
    out2<-setNames(dimSums(out,dim=3),paste0("Resources|",reportingnames(nutrient),"|Waste"))
    out<-mbind(out2,out)
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