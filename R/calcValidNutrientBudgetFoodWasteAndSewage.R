#' @title calcValidNutrientBudgetFoodWasteAndSewage
#' @description Validation Script for Nitrogen Budgets for Livestock production
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
#' calcOutput("ValidNutrientBudgetFoodWasteAndSewage")
#' }
#' 
#' @importFrom magclass clean_magpie
#' @importFrom magpiesets reportingnames
calcValidNutrientBudgetFoodWasteAndSewage<-function(datasource="Bodirsky",nutrient="nr"){
  
  if(datasource=="Bodirsky"){
    x<-calcOutput("FoodWasteAndSewage",aggregate = FALSE)
    past<-findset("past")
    #just select ssp1 scenario for history
    nutrientname<-reportingnames(nutrient)
    x1<-setNames(dimSums(x[,,nutrient],dim=3),nm=paste0("Resources|",nutrientname,"|Food Consumption"))
    x2<-setNames(dimSums(x[,,nutrient][,,"hh_food_waste"],dim=3),nm=paste0("Resources|",nutrientname,"|Food Consumption|+|Household Food Waste"))
    x3<-setNames(dimSums(x[,,nutrient][,,c("urine","feces")],dim=3),nm=paste0("Resources|",nutrientname,"|Food Consumption|+|Intake"))
    #x4<-setNames(dimSums(x[,,nutrient][,,c("urine")],dim=3),nm=paste0("Resources|",nutrientname,"|Food Consumption|Intake|+|Urine"))
    #x5<-setNames(dimSums(x[,,nutrient][,,c("feces")],dim=3),nm=paste0("Resources|",nutrientname,"|Food Consumption|Intake|+|Feces"))
    
    y<-collapseNames(calcOutput("NutrientBudgetSewage",aggregate=FALSE)[,,nutrient])
    y1<-setNames(dimSums(y,dim=3),nm=paste0("Resources|",nutrientname,"|Wastewater"))
    y2<-setNames(y,nm=paste0("Resources|",nutrientname,"|Wastewater|",reportingnames(getNames(y))))
    
    out<-mbind(
      x1,x2,x3,
      #x4,x5
      y1,y2
    )
    
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