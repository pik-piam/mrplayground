#' @title calcValidNutrientBudgetSewage
#' @description Validation Script for Nitrogen and Phosphorus Budgets in Sewage Systems
#'
#' @param datasource Bodirsky for own calculations, Lassaletta2014 for a country dataset from 
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Pasture.
#' Environmental Research Letters.
#' FAO for some N related parameters published in FAOSTAT.
#' @param nutrient nitrogen (nr) or phosphorus (p)
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidSNUpE}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNutrientBudgetSewage")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNutrientBudgetSewage<-function(datasource="Bodirsky",nutrient="nr"){
  
  if(datasource=="Bodirsky"){
    budget<-collapseNames(calcOutput("NutrientBudgetSewage",aggregate = FALSE)[,,nutrient])
    names_x<-reportingnames(getNames(budget))
    names(names_x)<-NULL
    getNames(budget)<-paste0("Resources|",reportingnames(nutrient),"|Sewage Budget|",names_x)
    out<-budget
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  if(nutrient=="nr"){
    unit="Mt Nr/yr"
  } else if(nutrient=="p"){
    unit="Mt P/yr"
  } else {unit=nutrient}
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Sewage Budget")
         )
}
