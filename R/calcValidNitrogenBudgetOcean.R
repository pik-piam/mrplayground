#' @title calcValidNitrogenBudgetOcean
#' @description Validation Script for Nitrogen Budgets for Oceans 
#'
#' @param datasource Bodirsky for own calculations
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidSNUpE}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNitrogenBudgetOcean")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNitrogenBudgetOcean<-function(datasource="Bodirsky"){
  
  if(datasource=="Bodirsky"){
    budget<-calcOutput("NitrogenBudgetOcean",aggregate = FALSE,deposition="Nsurplus2",leaching="Nsurplus2")
    names_x<-reportingnames(getNames(budget))
    names(names_x)<-NULL
    getNames(budget)<-paste0("Resources|Nitrogen|Ocean Budget|",names_x)
    out<-budget
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  unit="Mt Nr/yr"
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Non-Agricultural Land Nitrogen Budget")
         )
}