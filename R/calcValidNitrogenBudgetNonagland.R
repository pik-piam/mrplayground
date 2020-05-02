#' @title calcValidNitrogenBudgetNonagland
#' @description Validation Script for Nitrogen Budgets on Non-Agricultural Land 
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
#' calcOutput("ValidNitrogenBudgetNonagland")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNitrogenBudgetNonagland<-function(datasource="Bodirsky"){
  
  if(datasource=="Bodirsky"){
    budget<-calcOutput("NitrogenBudgetNonagland",aggregate = FALSE)
    budget=dimSums(budget,dim=3.1)
    
    all<-getNames(budget)
    withdrawaltypes=NULL
    balancetypes<-c("accumulation","surplus","balanceflow")
    inputtypes<-setdiff(setdiff(all,withdrawaltypes),balancetypes)
    
    tmp<-budget[,,inputtypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Non-Agricultural Land Budget|Inputs|+|",reportingnames(getNames(tmp)))
    inputs<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Non-Agricultural Land Budget|Inputs"),
      tmp
    )
    
    
    tmp<-budget[,,balancetypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Non-Agricultural Land Budget|Balance|+|",reportingnames(getNames(tmp)))
    balance<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Non-Agricultural Land Budget|Balance"),
      tmp
    )
    
    out<-mbind(
      inputs,
      balance
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  
  unit="Mt Nr/yr"
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Nitrogen Budget of Non-Agricultural Land")
         )
}