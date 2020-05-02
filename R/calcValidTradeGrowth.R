#' @title calcValidTradeGrowth
#' @description calculate historical trade growth rate
#' 
#' 
#' @param datasource which data to use
#' @param detail if true, provides results for all commodities, otherwhise aggregates some groups
#' @param base_year y1995
#' @return Trade growth rates (index)
#' @author Xiaoxi Wang
#' @examples
#' 
#'   \dontrun{
#'     x <- calcValidTradeGrowth(datasource = "FAO", detail = TRUE,base_year ="y1995")
#'   }
#' @importFrom  magpiesets findset

calcValidTradeGrowth <- function(datasource = "FAO", detail = TRUE,base_year ="y1995"){
  x <- calcOutput("ValidTrade",supplementary = TRUE)
  isocountries  <- x$isocountries
  x <- x$x
  if (any(grepl("\\+\\|", getNames(x)))) getNames(x) <- gsub("\\+\\|","",getNames(x))
  getNames(x) <- gsub("Net-Trade","Trade growth rate",getNames(x))
  if(is.null(base_year)) base_year <- "y1995"
  x <- round(x,2)/setYears(round(x[,base_year,],2),NULL)
  x[(is.nan(x))] <- 0
  x[(is.infinite(x))] <- 0
  
  weight <- round(x[,base_year,],2)
  description <- "Trade growth rate"
  return(list(x=x,weight=weight,description =description,isocountries = isocountries)) 
}