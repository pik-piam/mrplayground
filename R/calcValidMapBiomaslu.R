#' @title calcValidMapBiomaslu
#' @description Integrates the MapBiomas landuse-dataset.
#' 
#' @return List of magpie objects with results for Brazil, unit and description.
#' @author Geanderson Ambrosio
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidMapBiomaslu")
#' }
#' 
#' @importFrom madrat readSource
#' @importFrom magclass getNames collapseNames add_dimension
#' @importFrom magpiesets reportingnames

calcValidMapBiomaslu <- function () {
  MapBiomaslu <- readSource("MapBiomas")
  data <- collapseNames(MapBiomaslu[, , c("natural_forest","managed_forest","forest","cropland","pastureland","other_land","urban")])
  getNames(data) <- c("natrforest","forestry","forest", "crop", "past", "other", "urban")
  out <- data[, , c("natrforest","forestry","forest", "crop", "past", "other", "urban")]
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "MapBiomas")
  getNames(out, dim = 3)[c(1,2)] <- paste0("Resources|Land Cover|Forest|+|",reportingnames(getNames(out, dim = 3)[c(1,2)]), " (million ha)")
  getNames(out, dim = 3)[c(3:7)] <- paste0("Resources|Land Cover|+|",reportingnames(getNames(out, dim = 3)[c(3:7)]), " (million ha)")
  names(dimnames(out))[1] <- "ISO"
  names(dimnames(out))[2] <- "Year"
  names(dimnames(out))[3] <- "scenario.model.variable"
  return(list(x = out, weight = NULL, unit = "million ha",min = 0, description = "Brazilian land use historical data"))
}