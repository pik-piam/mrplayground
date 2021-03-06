## to do's:
# base function on FAOharm
# best way to replace NAs. currently only NAs in weight are replaces, which leads to many NAs in results
# check whether globally summed values agree with global values in dataset
# due to severeral aggregations: once sectoral, once regional there will be a strong propagation of NAs that are then replaced by 0 and cannot be seen
# fibre crops are currently missing any aggregation: update that in the aggregation matrix

## wishlist for toolAggregate:
# no propagation of NAs


## new version is based on FAOharm















#' Calculate FAO Crop aggregated
#' 
#' Provides the FAOSTAT Production Crops data aggregated to magpie kcr.
#' 
#' 
#' @return FAO land data and corresponding weights as a list of two MAgPIE
#' objects
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' 
#' a <- calcOutput("FAOCrop_aggr")
#' 
#' }
#' @importFrom utils read.csv
#' @importFrom magclass fulldim getNames<-

calcFAOCrop_aggr <- function() {
  
## read in the files
CropPrim <- readSource("FAO", "Crop")
Fodder <- readSource("FAO", "Fodder")
data <- toolFAOcombine(CropPrim, Fodder)

if (any( grepl("+ (Total)",getNames(data, fulldim=T)[[1]], fixed = TRUE))) {
  data <- data[,,"+ (Total)", pmatch=T, invert=T]
}

aggregation <- toolGetMapping("FAOitems.csv", type = "sectoral", where="mappingfolder")

data[is.na(data)] <- 0

FAO_out <- toolAggregate(data, rel=aggregation, from="ProductionItem", to="k", dim=3.1, partrel = TRUE)


if(any(fulldim(FAO_out)[[2]][[3]]=="")) {
  if (sum(FAO_out[,,""]) == 0) {
    FAO_out <- FAO_out[,,"", invert=T]
  } else  {
    vcat(1,'Aggregation created entries without name (""), but containing information. This should not be the case.')
  }
}

if(any(getNames(FAO_out)=="remaining.production")) {
  remain_prod <- mean( dimSums(FAO_out[,,"remaining.production"], dim=1)/dimSums(dimSums(FAO_out[,,"production"], dim=3), dim=1) )
  if (remain_prod > 0.02) vcat(1,"Aggregation created a 'remaining' category. Production is", round(remain_prod,digits = 3)*100, "% of total \n")
}
if(any(getNames(FAO_out)=="remaining.area_harvested")) {
  remain_area <- mean( dimSums(FAO_out[,,"remaining.area_harvested"], dim=1)/dimSums(dimSums(FAO_out[,,"area_harvested"], dim=3), dim=1) )
  if (remain_area > 0.02) vcat(1,"Aggregation created a 'remaining' category. The area harvested is", round(remain_area,digits = 3)*100, "% of total \n")
}

## recalculate the Yields where production and area information is available
cat("Yields from Crop_aggr should be used with caution.")
yieldelement <- intersect(getNames(FAO_out[,,"production"], fulldim=T)[[1]],getNames(FAO_out[,,"area_harvested"], fulldim=T)[[1]])
Yields <- FAO_out[,,paste(yieldelement,sep=".","production")]/FAO_out[,,paste(yieldelement,sep=".","area_harvested")]
Yields <- collapseNames(Yields)
getNames(Yields) <- paste(getNames(Yields), sep=".", "Yield_(t/ha)")
FAO_out <- mbind(FAO_out, Yields)


## area as weight for Yields
weight <- FAO_out
weight[,,] <- 0
weight[,,"Yield_(t/ha)"] <- FAO_out[,,"area_harvested"]
# weight[is.na(weight)] <- 0
# maybe set weight for yields to 0 whereever production, area or yield is 0, to avoid that not existing yield information is influencing results


  return(list(x=FAO_out,weight=weight, unit="if not specified differently in the dimension name in tonnes, area harvested in ha", description="FAO Crop Production information aggregated to magpie categories"))
}

