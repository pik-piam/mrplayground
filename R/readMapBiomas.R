#' @importFrom magclass read.magpie

readMapBiomas <- function() {
  x <- read.magpie("mapbiomas.mz")
  return(x)
}