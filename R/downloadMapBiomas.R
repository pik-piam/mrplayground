#' @importFrom utils download.file unzip

downloadMapBiomas <- function() {
  download.file("https://zenodo.org/record/2655065/files/mapbiomas_lu_historical_data_v3.zip", 
                destfile = "mapbiomas.zip")
  unzip("mapbiomas.zip")
  unlink("mapbiomas.zip")
}