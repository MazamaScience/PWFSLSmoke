#' @keywords plotting
#' @export
#' @title Plot a map from a RGB rasterBrick.
#' @param mapRaster a RGB rasterBrick object. It is assumed that layer 1 represents red, layer 2 represents
#' gree, and layer 3 represents blue. 
#' @param grayscale logical, if TRUE one layer is plotted with grayscale values. If FALSE, a color
#' map is plotted from red, green, and blue colors. 
#' @param ... arguments passed on to \code{plot} (for grayscale = TRUE) or \code{plotRGB} (for grayscale = FALSE)
#' @description The map is plotted using \code{plotRGB} from \pkg{raster}. 
#' @return An plot of the map
#' @examples
#' \dontrun{
#' map <- esriMap_getMap(-122.3318, 47.668)
#' esriMap_plotOnStaticMap(map)
#' esriMap_plotOnStaticMap(map, grayscale = TRUE)
#' }
#' @seealso \code{\link{esriMap_getMap}}

esriMap_plotOnStaticMap <- function(mapRaster, 
                                    grayscale = FALSE,
                                    ...){
  
  
  argsList <- list(...)
  if ( is.null(argsList$interpolate) ) { argsList$interpolate <- TRUE }
  
  if ( grayscale ) {
    singleLayerMap <- raster::raster(mapRaster, 1) 
    grayImageArray <- round(apply(raster::values(mapRaster), 1, sum)/3)
    raster::setValues(singleLayerMap, grayImageArray)
    argsList$x <- singleLayerMap
    if ( is.null(argsList$col) ) { argsList$col <- grDevices::gray.colors(255, gamma = 1) }
    do.call(sp::plot, argsList)
  } else {
    argsList$x <- mapRaster
    do.call(raster::plotRGB, argsList)
  }
  
}
