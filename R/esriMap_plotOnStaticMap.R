#' @keywords internal
#' @export
#' @import graphics
#' @title Plot a map loaded as a RGB rasterBrick.
#' @param grayscale logical, if TRUE one layer is returned representing grayscale values. If false, three layers 
#' representing red, green, and blue intensity are returned. 
#' @param ... arguments passed on to \code{plot}
#' @return An plot of the map
#' @description The map is plotted using \code{plotRGB} from \pkg{raster}. 
#' @examples
#' map <- esriMap_getMap(-122.3318, 47.668)
#' esriMap_plotOnStaticMap(map)
#' esriMap_plotOnStaticMap(map, grayscale = TRUE)

esriMap_plotOnStaticMap <- function(rasterMap, 
                                    grayscale = FALSE,
                                    ...){
  
  
  argsList <- list(...)
  if(is.null(argsList$interpolate)){argsList$interpolate <- TRUE}
  
  if (grayscale){
    grayImageArray <- round(apply(raster::values(rasterMap), 1, sum)/3)
    map <- raster::raster(rasterMap, 1)
    raster::setValues(map, grayImageArray)
    argsList$x <- map
    if(is.null(argsList$col)){argsList$col <- gray.colors(255, gamma = 1)}
    do.call(plot, argsList)
  } else {
    argsList$x <- rasterMap
    do.call(raster::plotRGB, argsList)
  }
  
}
