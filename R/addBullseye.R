#' @keywords plotting
#' @export
#' @title Add a Bullseye at the Specified Location
#' @param lon vector of longitudes
#' @param lat vector of latitudes
#' @param map optional RgoogleMaps map object
#' @param cex character expansion
#' @param lwd line width of individual circles
#' @description Draws a bullseye with concentric circles of black and white.
#' @examples
#' \dontrun{
#' # Google map
#' map <- monitorGoogleMap(ca)
#' addBullseye(ca$meta$longitude, ca$meta$latitude, map=map)
#' # line map
#' monitorMap(ca)
#' addBullseye(ca$meta$longitude, ca$meta$latitude)
#' }

addBullseye <- function(lon, lat, map=NULL, cex=2.0, lwd=2) {
  
  if ( !is.null(map) ) {
    # RgoogleMap
    
    if ( !"staticMap" %in% class(map) ) {
      stop("'map' argument is not of class 'staticMap'")
    }
    
    # NOTE:  latitude comes before longitude
    RgoogleMaps::PlotOnStaticMap(map, lat, lon, add=TRUE, cex=cex, lwd=lwd, col='black')
    RgoogleMaps::PlotOnStaticMap(map, lat, lon, add=TRUE, cex=cex*0.75, lwd=lwd, col='white')
    RgoogleMaps::PlotOnStaticMap(map, lat, lon, add=TRUE, cex=cex*0.5, lwd=lwd, col='black')
    
  } else {
    # basic plot from maps::map()
    points(lon, lat, pch=1, cex=cex, lwd=lwd, col='black')
    points(lon, lat, pch=1, cex=cex*0.75, lwd=lwd, col='white')
    points(lon, lat, pch=1, cex=cex*0.5, lwd=lwd, col='black')
    
  }
  
}

