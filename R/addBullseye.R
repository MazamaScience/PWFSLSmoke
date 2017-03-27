#' @keywords plotting
#' @export
#' @title Add a Bullseye at the Specified Location
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param map optional RgoogleMaps map object
#' @param cex character expansion
#' @param lwd line width of individual circles
#' @description Draws a bullseye with concentric circles of black and white.
#' @examples
#' wa <- monitor_subset(Northwest_Megafires, stateCodes='WA', tlim=c(20150821,20150828))
#' monitorMap(wa, cex=4)
#' addBullseye(wa$meta$longitude, wa$meta$latitude)

addBullseye <- function(longitude, latitude, map=NULL, cex=2.0, lwd=2) {
  
  if ( !is.null(map) ) {
    # RgoogleMap
    
    if ( !"staticMap" %in% class(map) ) {
      stop("'map' argument is not of class 'staticMap'")
    }
    
    # NOTE:  latitude comes before longitude
    RgoogleMaps::PlotOnStaticMap(map, latitude, longitude, add=TRUE, cex=cex, lwd=lwd, col='black')
    RgoogleMaps::PlotOnStaticMap(map, latitude, longitude, add=TRUE, cex=cex*0.75, lwd=lwd, col='white')
    RgoogleMaps::PlotOnStaticMap(map, latitude, longitude, add=TRUE, cex=cex*0.5, lwd=lwd, col='black')
    
  } else {
    # basic plot from maps::map()
    points(longitude, latitude, pch=1, cex=cex, lwd=lwd, col='black')
    points(longitude, latitude, pch=1, cex=cex*0.75, lwd=lwd, col='white')
    points(longitude, latitude, pch=1, cex=cex*0.5, lwd=lwd, col='black')
    
  }
  
}

