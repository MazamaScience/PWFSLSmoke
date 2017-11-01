#' @export
#' @import graphics
#' @title Add Icons to a Map or RgoogleMap Plot
#' @param color marker color; 'red' is currently the only option. 
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param map optional RgoogleMaps map object or Raster* object. 
#' @param expansion icon expansion factor. Ignored if size is specified.
#' @param width icon width in map coordinates 
#' @param height icon height in map coordinates
#' @param ... arguments passed on to \code{rasterImage} 
#' Taken from \code{map} if \code{map} is not null.
#' @description Adds an icon to a plot or \code{map} -- an RgoogleMaps map object. 
#' 
#' @note 
#' 
#' When \code{map} is not null, \code{expansion} will change the icon size based on its actual size. 
#' For basic plots, it may need to be much smaller, perhaps ~ 0.001.
#' 
#' Alternatively, if \code{width} and \code{height} are specified, the marker will be drawn with
#' the specified dimensions, in plot coordinates.
#' 
#' @examples
#' \dontrun{
#' ca <- airnow_load(20160801, 20160831, stateCodes='ca')
#' # Google map
#' map <- monitorGoogleMap(ca)
#' addIcon("orangeFlame", ca$meta$longitude, ca$meta$latitude, map=map, expansion=0.1)
#' # line map
#' monitorMap(ca)
#' addIcon("orangeFlame", ca$meta$longitude, ca$meta$latitude, expansion=0.002)
#' }
#' 


addMarker <- function(longitude, latitude, color = "red", map=NULL, expansion=1,
                      width = NULL, height = NULL, ...) {
  
  
  markerPath <- paste0("marker_",color,".png")
  pngFile <- base::system.file("icons", markerPath, package="PWFSLSmoke")
  
  if ( pngFile == "" ) {
    stop("Cannot find package file 'inst/icons/",markerPath,"'")
  }
  
  # Read in the png file
  marker <- png::readPNG(pngFile)
  
  if ( !is.null(map) ) {
    
    if ( !any( c("staticMap", "RasterBrick", "RasterLayer", "RasterStack") %in% class(map) )  ) {
      stop("'map' argument is not of class 'staticMap' or 'Raster*'")
    }
  }
    
    if ( "staticMap" %in% class(map) ) {
    #RgoogleMap
      
    # limit longitude, latitude to those within bounding box
    lon_lo <- map$BBOX$ll[,'lon']
    lon_hi <- map$BBOX$ur[,'lon']
    lat_lo <- map$BBOX$ll[,'lat']
    lat_hi <- map$BBOX$ur[,'lat']
    
    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask
    
    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]
    
    # Get lat/lon coordinates for marker dimensions
    zoom <- map$zoom
    
    degreesPerPixelEW <- 360/(256*2^zoom) 
    degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*latitude) # R does trigonometry in radians
    
    if ( is.null(width) ) {
      width <- degreesPerPixelEW*dim(marker)[2]*expansion
    } 
    
    if ( is.null(height) ) {
      height <- degreesPerPixelNS*dim(marker)[1]*expansion
    } 
    
    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height
    
    # Get plot coordinates
    
    newXY <- RgoogleMaps::LatLon2XY.centered(map, c(bottom, top), c(left, right))
    bottom <- newXY$newY[1:length(latitude)]
    top <- newXY$newY[-(1:length(latitude))]
    left <- newXY$newX[1:length(longitude)]
    right <- newXY$newX[-(1:length(longitude))]
    
    
  } else if ( any( c("staticMap", "RasterBrick", "RasterLayer", "RasterStack") %in% class(map) ) ) {
    # Raster*
    
    # limit longitude, latitude to those within bounding box
    lon_lo <- extent(map)[1]
    lon_hi <- extent(map)[2]
    lat_lo <- extent(map)[3]
    lat_hi <- extent(map)[4]
    
    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask
    
    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]
    
    
    # Get lat/lon coordinates for marker dimensions
    degreesPerPixelEW <- raster::res(map)[1]
    degreesPerPixelNS <- raster::res(map)[2]
    
    if ( is.null(width) ) {
      width <- degreesPerPixelEW*dim(marker)[2]*expansion
    } 
    
    if ( is.null(height) ) {
      height <- degreesPerPixelNS*dim(marker)[1]*expansion
    } 
    
    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height
    
  } else {
    # basic plot from maps::map()
    
    # limit longitude, latitude to those within bounding box
    usr <- par("usr")
    lon_lo <- usr[1]
    lon_hi <- usr[2]
    lat_lo <- usr[3]
    lat_hi <- usr[4]
    
    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask
    
    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]
    
    # Get width and height
    
    degreesPerInchEW <- (usr[2]-usr[1]) %% 360 / par("pin")[1]
    degreesPerInchNS <- (usr[4]-usr[3]) %% 360 / par("pin")[2]
    
    # estimate conversion from inch to pixel (assuming dpi ~ 96)
    degreesPerPixelEW <- degreesPerInchEW/96
    degreesPerPixelNS <- degreesPerInchNS/96
    
    if ( is.null(width) ) {
      width <- degreesPerPixelEW*dim(marker)[2]*expansion
    } 
    
    if ( is.null(height) ) {
      height <- degreesPerPixelNS*dim(marker)[1]*expansion
    } 
    
    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height
    
  }
    
    
  argsList <- list(...)
  
  argsList$image <- marker
  argsList$xleft <- left
  argsList$ybottom <- bottom
  argsList$xright <- right
  argsList$ytop <- top
  
  do.call(graphics::rasterImage, argsList)
  
}
