#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create a RGB spatial raster from arcGIS REST
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level; corresponds to googleMaps zoom level
#' @param maptype map type. natGeo, worldStreetMap, worldTopoMap, satellite, or deLorme, or any available map server identity 
#' found at \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/}
#' @param grayscale logical, if TRUE one layer is returned representing grayscale values. If false, three layers 
#' representing red, green, and blue intensity are returned. 
#' @param ... arguments passed on to methods
#' @return A rasterBrick raster object which can be plotted with \code{plotRGB} and serve as a base plot.
#' @description Creates a rasterBrick using the \pkg{raster} package with layers for red, green, and blue or gray color intensity.
#'
#'@references \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Export_Map/02r3000000v7000000/}
#' @examples
#' map <- esriMap_getMap(-122.3318, 47.668)
#' raster::plotRGB(map)
#' map <- esriMap_getMap(-122.3318, 47.668, crs = sp::CRS("+init=epsg:4326"))
#' raster::plotRGB(map)

esriMap_getMap <- function(centerLon, 
                           centerLat,
                           mapType = "worldStreetMap", 
                           zoom = 12, 
                           width = 640,
                           height = 640,
                           crs = sp::CRS("+init=epsg:3857"), 
                           grayscale = FALSE,
                           ...){
  
  
  # calculate degrees per pixel from zoom to determine bbox: 
  # * google maps tiles are 256 pixels wide
  # * zoom level 0 includes 360 degrees-EW
  # * every time zoom level increases, scale halves. Thus, for degrees EW:
  # pixels/degree = 256pixels*2^zoomLevel/360degrees 
  # degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
  # degrees-NS/pixel = degreesPerPixelNS*cos(latitude)
  # Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to
  
  degreesPerPixelEW <- 360/(256*2^zoom) 
  degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*centerLat) # R does trigonometry in radians
  lonlo <- centerLon - degreesPerPixelEW*width/2
  lonhi <- centerLon + degreesPerPixelEW*width/2
  
  latlo <- centerLat-degreesPerPixelNS*height/2
  lathi <- centerLat+degreesPerPixelNS*height/2
  bbox <- paste(lonlo, latlo, lonhi, lathi, sep = ",")
  
  # url text for mapType http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/
  # NatGeo_World_Map = natGeo
  # World_Street_Map = worldStreetMap
  # World_Topo_Map = worldTopoMap
  # Specialty/DeLorme_World_Base_Map = deLorme
  # World_Imagery = satellite
  
  if ( mapType == "natGeo"){
    mapTypeText <- "NatGeo_World_map"
  } else if(mapType == "worldStreetMap"){
    mapTypeText <- "World_Street_Map"
  } else if ( mapType == "worldTopoMap"){
    mapTypeText <- "World_Topo_Map"
  } else if ( mapType == "deLorme" ) {
    mapTypeText <- "Specialty/DeLorme_World_Base_Map"
  } else {
    stop("invalid mapType")
  }
  
  # Apply arguments to url
  
  baseurl <- paste0("http://server.arcgisonline.com/arcgis/rest/services/", mapTypeText, "/MapServer/export")
  url <- paste0(baseurl, "?bbox=", bbox, "&bboxSR=4326&size=",width,",",height)
  urlPNG <- paste0(url, "&f=image")
  urlJSON <- paste0(url, "&f=json")
  
  # Download and load PNG and metadata
  
  dirName <- paste("esriMap", centerLat, centerLon, sep = "_")
  dir.create(dirName)
  
  download.file(urlPNG, paste0(dirName, "/image.png"))
  download.file(urlJSON, paste0(dirName, "/info.json"))
  
  imageArray <- png::readPNG(paste0(dirName, "/image.png"))
  mapInfoJSON <- readr::read_file(paste0(dirName, "/info.json"))
  
  unlink(dirName, recursive = TRUE)
  
  mapInfo <- jsonlite::fromJSON(mapInfoJSON)
  
  if (grayscale){
    grayImageArray <- (imageArray[,,1]+imageArray[,,2]+imageArray[,,3])/3 
    map <- raster::raster(ncol=mapInfo$width, 
                          nrow=mapInfo$height,
                          xmn=mapInfo$extent$xmin, 
                          xmx=mapInfo$extent$xmax, 
                          ymn=mapInfo$extent$ymin, 
                          ymx=mapInfo$extent$ymax, 
                          crs=sp::CRS(paste0("+init=epsg:",mapInfo$extent$spatialReference$latestWkid)))
    raster::values(map) <- grayImageArray*255
  } else {
    map <- raster::brick(ncol=mapInfo$width, 
                         nrow=mapInfo$height,
                         nl = 3,
                         xmn=mapInfo$extent$xmin, 
                         xmx=mapInfo$extent$xmax, 
                         ymn=mapInfo$extent$ymin, 
                         ymx=mapInfo$extent$ymax, 
                         crs=sp::CRS(paste0("+init=epsg:",mapInfo$extent$spatialReference$latestWkid)))
    raster::values(map) <- imageArray*255
    map <- raster::t(map)
  }
  
  # The planes are interpreted in the sequence
  # red, green, blue, alpha.
  
  if ( rgdal::CRSargs(crs) != CRSargs(sp::CRS(paste0("+init=epsg:", mapInfo$extent$spatialReference$latestWkid))) ) {
    map <- raster::projectRaster(map, crs = crs)
  }
  
  return(map)
  
}
