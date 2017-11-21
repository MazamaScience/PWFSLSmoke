#' @keywords plotting
#' @export
#' @title Create a RGB spatial raster from arcGIS REST
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param maptype map type. natGeo, worldStreetMap, worldTopoMap, satellite, or deLorme. Also accepts
#' map server identity, found at \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/}
#' @param zoom map zoom level; corresponds to googleMaps zoom level
#' @param bboxString comma separated string with bounding box (xmin, ymin, xmax, ymax). If not null, centerLon, centerLat, and zoom are ignored.
#' @param bboxSR spatial reference of the bounding box
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the returned map. If the CRS of the downloaded
#' map does not match, it will be projected to the specified CRS using \code{raster::projectRaster}.
#' @param additionalArgs additional arguments to be pasted into the image URL eg. \code{"&rotation=90"}
#' @note The spatial reference of the image when it is downloaded is 3857. If the crs argument is different, projecting may cause 
#' the size and extent of the image to differ very slightly from the input, on a scale of 1-2 pixels or 10^-3 degrees. 
#' 
#' If bboxString is specified and the bbox aspect ratio does not match the width/height aspect ratio the extent is resized to prevent
#' the map image from appearing stretched, so the map extent may not match the bbox argument exactly.
#' @return A rasterBrick object which can be plotted with \code{raster::plotRGB} and serve as a base plot.
#' @description Creates a rasterBrick using the \pkg{raster} package with layers for red, green, and blue color intensity.
#' @references \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Export_Map/02r3000000v7000000/}
#' @examples
#' \dontrun{
#' map <- esriMap_getMap(-122.3318, 47.668)
#' esriMap_plotOnStaticMap(map)
#' }

esriMap_getMap <- function(centerLon = NULL, 
                           centerLat = NULL,
                           bboxString = NULL,
                           bboxSR = "4326",
                           maptype = "worldStreetMap",
                           zoom = 12, 
                           width = 640,
                           height = 640,
                           crs = sp::CRS("+init=epsg:4326"),
                           additionalArgs = NULL) {
  
  
  # calculate degrees per pixel from zoom to determine bbox: 
  # * google maps tiles are 256x256 pixels
  # * zoom level 0 includes 360 degrees-EW
  # * every time zoom level increases, scale halves. Thus, for degrees EW:
  # pixels/degree = 256pixels*2^zoomLevel/360degrees 
  # degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
  # degrees-NS/pixel = degreesPerPixelNS*cos(latitude)
  # Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to
  
  # TODO:  Improve logic
  # TODO:  if (lat AND lon) { ... } else if (bboxString) { ... } else { ERROR }
  if ( is.null(centerLat) && is.null(centerLon) && is.null(bboxString) ) {
    stop("centerLat, centerLon, or bboxString must be specified")
  }
  
  # TODO:  Merge this if block into the logic above
  if ( is.null(bboxString) ) {
    degreesPerPixelEW <- 360/(256*2^zoom) 
    degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*centerLat) # R does trigonometry in radians
    lonLo <- centerLon - degreesPerPixelEW*(width/2-.5)
    lonHi <- centerLon + degreesPerPixelEW*(width/2-.5)
    
    latLo <- centerLat-degreesPerPixelNS*(height/2-.5)
    latHi <- centerLat+degreesPerPixelNS*(height/2-.5)
    bbox <- c(lonLo, latLo, lonHi, latHi)
    bboxSR <- "4326"
  } 
  bboxString <- paste0(bbox, collapse = ",")
  
  # url text for maptype http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/
  # NatGeo_World_Map = natGeo
  # World_Street_Map = worldStreetMap
  # World_Topo_Map = worldTopoMap
  # Specialty/DeLorme_World_Base_Map = deLorme
  # World_Imagery = satellite
  
  if ( maptype == "natGeo" ) {
    maptypeText <- "NatGeo_World_map"
  } else if ( maptype == "worldStreetMap" ) {
    maptypeText <- "World_Street_Map"
  } else if ( maptype == "worldTopoMap" ) {
    maptypeText <- "World_Topo_Map"
  } else if ( maptype == "deLorme" ) {
    maptypeText <- "Specialty/DeLorme_World_Base_Map"
  } else if ( maptype == "satellite" ) {
    maptypeText <- "World_Imagery"
  } else {
    maptypeText <- maptype
  }
  
  # Apply arguments to url
  
  # TODO:  Naming consistency: url, baseUrl, pngUrl, jsonUrl
  baseUrl <- paste0("http://server.arcgisonline.com/arcgis/rest/services/", maptypeText, "/MapServer/export")
  url <- paste0(baseUrl, "?bbox=", bboxString, "&bboxSR=", bboxSR, "&size=", width, ",", height, additionalArgs)
  pngUrl <- paste0(url, "&f=image")
  jsonUrl <- paste0(url, "&f=json")
  
  # TODO:  Always use httr package to download stuff
  # TODO:  Much of the code below here can be replaced with:
  # TODO:
  # TODO:  response <- httr::GET(jsonUrl)
  # TODO:  # check response$status_code and issue an intelligible error message if needed
  # TODO:  mapInfo <- jsonlite::fromJSON(httr::content(response))
  # TODO:  response <- httr::GET(pngUrl)
  # TODO:  # check status_code
  # TODO:  array <- httr::content(response)
  # TODO:  mapRaster <- raster::rasterImamge(array, ...)
  
  # TODO:  Test each web request by checking out 

  # Download and load PNG and metadata
  
  dirName <- paste("esriMap", centerLat, centerLon, sep = "_")
  if ( dir.exists(dirName) ) { dirName <- paste0(dirName, "(temp)") }
  dir.create(dirName)
  
  utils::download.file(pngUrl, paste0(dirName, "/image.png"))
  utils::download.file(jsonUrl, paste0(dirName, "/info.json"))
  
  imageArray <- png::readPNG(paste0(dirName, "/image.png"))
  mapInfoJSON <- readr::read_file(paste0(dirName, "/info.json"))
  
  unlink(dirName, recursive = TRUE)
  
  mapInfo <- jsonlite::fromJSON(mapInfoJSON)
  
  # Create raster with RBG layers and matching metadata
  
  mapRaster <- raster::brick(ncol=mapInfo$width, 
                       nrow=mapInfo$height,
                       nl = 3)
  mapRaster <- raster::setValues(mapRaster, imageArray*255)
  mapRaster <- raster::t(mapRaster) # correct because columns from the array are read in as rows
  
  names(mapRaster) <- c("red", "green", "blue")
  
  raster::extent(mapRaster) <- c(mapInfo$extent$xmin, mapInfo$extent$xmax, mapInfo$extent$ymin, mapInfo$extent$ymax)
  raster::crs(mapRaster) <- sp::CRS(paste0("+init=epsg:",mapInfo$extent$spatialReference$latestWkid)) 
  
  
  if ( rgdal::CRSargs(crs) != rgdal::CRSargs(sp::CRS(paste0("+init=epsg:", mapInfo$extent$spatialReference$latestWkid))) ) {
    mapRaster <- raster::projectRaster(mapRaster, crs = crs, method = "ngb")
    
    # trim any extra NA on the edges generated by projectRaster
    
    mapRaster <- raster::trim(mapRaster)
    
  }
  
  return(mapRaster)
  
}
