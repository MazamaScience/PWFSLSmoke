#' @keywords plotting
#' @export
#' @title Download a Spatial Raster Object from ESRI
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param maptype map type
#' @param zoom map zoom level; corresponds to googleMaps zoom level
#' @param bboxString comma separated string with bounding box (xmin, ymin, xmax, ymax). If not null, centerLon, centerLat, and zoom are ignored.
#' @param bboxSR spatial reference of the bounding box
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the returned map. If the CRS of the downloaded
#' map does not match, it will be projected to the specified CRS using \code{raster::projectRaster}.
#' @param additionalArgs character string with additional arguments to be pasted into the image URL eg. \code{"&rotation=90"}
#' @description Downloads a PNG from ESRI and creates a \code{raster::rasterBrick} object with layers for red, green, and blue.
#' This can then passed as the \code{mapRaster} object to the \code{esriMap_plotOnStaticMap()} function for plotting.
#'
#' Available \code{maptypes} include:
#' \itemize{
#' \item{natGeo}
#' \item{worldStreetMap}
#' \item{worldTopoMap}
#' \item{satellite}
#' \item{deLorme}
#' }
#'
#' Additional base maps are found at:
#' \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/}
#'
#' @note The spatial reference of the image when it is downloaded is 3857. If the crs argument is different, projecting may cause
#' the size and extent of the image to differ very slightly from the input, on a scale of 1-2 pixels or 10^-3 degrees.
#'
#' If bboxString is specified and the bbox aspect ratio does not match the width/height aspect ratio the extent is resized to prevent
#' the map image from appearing stretched, so the map extent may not match the bbox argument exactly.
#' @return A rasterBrick object which can be plotted with \code{esriMap_plotOnStaticMap()} or \code{raster::plotRGB()} and serve as a base plot.
#' @references \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Export_Map/02r3000000v7000000/}
#' @examples
#' \dontrun{
#' map <- esriMap_getMap(-122.3318, 47.668)
#' esriMap_plotOnStaticMap(map)
#' }
#' @seealso \code{\link{esriMap_plotOnStaticMap}}
#'
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


  # Calculate degrees per pixel from zoom to determine bbox:
  # * google maps tiles are 256x256 pixels
  # * zoom level 0 includes 360 degrees-EW
  # * every time zoom level increases, scale halves. Thus, for degrees EW:
  # pixels/degree = 256pixels*2^zoomLevel/360degrees
  # degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
  # degrees-NS/pixel = degreesPerPixelNS*cos(latitude)
  # Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to

  # Create components of webservice URL
  if ( !is.null(centerLon) && !is.null(centerLat) ) {
    degreesPerPixelEW <- 360/(256*2^zoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*centerLat) # R does trigonometry in radians
    lonLo <- centerLon - degreesPerPixelEW*(width/2-.5)
    lonHi <- centerLon + degreesPerPixelEW*(width/2-.5)

    latLo <- centerLat-degreesPerPixelNS*(height/2-.5)
    latHi <- centerLat+degreesPerPixelNS*(height/2-.5)
    bbox <- c(lonLo, latLo, lonHi, latHi)
    bboxString <- paste0(bbox, collapse=",")
    bboxSR <- "4326"
  } else if ( !is.null(bboxString) ) {
    bboxSR <- "4326"
  } else {
    stop("centerLat + centerLon, or bboxString must be specified")
  }

  # NOTE:  Url text for maptype http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/
  # NOTE:   * NatGeo_World_Map = natGeo
  # NOTE:   * World_Street_Map = worldStreetMap
  # NOTE:   * World_Topo_Map = worldTopoMap
  # NOTE:   * Specialty/DeLorme_World_Base_Map = deLorme
  # NOTE:   * World_Imagery = satellite

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

  # Create URLs from components
  baseUrl <- paste0("http://server.arcgisonline.com/arcgis/rest/services/", maptypeText, "/MapServer/export")
  url <- paste0(baseUrl, "?bbox=", bboxString, "&bboxSR=", bboxSR, "&size=", width, ",", height, additionalArgs)
  pngUrl <- paste0(url, "&f=image")
  jsonUrl <- paste0(url, "&f=json")

  # Get ESRI JSON map metadata
  try(logger.info("ESRI json URL: %s", jsonUrl), silent = TRUE)
  response <- httr::GET(jsonUrl)
  if ( httr::http_error(response) ) {
    stop(paste0("ESRI JSON request failed with: ",httr::content(response)))
  }
  mapInfo <- jsonlite::fromJSON(httr::content(response))

  # Get ESRI map png
  try(logger.info("ESRI png URL: %s", pngUrl), silent = TRUE)
  response <- httr::GET(pngUrl)
  if ( httr::http_error(response) ) {
    stop(paste0("ESRI PNG request failed with: ",httr::content(response)))
  }
  imageArray <- httr::content(response, type="image/png")
  try(logger.trace("successfully downloaded ESRI map"), silent = TRUE)
  try(logger.trace("ESRI Map Info: %s", utils::str(mapInfo)), silent = TRUE)

  # Convert PNG into a Raster object
  mapRaster <- raster::brick(ncol=mapInfo$width,
                       nrow=mapInfo$height,
                       nl = 3)
  mapRaster <- raster::setValues(mapRaster, imageArray*255)
  if (width == height) {mapRaster <- raster::t(mapRaster)} # rows and columns are confused when width = height

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
