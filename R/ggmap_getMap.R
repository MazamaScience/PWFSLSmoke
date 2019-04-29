#' @keywords plotting
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download a Spatial Raster Object from ESRI
#'
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param maptype map type
#' @param zoom map zoom level; corresponds to \code{ggmap::get_map()} zoom level
#' @param bboxString comma separated string with bounding box (xmin, ymin, xmax,
#' ymax). If not null, centerLon, centerLat, and zoom are ignored.
#' @param bboxSR spatial reference of the bounding box
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the
#' returned map. If the CRS of the downloaded map does not match, it will be
#' projected to the specified CRS using \code{raster::projectRaster}.
#' @param source Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps
#' ("stamen")
#'
#' @description Downloads a PNG from a map tile server and creates a
#' \code{raster::rasterBrick} object with layers for red, green, and blue. This
#' can then passed as the \code{mapRaster} object to the
#' \code{esriMap_plotOnStaticMap()} function for plotting.
#'
#' @note The spatial reference of the image when it is downloaded is 3857. If
#' the crs argument is different, projecting may cause the size and extent of
#' the image to differ very slightly from the input, on a scale of 1-2 pixels or
#' 10^-3 degrees.
#'
#' If bboxString is specified and the bbox aspect ratio does not match the
#' width/height aspect ratio the extent is resized to prevent the map image from
#' appearing stretched, so the map extent may not match the bbox argument exactly.
#'
#' @return A rasterBrick object which can be plotted with
#' \code{esriMap_plotOnStaticMap()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' map <- ggmap_getMap(-122.3318, 47.668)
#' esriMap_plotOnStaticMap(map)
#' }
#' @seealso \code{\link{esriMap_plotOnStaticMap}}

ggmap_getMap <- function(
  centerLon = NULL,
  centerLat = NULL,
  bboxString = NULL,
  bboxSR = "4326",
  maptype = "terrain",
  zoom = 12,
  width = 640,
  height = 640,
  crs = sp::CRS("+init=epsg:4326"),
  source = "stamen"
) {

  # ===== DEBUGGING ============================================================

  if ( FALSE ) {

    centerLon <- -120
    centerLat <- 47
    bboxString <- NULL
    bboxSR <- "4326"
    maptype <- "terrain"
    zoom <- 10
    width <- 640
    height <- 640
    crs <- sp::CRS("+init=epsg:4326")
    source <- "stamen"

  }

  # ----- Validate Parameters --------------------------------------------------

  validMapTypes <- c("terrain", "terrain-background", "satellite", "roadmap",
                     "hybrid", "toner", "watercolor", "terrain-labels",
                     "terrain-lines", "toner-2010", "toner-2011",
                     "toner-background", "toner-hybrid", "toner-labels",
                     "toner-lines", "toner-lite")

  validSources <- c("google", "osm", "stamen")

  # ----- Determine Bounding Box -----------------------------------------------

  # TODO:  We want a square bounding box and will probably need a different
  # TODO:  calculation for stamen maps

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

  # ----- Get Map Image --------------------------------------------------------

  ggmap_obj <- get_stamenmap(
    bbox = c(left = lonLo, bottom = latLo, right = lonHi, top = latHi),
    zoom = zoom,
    maptype = maptype,
    crop = TRUE,
    messaging = FALSE,
    urlonly = FALSE,
    color = "color",
    force = FALSE,
    where = tempdir()
  )

  # ----- Create Raster object -------------------------------------------------

  # Convert vector of HTML color strings into separate R, G, B vectors
  rgb_vec <- grDevices::col2rgb(ggmap_obj)

  # Assemble the mapRaster
  dim <- attr(ggmap_obj, "dim") # x, y (i.e. cols, rows)
  bb <- attr(ggmap_obj, "bb")

  red <- matrix(rgb_vec[1,], nrow = dim[2])
  green <- matrix(rgb_vec[2,], nrow = dim[2])
  blue <- matrix(rgb_vec[3,], nrow = dim[2])
  ggmap_rgbArray <- array(dim = c(dim[2], dim[1], 3))
  ggmap_rgbArray[,,1] <- red
  ggmap_rgbArray[,,2] <- green
  ggmap_rgbArray[,,3] <- blue

  mapRaster <- raster::brick(ncol = dim[2],
                             nrow = dim[1],
                             nl = 3)
  mapRaster <- raster::setValues(mapRaster, ggmap_rgbArray)
  ###if (width == height) {mapRaster <- raster::t(mapRaster)} # rows and columns are confused when width = height

  names(mapRaster) <- c("red", "green", "blue")
  raster::extent(mapRaster) <- c(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat)

  # https://spatialreference.org/ref/sr-org/epsg3857-wgs84-web-mercator-auxiliary-sphere/
  raster::crs(mapRaster) <- sp::CRS("+init=epsg:3857")

  # if ( rgdal::CRSargs(crs) != rgdal::CRSargs(sp::CRS(paste0("+init=epsg:", mapInfo$extent$spatialReference$latestWkid))) ) {
  #   mapRaster <- raster::projectRaster(mapRaster, crs = crs, method = "ngb")
  #   # trim any extra NA on the edges generated by projectRaster
  #   mapRaster <- raster::trim(mapRaster)
  # }

  return(mapRaster)

}
