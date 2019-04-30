#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from stamenmap tiles
#'
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param maptype map type
#' @param zoom map zoom level; corresponds to \code{ggmap::get_map()} zoom level
#' @param bbox bounding box vector (lonLo, latLo, lonHi, latHi). If not null,
#'   \code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.
#' @param bboxSR spatial reference of the bounding box
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param crs object of class CRS. The Coordinate Reference System (CRS) for the
#'   returned map. If the CRS of the downloaded map does not match, it will be
#'   projected to the specified CRS using \code{raster::projectRaster}.
#'
#' @description Downloads a PNG from the stamenmap tile server and creates a
#' \code{raster::rasterBrick} object with layers for red, green, and blue. This
#' can then passed as the \code{rasterBrick} object to the
#' \code{staticmap_plotRasterBrick()} function for plotting.
#'
#' Stamen maps tiles are freely available (April, 2019) and are described at the
#' following URL:
#'
#' \url{http://maps.stamen.com/#terrain/12/37.7706/-122.3782}
#'
#' "These tiles are made available as part of the CityTracking project, funded
#' by the Knight Foundation, in which Stamen is building web services and open
#' source tools to display public data in easy-to-understand, highly visual
#' ways."
#'
#' @note The spatial reference of the image when it is downloaded is 3857. If
#' the crs argument is different, projecting may cause the size and extent of
#' the image to differ very slightly from the input, on a scale of 1-2 pixels or
#' 10^-3 degrees.
#'
#' If bbox is specified and the bbox aspect ratio does not match the
#' width/height aspect ratio the extent is resized to prevent the map image from
#' appearing stretched, so the map extent may not match the bbox argument
#' exactly.
#'
#' @return A rasterBrick object which can be plotted with
#' \code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' mapRaster <- staticmap_getStamenmapBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(mapRaster)
#' }
#' @seealso \code{\link{staticmap_plotRasterBrick}}
#'

staticmap_getStamenmapBrick <- function(
  centerLon = NULL,
  centerLat = NULL,
  bbox = NULL,
  bboxSR = "4326",
  maptype = "terrain",
  zoom = 12,
  width = 640,
  height = 640,
  crs = sp::CRS("+init=epsg:4326"),
  silent = TRUE
) {

  # ===== DEBUGGING ============================================================

  if ( FALSE ) {

    centerLon <- -120
    centerLat <- 47
    bbox <- NULL
    bboxSR <- "4326"
    maptype <- "terrain"
    zoom <- 10
    width <- 640
    height <- 640
    crs <- sp::CRS("+init=epsg:4326")

  }

  # ----- Validate parameters --------------------------------------------------

  validMapTypes <- c("terrain", "terrain-background",
                     "terrain-labels", "terrain-lines",
                     "toner", "toner-background", "toner-hybrid",
                     "toner-labels","toner-lines", "toner-lite",
                     "watercolor")

  if ( !maptype %in% validMapTypes )
    stop(paste0("Required parameter maptype = '", maptype, "' is not recognized."))

  # ----- Determine bounding box -----------------------------------------------

  # Projection information:
  # https://spatialreference.org/
  # https://en.wikipedia.org/wiki/Web_Mercator_projection

  # EPSG:4326 -- lon lat pairs on the WGS84 reference ellipsoid
  # EPSG:3857 -- most (all?) tiling servers have a "web Mercator" projection

  # Calculate degrees per pixel from zoom to determine bbox:
  # * google maps tiles are 256x256 pixels
  # * zoom level 0 includes 360 degrees-EW
  # * every time zoom level increases, scale halves. Thus, for degrees EW:
  # pixels/degree = 256pixels*2^zoomLevel/360degrees
  # degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
  # degrees-NS/pixel = degreesPerPixelNS*cos(latitude)
  # Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to

  # Fudge factor
  adjustedZoom <- zoom + 0

  # Create lon/lat ranges
  if ( !is.null(centerLon) && !is.null(centerLat) ) {
    degreesPerPixelEW <- 360/(256 * 2^adjustedZoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(centerLat * pi/180) # radians
    lonLo <- centerLon - degreesPerPixelEW*(width/2 - 0.5)
    lonHi <- centerLon + degreesPerPixelEW*(width/2 - 0.5)
    latLo <- centerLat - degreesPerPixelNS*(height/2 - 0.5)
    latHi <- centerLat + degreesPerPixelNS*(height/2 - 0.5)
    bbox <- c(lonLo, latLo, lonHi, latHi)
    bboxSR <- "4326"
  } else if ( !is.null(bbox) ) {
    bboxSR <- "4326"
  } else {
    stop("centerLat + centerLon, or bbox must be specified")
  }

  # ----- Get map iImage --------------------------------------------------------

  suppressMessages({ # "messaging = FALSE" seems broken in get_stamenmap()

    ggmap_obj <- ggmap::get_stamenmap(
      bbox = bbox,
      zoom = zoom,
      maptype = maptype,
      crop = TRUE,
      messaging = FALSE,
      urlonly = FALSE,
      color = "color",
      force = FALSE,
      where = tempdir()
    )

  })

  # ----- Create rasterBrick ---------------------------------------------------

  # Convert vector of HTML color strings into separate R, G, B vectors
  rgb_vec <- grDevices::col2rgb(ggmap_obj)

  # Assemble the rasterBrick
  dim <- attr(ggmap_obj, "dim") # x, y (i.e. cols, rows)
  bb <- attr(ggmap_obj, "bb")

  red <- matrix(rgb_vec[1,], nrow = dim[2])
  green <- matrix(rgb_vec[2,], nrow = dim[2])
  blue <- matrix(rgb_vec[3,], nrow = dim[2])
  ggmap_rgbArray <- array(dim = c(dim[2], dim[1], 3))
  ggmap_rgbArray[,,1] <- red
  ggmap_rgbArray[,,2] <- green
  ggmap_rgbArray[,,3] <- blue

  rasterBrick <- raster::brick(ncol = dim[2],
                               nrow = dim[1],
                               nl = 3)
  rasterBrick <- raster::setValues(rasterBrick, ggmap_rgbArray)
  ###if (width == height) {rasterBrick <- raster::t(rasterBrick)} # rows and columns are confused when width = height

  names(rasterBrick) <- c("red", "green", "blue")
  raster::extent(rasterBrick) <- c(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat)

  # Geographic coordinate system
  raster::crs(rasterBrick) <- sp::CRS("+init=epsg:4326")

  #### https://spatialreference.org/ref/sr-org/epsg3857-wgs84-web-mercator-auxiliary-sphere/
  ###raster::crs(rasterBrick) <- sp::CRS("+init=epsg:3857")

  ### if ( rgdal::CRSargs(crs) != rgdal::CRSargs(sp::CRS(paste0("+init=epsg:", mapInfo$extent$spatialReference$latestWkid))) ) {
  ###   rasterBrick <- raster::projectRaster(rasterBrick, crs = crs, method = "ngb")
  ###   # trim any extra NA on the edges generated by projectRaster
  ###   rasterBrick <- raster::trim(rasterBrick)
  ### }

  return(rasterBrick)

}
