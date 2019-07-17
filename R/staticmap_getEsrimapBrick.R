#' @keywords plotting
#' @export
#'
#' @title Create a rasterBrick from an Esri tiled image server
#'
#' @param centerLon Map center longitude.
#' @param centerLat Map center latitude.
#' @param maptype Selects the appropriate Esri tile server. Options include:
#' \itemize{
#'   \item "world_topo"
#'   \item "world_imagery"
#'   \item "world_terrain"
#'   \item "de_Lorme"
#'   \item "world_grey"
#'   \item "world_streets"
#' }
#' @param zoom map Zoom level.
#' @param width Width of image, in pixels.
#' @param height Height of image, in pixels.
#' @param bbox Bounding box vector (lonLo, latLo, lonHi, latHi). If not null,
#'   \code{centerLon}, \code{centerLat}, and \code{zoom} are ignored.
#' @param maxTiles Maximum number of tiles to be returned. The greater the
#'   number, the slower the performance -- arbitrarily set to 20 by default.
#' @param crs Object of class CRS. The Coordinate Reference System (CRS) for the
#'   returned map. If the CRS of the downloaded map does not match, it will be
#'   projected to the specified CRS using \code{raster::projectRaster}.
#' @param tileCacheDir Optional location for cached tiles.
#'
#' @description Uses the input coordinates to fetch and composite a raster from
#' the tile server. Returns a \code{raster::rasterBrick} object. This
#' can then passed as the \code{rasterBrick} object to the
#' \code{staticmap_plotRasterBrick()} function for plotting.
#'
#' As of July 2019, this list is a handy reference to the freely available tile
#' servers which can be previewed at the following URL:
#'
#' \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
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
#' @note If both \code{zoom} and \code{maxTiles} are specified, \code{maxTiles}
#' takes precedence. To get a specified zoom level, set \code{maxTiles = NULL}.
#'
#' @return A rasterBrick object which can be plotted with
#' \code{staticmap_plotRasterBrick()} or \code{raster::plotRGB()} and serve as a
#' base plot.
#'
#' @examples
#' \dontrun{
#' rasterBrick <- staticmap_getEsrimapBrick(-122.3318, 47.668)
#' staticmap_plotRasterBrick(rasterBrick)
#' }
#' @seealso \code{\link{staticmap_getRasterBrick}}
#' @seealso \code{\link{staticmap_plotRasterBrick}}

staticmap_getEsrimapBrick <- function(
  centerLon = NULL,
  centerLat = NULL,
  maptype = "world_topo",
  zoom = 12,
  width = 640,
  height = 640,
  bbox = NULL,
  maxTiles = 20,
  crs = sp::CRS("+init=epsg:4326"),
  tileCacheDir = tempdir()
) {

  # ===== DEBUGGING ============================================================

  if ( FALSE ) {

    centerLon = -117.2554
    centerLat = 47.68013
    maptype <- "world_topo"
    zoom <- 12
    width <- 640
    height <- 640
    bbox <- NULL
    maxTiles <- 40
    crs <- sp::CRS("+init=epsg:4326")
    tileCacheDir <- tempdir()

  }

  # ----- Validate parameters --------------------------------------------------

  validMapTypes <- c("world_topo", "world_imagery", "world_terrain",
                     "de_Lorme", "world_gray", "world_streets")

  if ( !maptype %in% validMapTypes )
    stop(paste0("Required parameter maptype = '", maptype, "' is not recognized."))

  if ( is.null(centerLon) || is.null(centerLat) ) {
    if ( is.null(bbox) ) {
      stop("Required arameters centerLat + centerLon, or bbox must be specified")
    }
  }

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

  # Create lon/lat ranges
  if ( !is.null(centerLon) && !is.null(centerLat) ) {
    degreesPerPixelEW <- 360/(256 * 2^zoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(centerLat * pi/180) # radians
    xmin <- centerLon - degreesPerPixelEW*(width/2 - 0.5)
    xmax <- centerLon + degreesPerPixelEW*(width/2 - 0.5)
    ymin <- centerLat - degreesPerPixelNS*(height/2 - 0.5)
    ymax <- centerLat + degreesPerPixelNS*(height/2 - 0.5)

    bbox <-
      sf::st_bbox(c(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax),
                  crs = crs)
  } else if ( !is.null(bbox) ) {
    bbox <-
      sf::st_bbox(c(xmin = bbox[1],
                    xmax = bbox[3],
                    ymin = bbox[2],
                    ymax = bbox[4]),
                  crs = crs)
  } else {
    stop("centerLat + centerLon, or bbox must be specified")
  }

  # convert sf bbox to slippymath tile grid
  tile_grid <- slippymath::bbox_to_tile_grid(bbox,
                                             zoom = zoom,
                                             max_tiles = maxTiles)

  # Use slippymath zoom level (because we set max_tiles)
  zoom <- tile_grid$zoom

  # ----- Download tiles -------------------------------------------------------

  # Examples of additional, non-Esri tile servers can be found here:
  #   https://leaflet-extras.github.io/leaflet-providers/preview/

  esriBaseUrl <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
  slippyMapApi <- "/MapServer/tile/{zoom}/{y}/{x}"

  world_topo <-    paste0(esriBaseUrl, "World_Topo_Map", slippyMapApi)
  world_imagery <- paste0(esriBaseUrl, "World_Imagery", slippyMapApi)
  world_terrain <- paste0(esriBaseUrl, "World_Terrain_Base", slippyMapApi)
  de_Lorme <-      paste0(esriBaseUrl, "Specialty/DeLorme_World_Base_Map", slippyMapApi)
  world_gray <-    paste0(esriBaseUrl, "Canvas/World_Light_Gray_Base", slippyMapApi)
  world_streets <- paste0(esriBaseUrl, "World_Street_Map", slippyMapApi)

  tileserverQueryString <- eval(parse(text=maptype))

  # curl images
  images <-
    purrr::pmap(tile_grid$tiles,
                function(x, y, zoom) {
                  filename <- glue::glue("{maptype}_{zoom}_{x}_{y}.jpg")
                  filepath <- file.path(tileCacheDir, filename)
                  if ( !file.exists(filepath) ) {
                    curl::curl_download(url = glue::glue(tileserverQueryString),
                                        destfile = filepath)
                  }
                  return(filepath)
                },
                zoom = tile_grid$zoom)

  # ----- Assemble image -------------------------------------------------------

  # Uncropped image
  rasterBrick <- slippymath::compose_tile_grid(tile_grid, images)

  # Determine the actual extent of tiles we received
  e <- raster::extent(rasterBrick)
  mercatorMatrix <- matrix(c(e@xmin, e@xmax, e@ymin, e@ymax), nrow = 2)
  lonLatMatrix <- slippymath::merc_to_lonlat(mercatorMatrix)

  # Assign a geographic coordinate system
  raster::extent(rasterBrick) <- c(
    lonLatMatrix[1,1],
    lonLatMatrix[2,1],
    lonLatMatrix[1,2],
    lonLatMatrix[2,2]
  )
  raster::crs(rasterBrick) <- sp::CRS("+init=epsg:4326")

  # ----- Crop image -----------------------------------------------------------

  # NOTE:  Rows are latitude and columns are longitude
  x_count <- nrow(rasterBrick)
  if ( x_count > height ) {
    x_mid <- floor(x_count/2)
    x_lo <- x_mid - floor(height/2)
    x_hi <- x_mid + floor(height/2)
  }
  y_count <- ncol(rasterBrick)
  if ( y_count > width ) {
    y_mid <- floor(y_count/2)
    y_lo <- y_mid - floor(width/2)
    y_hi <- y_mid + floor(width/2)
  }

  extent <- raster::extent(rasterBrick, x_lo, x_hi, y_lo, y_hi)
  rasterBrick <- raster::crop(rasterBrick, extent)

  # ----- Return ---------------------------------------------------------------

  return(rasterBrick)

}
