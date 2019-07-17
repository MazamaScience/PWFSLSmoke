#' @keywords ws_monitor
#' @export
#'
#' @title Create a static map of ws_monitor object
#'
#' @param ws_monitor \emph{ws_monitor} object
#' @param slice either a time index or a function used to collapse the time
#'   axis -- defautls to \code{get('max')}
#' @param breaks set of breaks used to assign colors
#' @param colors a set of colors for different levels of air quality data
#'   determined by \code{breaks}
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param maptype map type
#' @param grayscale logical, if TRUE the colored map tile is rendered into a
#'   black & white image
#' @param rasterBrick optional RGB rasterBrick object returned from
#'   \code{staticmap_get~Brick})
#' @param cex character expansion for points
#' @param pch plotting character for points
#' @param ... arguments passed on to \code{staticmap_plotRasterBrick()}
#'   (\emph{e.g.} destfile, cex, pch, etc.)
#'
#' @return A plot with a basemap and colored dots for each monitor.
#'
#' @description Plots a map showing \emph{ws_monitor} locations and values.
#'
#' See \code{\link{staticmap_getRasterBrick}} for a list of available
#' \code{maptype} options.
#'
#' If \code{centerLon}, \code{centerMap} or \code{zoom} are not specified,
#' appropriate values will be calcualted using data from the
#' \code{ws_monitor$meta} dataframe.
#'
#' @examples
#' \dontrun{
#' N_M <- Northwest_Megafires
#' # monitor_leaflet(N_M) # to identify Spokane monitorIDs
#' Spokane <- monitor_subsetBy(N_M, stringr::str_detect(N_M$meta$monitorID,'^53063'))
#' Spokane <- monitor_subset(Spokane, tlim=c(20150815, 20150831))
#' monitor_staticmap(Spokane)
#' }
#'
#' @seealso \code{\link{staticmap_getStamenmapBrick}}
#' @seealso \code{\link{staticmap_plotRasterBrick}}

monitor_staticmap <- function(
  ws_monitor,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  width = 640,
  height = 640,
  centerLon = NULL,
  centerLat = NULL,
  zoom = NULL,
  maptype = "terrain",
  grayscale = FALSE,
  rasterBrick = NULL,
  cex = par("cex") * 2.0,
  pch = 16,
  ...
) {

  # ===== DEBUGGING ============================================================

  if ( FALSE ) {

    N_M <-
      PWFSLSmoke::Northwest_Megafires %>%
      monitor_subset(tlim=c(20150815, 20150831))

    Spokane_MonroeSt <- monitor_subset(N_M, monitorIDs = "530630047_01")
    Seattle_10th <- monitor_subset(N_M, monitorIDs = "530330030_01")
    Spokane_area <-
      N_M %>%
      monitor_subsetBy(stringr::str_detect(N_M$meta$monitorID,'^53063'))
    # Seattle_area <-
    #   N_M %>%
    #   monitor_subsetByDistance(longitude = Seattle_10th$meta$longitude,
    #                            latitude = Seattle_10th$meta$latitude,
    #                            radius = 20)
    # lon <- Seattle_10th$meta$longitude
    # lat <- Seattle_10th$meta$latitude
    #
    # N_M %>% monitor_subsetByDistance(lon, lat, 2) %>% monitor_staticmap()

    ws_monitor <- Spokane_area

    slice <- get("max")
    breaks <- AQI$breaks_24
    colors <- AQI$colors
    width <- 640
    height <- 640
    centerLon <- NULL
    centerLat <- NULL
    zoom <- NULL
    maptype <- "terrain"
    grayscale <- FALSE
    rasterBrick <- NULL
    cex <- par("cex") * 2.0
    pch <- 16
    ... <- list()

  }

  # ----- Validate Parameters --------------------------------------------------

  if ( monitor_isEmpty(ws_monitor) )
    stop("ws_monitor object contains zero monitors")

  esriMapTypes <- c("world_topo", "world_imagery", "world_terrain",
                    "de_Lorme", "world_gray", "world_streets")

  stamenMapTypes <- c("terrain", "terrain-background",
                      "terrain-labels", "terrain-lines",
                      "toner", "toner-background", "toner-hybrid",
                      "toner-labels","toner-lines", "toner-lite",
                      "watercolor")

  validMapTypes <- c(esriMapTypes, stamenMapTypes)

  if ( !maptype %in% validMapTypes )
    stop(paste0("Required parameter maptype = '", maptype, "' is not recognized."))

  if ( is.null(zoom) ) {
    if ( !is.null(centerLon) || !is.null(centerLat) ) {
      stop("zoom must be specified for user specified centerLon and centerLat")
    }
  }

  if ( is.null(centerLon) ) {
    lonRange <- range(ws_monitor$meta$longitude, na.rm = TRUE)
    centerLon <- lonRange[1] + 0.5 * diff(lonRange)
    # Handle single-monitor ws_monitor object
    if ( diff(lonRange) < 0.001 )
      lonRange <- c(centerLon - 0.01, centerLon + 0.01)
  }

  if ( is.null(centerLat) ) {
    latRange <- range(ws_monitor$meta$latitude, na.rm = TRUE)
    centerLat <- latRange[1] + 0.5 * diff(latRange)
    # Handle single-monitor ws_monitor object
    if ( diff(latRange) < 0.001 )
      latRange <- c(centerLat - 0.01, centerLat + 0.01)
  }

  if ( is.null(zoom) ) {
    zoom <- ggmap::calc_zoom(lonRange, latRange, f = 0.1)
    # Custom adjustment based on trial and error
    if ( zoom > 9 ) {
      zoom <- zoom - 2
    } else if (zoom > 5) {
      zoom <- zoom - 1
    }
  }


  # ----- Data Preparation ----------------------------------------------------

  # Create the 'slice'
  if ( class(slice) == "function" ) {
    # NOTE:  Need as.matrix in case we only have a single monitor
    data <- as.matrix(ws_monitor$data[, -1])
    allMissingMask <- apply(data, 2, function(x) { all(is.na(x)) } )
    pm25 <- apply(as.matrix(data[, !allMissingMask]), 2, slice, na.rm = TRUE)
  } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
    pm25 <- ws_monitor$data[as.integer(slice), ][-1]
  } else {
    stop("Improper use of slice parameter")
  }

  # Complain if the user only specifies breaks and not the colors or vice versa
  if ( xor(missing(breaks), missing(colors)) ) {
    stop(paste0("The 'breaks' paramater ",
                ifelse(missing(breaks), "wasn't", "was"),
                " specified but the 'colors' parameter ",
                ifelse(missing(colors), "wasn't", "was"),
                " specified. You must specify both paramaters or neither."))
  }

  # Colors for each point
  cols <- aqiColors(pm25, palette = colors, bins = breaks)

  # ----- Generate RGB RasterBrick ---------------------------------------------

  if ( is.null(rasterBrick) ) {

    rasterBrick <- staticmap_getRasterBrick(
      centerLon,
      centerLat,
      maptype = maptype,
      zoom = zoom,
      width = width,
      height = height,
      crs = sp::CRS("+init=epsg:4326") # TODO:  Is this correct?
    )

  }

  # ----- Create Plot ----------------------------------------------------------

  argsList <- list(...)

  # Explicitly declare defaults for the staticmap_plotRasterBrick() function
  argsList$rasterBrick <- rasterBrick
  argsList$grayscale <- grayscale

  # Basemap
  do.call(staticmap_plotRasterBrick, argsList)

  # Add monitors
  lat <- ws_monitor$meta$latitude
  lon <- ws_monitor$meta$longitude
  if ( ! "col" %in% names(argsList) ) {
    col <- cols
  } else {
    col <- argsList$col
  }

  graphics::points(lon, lat, pch = pch, col = col, cex = cex)

  return(invisible(map))

}
