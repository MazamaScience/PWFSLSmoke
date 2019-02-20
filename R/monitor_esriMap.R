#' @keywords ws_monitor
#' @export
#' @title Create an ESRI Map of ws_monitor Object
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
#' @param mapRaster optional RGB Raster* object returned from
#'   \code{esriMap_getMap})
#' @param cex character expansion for points
#' @param pch plotting character for points
#' @param ... arguments passed on to \code{esriMap_plotOnStaticMap}
#'   (\emph{e.g.} destfile, cex, pch, etc.)
#' @return Plots a map loaded from arcGIS REST with points for each monitor
#' @description Creates an ESRI map of a \emph{ws_monitor} object.
#'
#' #' Available \code{maptypes} include:
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
#' If \code{centerLon}, \code{centerMap} or \code{zoom} are not specified,
#' appropriate values will be calcualted using data from the
#' \code{ws_monitor$meta} dataframe.
#' @examples
#' \dontrun{
#' N_M <- Northwest_Megafires
#' # monitor_leaflet(N_M) # to identify Spokane monitorIDs
#' Spokane <- monitor_subsetBy(N_M, stringr::str_detect(N_M$meta$monitorID,'^53063'))
#' Spokane <- monitor_subset(Spokane, tlim=c(20150815, 20150831))
#' monitor_esriMap(Spokane)
#' }
#' @seealso \code{\link{esriMap_plotOnStaticMap}}

monitor_esriMap <- function(ws_monitor,
                            slice = get("max"),
                            breaks = AQI$breaks_24,
                            colors = AQI$colors,
                            width = 640,
                            height = 640,
                            centerLon = NULL,
                            centerLat = NULL,
                            zoom = NULL,
                            maptype = "worldStreetMap",
                            grayscale = FALSE,
                            mapRaster = NULL,
                            cex = par("cex") * 2.0,
                            pch = 16,
                            ...) {

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }

  # ----- Data Preparation ----------------------------------------------------

  if ( is.null(centerLon) ) {
    centerLon <- base::mean(ws_monitor$meta$longitude)
  }

  if ( is.null(centerLat) ) {
    centerLat <- base::mean(ws_monitor$meta$latitude)
  }

  # Create the 'slice'
  if ( class(slice) == "function" ) {
    # NOTE:  Need as.matrix in case we only have a single monitor
    allMissingMask <- apply(as.matrix(ws_monitor$data[, -1]), 2, function(x) { all(is.na(x)) } )
    data <- as.matrix(ws_monitor$data[, -1])
    pm25 <- apply(as.matrix(data[, !allMissingMask]), 2, slice, na.rm = TRUE)
  } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
    pm25 <- ws_monitor$data[as.integer(slice), ][-1]
  } else {
    stop("Improper use of slice parameter")
  }

  # If the user only specifies breaks and not the colors or vice versa then complain
  if ( xor(missing(breaks), missing(colors)) ) {
    stop(paste0("The breaks paramater ", ifelse(missing(breaks), "wasn't", "was"),
                " specified but the colors ", ifelse(missing(colors), "wasn't", "was"),
                " specified. You must specify both paramaters or neither."))
  }

  # Colors for each point
  cols <- aqiColors(pm25, palette = colors, bins = breaks)

  # Guess at zoom level if not specified
  if ( is.null(zoom) ) {
    maxRange <- max(
      diff(range(ws_monitor$meta$longitude, na.rm = TRUE)),
      diff(range(ws_monitor$meta$latitude, na.rm = TRUE))
    )
    if ( maxRange > 50 ) {
      zoom <- 3
    } else if ( maxRange > 20 ) {
      zoom <- 4
    } else if ( maxRange > 10 ) {
      zoom <- 5
    } else if ( maxRange > 5 ) {
      zoom <- 6
    } else if ( maxRange > 2 ) {
      zoom <- 7
    } else if ( maxRange > 1 ) {
      zoom <- 8
    } else if ( maxRange > 0.5 ) {
      zoom <- 9
    } else {
      zoom <- 9
    }
  } else {
    zoom <- round(zoom)
  }


  # ----- Generate RGB Raster --------------------------------------------------------

  if ( is.null(mapRaster) ) {
    mapRaster <- esriMap_getMap(centerLon, centerLat, width = width, height = height,
                               zoom = zoom, maptype = maptype, crs = sp::CRS("+init=epsg:4326"))
  }

  # Overlay function default arguments ----------------------------------------

  argsList <- list(...)

  # Explicitly declare defaults for the esriMap_plotOnStaticMap() function
  argsList$mapRaster <- mapRaster
  argsList$grayscale <- grayscale

  do.call(esriMap_plotOnStaticMap, argsList)

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
