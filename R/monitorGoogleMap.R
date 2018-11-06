#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create a Google Map of ws_monitor Object
#' @param ws_monitor \emph{ws_monitor} object
#' @param slice either a time index or a function used to collapse the time axis -- defautls to \code{get('max')}
#' @param breaks set of breaks used to assign colors
#' @param colors a set of colors for different levels of air quality data determined by \code{breaks}
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param maptype map type
#' @param grayscale logical, if TRUE the colored map tile is rendered into a black & white image
#' @param map optional map object returned from \code{monitorGoogleMap(})
#' @param ... arguments passed on to \code{RgoogleMaps::PlotOnStaticMap()} (\emph{e.g.} destfile, cex, pch, etc.)
#' @return A \emph{MyMap} RgoogleMaps map object object that can serve as a base plot.
#' @description Creates a Google map of a \emph{ws_monitor} object using the \pkg{RgoogleMaps} package.
#'
#' If \code{centerLon}, \code{centerMap} or \code{zoom} are not specified, appropriate values
#' will be calcualted using data from the \code{ws_monitor$meta} dataframe.
#' @examples
#' \dontrun{
#' N_M <- Northwest_Megafires
#' # monitorLeaflet(N_M) # to identify Spokane monitorIDs
#' Spokane <- monitor_subsetBy(N_M, stringr::str_detect(N_M$meta$monitorID,'^53063'))
#' Spokane <- monitor_subset(Spokane, tlim=c(20150815, 20150831))
#' monitorGoogleMap(Spokane)
#' }

monitorGoogleMap <- function(ws_monitor,
                             slice=get('max'),
                             breaks=AQI$breaks_24,
                             colors=AQI$colors,
                             width=640,
                             height=640,
                             centerLon=NULL,
                             centerLat=NULL,
                             zoom=NULL,
                             maptype='roadmap',
                             grayscale=FALSE,
                             map=NULL,
                             ...) {

  stop("'monitorGoogleMap' is no longer supported. Use 'monitor_esriMap' instead")

  # # Sanity check
  # if ( monitor_isEmpty(ws_monitor) ) {
  #   stop("ws_monitor object contains zero monitors")
  # }
  #
  # # ----- Data Preparation ----------------------------------------------------
  #
  # if ( is.null(centerLon) ) {
  #   centerLon <- base::mean(ws_monitor$meta$longitude)
  # }
  #
  # if ( is.null(centerLat) ) {
  #   centerLat <- base::mean(ws_monitor$meta$latitude)
  # }
  #
  # # Create the 'slice'
  # if ( class(slice) == "function" ) {
  #   # NOTE:  Need as.matrix in case we only have a single monitor
  #   allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
  #   data <- as.matrix(ws_monitor$data[,-1])
  #   pm25 <- apply(as.matrix(data[,!allMissingMask]), 2, slice, na.rm=TRUE)
  # } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
  #   pm25 <- ws_monitor$data[as.integer(slice),][-1]
  # } else {
  #   stop("Improper use of slice parameter")
  # }
  #
  # # If the user only specifies breaks and not the colors or vice versa then complain
  # if ( xor(missing(breaks), missing(colors)) ) {
  #   stop(paste0("The breaks paramater ", ifelse(missing(breaks), "wasn't", "was"),
  #               " specified but the colors ", ifelse(missing(colors), "wasn't", "was"),
  #               " specified. You must specify both paramaters or neither."))
  # }
  #
  # # Colors for each point
  # cols <- aqiColors(pm25, palette=colors, bins=breaks)
  #
  # # Guess at zoom level if not specified
  # if ( is.null(zoom) ) {
  #   maxRange <- max( diff(range(ws_monitor$meta$longitude, na.rm=TRUE)), diff(range(ws_monitor$meta$latitude, na.rm=TRUE)) )
  #   if ( maxRange > 50 ) {
  #     zoom <- 3
  #   } else if ( maxRange > 20 ) {
  #     zoom <- 4
  #   } else if ( maxRange > 10 ) {
  #     zoom <- 5
  #   } else if ( maxRange > 5 ) {
  #     zoom <- 6
  #   } else if ( maxRange > 2 ) {
  #     zoom <- 7
  #   } else if ( maxRange > 1 ) {
  #     zoom <- 8
  #   } else if ( maxRange > 0.5 ) {
  #     zoom <- 9
  #   } else {
  #     zoom <- 9
  #   }
  # } else {
  #   zoom <- round(zoom)
  # }
  #
  #
  # # ----- Generate map --------------------------------------------------------
  #
  # if ( is.null(map) ) {
  #   map <- RgoogleMaps::GetMap(center=c(centerLat,centerLon), size=c(height,width),
  #                              zoom=zoom, maptype=maptype, GRAYSCALE=grayscale)
  #   map <- RgoogleMaps::PlotOnStaticMap(map)
  # }
  #
  # # Overlay function default arguments ----------------------------------------
  #
  # argsList <- list(...)
  #
  # # Explicitly declare defaults for the PlotOnStaticMap() function
  # argsList$MyMap <- map
  # argsList$lat <- ws_monitor$meta$latitude
  # argsList$lon <- ws_monitor$meta$longitude
  # argsList$size <- map$size
  # argsList$add=TRUE
  # argsList$FUN <- ifelse('FUN' %in% names(argsList), argsList$FUN, points)
  # argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 16)
  # # "ifelse returns a value with the same shape as test ..."
  # if ( ! 'col' %in% names(argsList) ) {
  #   argsList$col <- cols
  # }
  # argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, par("cex")*2.0)
  #
  # map <- do.call(RgoogleMaps::PlotOnStaticMap, argsList)
  #
  # return(invisible(map))

}
