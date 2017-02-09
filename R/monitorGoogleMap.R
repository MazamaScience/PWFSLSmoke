#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create a Google Map of ws_monitor Object
#' @param ws_monitor ws_monitor object
#' @param slice either a time index or a function used to collapse the time axis -- defautls to \code{get('max')}
#' @param breaks set of breaks used to assign colors
#' @param colors a set of colors for different levels of air quality data determined by \code{breaks}
#' @param labels a set of text labels, one for each color
#' @param legendTitle legend title
#' @param legendX x coordinate passed on to the legend() command
#' @param legendY y coordinate passed on to the legend() command
#' @param showLegend logical specifying whether to add a legend
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param zoom map zoom level
#' @param maptype map type
#' @param grayscale logical, if TRUE the colored map tile is rendered into a black & white image
#' @param map optional map object returned from monitorGoogleMap()
#' @param ... arguments passed on to RgoogleMaps::PlotOnStaticMap() (e.g. destfile, cex, pch, etc.)
#' @return A \code{MyMap} RgoogleMaps map object object that can serve as a base plot.
#' @description Creates a Google map of a ws_monitor object.
#' TODO:  More description here.
#' @examples
#' \dontrun{
#' CarmelValley <- airnow_load(20160801,20160831,monitorIDs="060530002")
#' monitorGoogleMap(CarmelValley)
#' }

monitorGoogleMap <- function(ws_monitor,
                             slice=get('max'),
                             breaks=AQI$breaks_24,
                             colors=AQI$colors,
                             labels=AQI$names,
                             legendTitle='Max AQI Level',
                             legendX="topright",
                             legendY=NULL,
                             showLegend=TRUE,
                             width=640,
                             height=640,
                             zoom=NULL,
                             maptype='terain',
                             grayscale=FALSE,
                             map=NULL,
                             ...) {
  
  # ----- Data Preparation ----------------------------------------------------
  
  centerLat <- base::mean(ws_monitor$meta$latitude)
  centerLon <- base::mean(ws_monitor$meta$longitude)
  
  # Create the 'slice'
  if ( class(slice) == "function" ) {
    # NOTE:  Need as.matrix in case we only have a single monitor
    allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
    data <- as.matrix(ws_monitor$data[,-1])
    pm25 <- apply(as.matrix(data[,!allMissingMask]), 2, slice, na.rm=TRUE)
  } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
    pm25 <- ws_monitor$data[as.integer(slice),][-1]
  } else {
    stop("Improper use of slice parameter")
  }
  
  # If the user only specifies breaks and not the colors or vice versa then complain
  if ( xor(missing(breaks), missing(colors)) ) {
    stop(paste0("The breaks paramater ", ifelse(missing(breaks), "wasn't", "was"),
                " specified but the colors ", ifelse(missing(colors), "wasn't", "was"),
                " specified. You must specify both paramaters or neither."))
  }
  
  # Create levels and use them to create a color mask
  levels <- .bincode(pm25, breaks, include.lowest=TRUE)  
  cols <- colors[levels]
  
  # Guess at zoom level if not specified
  if ( is.null(zoom) ) {
    maxRange <- max( diff(range(ws_monitor$meta$longitude, na.rm=TRUE)), diff(range(ws_monitor$meta$latitude, na.rm=TRUE)) )
    if ( maxRange > 50 ) {
      zoom <- 2
    } else if ( maxRange > 20 ) {
      zoom <- 3
    } else if ( maxRange > 10 ) {
      zoom <- 4
    } else if ( maxRange > 5 ) {
      zoom <- 5
    } else if ( maxRange > 2 ) {
      zoom <- 6
    } else if ( maxRange > 1 ) {
      zoom <- 7
    } else if ( maxRange > 0.5 ) {
      zoom <- 8
    } else {
      zoom <- 8
    }
  }
  
  
  # ----- Generate map --------------------------------------------------------
  
  if ( is.null(map) ) {
    map <- RgoogleMaps::GetMap(center=c(centerLat,centerLon), size=c(height,width),
                               zoom=zoom, maptype=maptype, GRAYSCALE=grayscale);
    map <- RgoogleMaps::PlotOnStaticMap(map)
  }
  
  # Overlay function default arguments ----------------------------------------
  
  argsList <- list(...)
  
  # Explicitly declare defaults for the PlotOnStaticMap() function
  argsList$MyMap <- map
  argsList$lat <- ws_monitor$meta$latitude
  argsList$lon <- ws_monitor$meta$longitude
  argsList$size <- map$size
  argsList$add=TRUE
  argsList$FUN <- ifelse('FUN' %in% names(argsList), argsList$FUN, points)
  argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 16)
  # "ifelse returns a value with the same shape as test ..."
  if ( ! 'col' %in% names(argsList) ) {
    argsList$col <- cols
  }
  argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, par("cex")*2.0)
  
  map <- do.call(RgoogleMaps::PlotOnStaticMap, argsList)
  
  # TODO:  Add legend to monitorGoogleMap.R
 
  return(invisible(map)) 
}
