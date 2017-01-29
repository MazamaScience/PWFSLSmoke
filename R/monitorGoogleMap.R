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
  
  # ----- Style ---------------------------------------------------------------
  
  #   # Events as red-outlined, gray triangles (unless otherwise overwritten by icon)
  #   cex_events <- 1.2
  #   pch_events <- 17
  #   col_events <- 'gray60'
  #   pch_eventsBorder <- 2
  #   col_eventsBorder <- 'red'
  #   lwd_eventsBorder <- 2
  #   
  #   # icons (defaults - use readPNG() to define alternates if wish to use something other than default)
  #   icon_paddle <- png::readPNG('localData/pin1.png') # from http://www.clker.com/clipart-map-pin-1.html
  #   
  #   # icon expansion
  #   iconExpansion_monitor <- .1
  #   iconExpansion_events <- .1
  
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
  
  # Figure out names for a legend
  
  # TODO:  Add sanity checks but allow for overlays as in:
  # TODO:
  # TODO:    wa <- airnow_load(20160901,20160930, stateCodes='WA')
  # TODO:    map <- monitorGoogleMap(wa)
  # TODO:    monitorGoogleMap(wa, breaks=NULL, colors=NULL, labels=NULL, col='black', pch=1, map=map)
  
#   # If the user didn't use custom breaks then use AQI names and colors
#   if ( ! missing(breaks) ) {
#     
#     if ( length(breaks) <= 2) {
#       stop("Please specify a vector of breaks")
#     }
#     
#     if ( ! (length(breaks) - 1 == length(colors)) ) {
#       stop("The number of colors provided should be one less than the number of breaks")
#     }
#     
#     if ( missing(labels) ) {
#       labels <- paste(sprintf("%.1f",breaks[-length(breaks)]),'--',sprintf("%.1f",breaks[-1]))
#     } else if ( length(labels) != length(colors) ) {
#       stop("The number of labels should be equal to the number of colors")
#     }
#     
#   }
  
  # Create levels and use them to create a color mask
  levels <- .bincode(pm25, breaks, include.lowest=TRUE)  
#   if ( ! all( ! is.na(levels)) ) {
#     warning("NOTE that there are data points outside of your specified breaks, non-requested color(s) might be displayed on your map.",
#             call.=FALSE)
#   }
  cols <- colors[levels]
  
  # Guess at zoom level if not specified
  if ( is.null(zoom) ) {
    maxRange <- max( diff(range(ws_monitor$meta$longitude, na.rm=TRUE)), diff(range(ws_monitor$meta$latitude, na.rm=TRUE)) )
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
