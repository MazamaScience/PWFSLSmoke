#' @keywords ws_monitor
#' @export
#' @importFrom grDevices
#' @title Leaflet Interactive Map of Monitoring Stations
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param slice either a time index or a function used to collapse the time axis -- defautls to \code{get('max')}
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks
#' @param colors a set of colors for different levels of air quality data determined by \code{breaks}
#' @param radius radius of monitor circles
#' @param opacity opacity of monitor circles
#' @param maptype optional name of leaflet ProviderTiles to use, e.g. "terrain"
#' @param popupInfo a vector of column names from ws_monitor$meta to be shown in a popup window
#' @description This function creates interactive maps that will be displayed in RStudio's 'Viewer' tab.  
#' Individual monitor timeseries are reduced to 
#' a single value by applying \code{statistic}to the entire timeseries of each monitor with \code{na.rm=TRUE}.
#' These values are then plotted over a map of the United States.
#' 
#' If \code{slice} is a function (not a function name) it will be used with argument \code{na.rm=TRUE} to
#' collapse the time dimension. Thus, user defined functions must accept \code{na.rm} as a parameter.
#' @details You can use AQI colors and 24-hr, daily average breaks by specifying \code{breaks=NULL, colors=Null}.
#' 
#' The maptypes that are acceptted:
#' \enumerate{
#' \item{roadmap}{ -- "OpenStreetMap"}
#' \item{satellite}{ -- "Esri.WorldImagery"}
#' \item{terrain}{ -- "Stamen.Terrain"}
#' \item{toner}{ -- "Stamen.Toner"}
#' }
#' 
#' See \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of "provider tiles"
#' to use as the background map..
#' @return Initiates the interactive dygraph plot in Rstudio's 'Viewer' tab.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' v_low <- AQI$breaks_24[4]
#' CA_unhealthy_monitors <- monitor_subset(airnow, stateCodes='CA', vlim=c(v_low, Inf))
#' monitorInteractiveMap(CA_unhealthy_monitors, maptype="toner")
#' }

if (FALSE) {
  slice=get('max')
  breaks=AQI$breaks_24
  colors=AQI$colors
  labels=AQI$names
  legendTitle='Max AQI Level'
  radius=10
  opacity=0.7
  maptype="terrain"
  popupInfo=c('siteName','monitorID','elevation')
}

monitorInteractiveMap <- function(ws_monitor, slice=get('max'),
                                  breaks=AQI$breaks_24,
                                  colors=AQI$colors,
                                  labels=AQI$names,
                                  legendTitle='Max AQI Level',
                                  radius=10, opacity=0.7, maptype="terrain",
                                  popupInfo=c('siteName','monitorID','elevation')) {

  # BEGIN verbatim from monitor_map.R -----------------------------------------
  
  # Create the 'slice'
  if ( class(slice) == "function" ) {
    # NOTE:  Need as.matrix in case we only have a single monitor
  allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
    data <- as.matrix(ws_monitor$data[,-1])
    pm25 <- apply(data[,!allMissingMask], 2, slice, na.rm=TRUE)
  } else if (class(slice) == "integer" || class(slice) == "numeric") {
    pm25 <- ws_monitor$data[,as.integer(slice)]
  } else {
    stop("Improper use of slice parameter")
  }
  
  # If the user only specifies breaks and not the colors or vice versa then complain
  if (xor(is.null(breaks), is.null(colors))) {
    stop(paste0("The breaks paramater ", ifelse(is.null(breaks), "wasn't", "was"),
                " specified but the colors ", ifelse(is.null(colors), "wasn't", "was"),
                " specified. You must specify both paramaters or neither."))
  }
  
  # ----- Figure out names for a legend and colors for each point ---- 
  
  # If the user didn't use custom breaks then use AQI names and colors
  if ( ! is.null(breaks) ) {
    
    if ( length(breaks) <= 2) {
      stop("Please specify the correct vector of breaks")
    }
    
    if (! (length(breaks) - 1 == length(colors)) ) {
      stop("The number of colorts provided should be one less than the number of breaks")
    }
    
    # For each break, use the lower number as the name in the legend.
    legendColors <- colors
    legendLabels <- paste(sprintf("%.1f",breaks[-length(breaks)]),'--',sprintf("%.1f",breaks[-1]))
  }
  
  # Create levels and use them to create a color mask
  levels <- .bincode(pm25, breaks, include.lowest=TRUE)  
  cols <- legendColors[levels]
  
  # END verbatim from monitor_map.R -------------------------------------------
  
  # Create popup
  
  # If there are no column names in the popupInfo, then don't make descriptions
  if ( !any(popupInfo %in% names(ws_monitor$meta)) ) {
    popupInfo <- NULL
  }
  
  # Create popup text vector
  popupText <- character()
  if ( !is.null(popupInfo) ) {
    for ( rowIndex in 1:nrow(ws_monitor$meta) ) {
      monitorText <- ''
      for ( colName in popupInfo ) {
        if ( colName %in% names(ws_monitor$meta) ) {
          value <- ws_monitor$meta[rowIndex, colName]
          if ( is.numeric(value) ) {
            monitorText <- paste0(monitorText, colName, ": ", signif(value,6), "<br/>")
          } else {
            monitorText <- paste0(monitorText, colName, ": ", value, "<br/>")
          }
        }
      }
      popupText[rowIndex] <- monitorText
    }
  }
  ws_monitor$meta$popupText <- popupText
  
  
  # Extract view information
  lonRange <- range(ws_monitor$meta$longitude, na.rm = TRUE)
  latRange <- range(ws_monitor$meta$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange),diff(latRange), na.rm = TRUE)
  # Determine appropriate zoom level
  if (maxRange > 20) { 
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }
  
  # Convert locations to SpatialPointsDataFrame  
  ws_monitor$meta <- ws_monitor$meta[!is.na(ws_monitor$meta$latitude),]
  SPDF <- sp::SpatialPointsDataFrame(coords=cbind(ws_monitor$meta$longitude,ws_monitor$meta$latitude),
                                     data=ws_monitor$meta)
  
  # Convert maptype to a character string that addProviderTiles can read
  if ( is.null(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  }
  
  # Create leaflet map
  leaflet::leaflet(SPDF) %>%
    leaflet::setView(lng=mean(lonRange), lat=mean(latRange), zoom=zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius=radius, 
      fillColor=cols, 
      fillOpacity=opacity,
      stroke=FALSE,
      popup=ws_monitor$meta$popupText) %>%
    leaflet::addLegend(
      position='bottomright',
      colors=rev(legendColors), # show low levels at the bottom
      labels=rev(legendLabels),  # show low levels at the bottom
      opacity = 1,
      title=legendTitle)

}
