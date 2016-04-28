#' @keywords ws_monitor
#' @export
#' @title Leaflet Interactive Map of Monitoring Stations
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param slice either a time index or a function used to collapse the time axis -- defautls to \code{get('max')}
#' @param AQIStyle AQI breaks and colors to use ('1_3'|'8'|'24') -- defaults to '1_3'
#' @param breaks 7 levels used instead of AQI break levels
#' @param radius radius of monitor circles
#' @param opacity opacity of monitor circles
#' @param providerTiles optional name of leaflet ProviderTiles to use, e.g. "Stamen.Terrain"
#' @param popupInfo a vector of column names from ws_monitor$meta to be shown in a popup window
#' @description This function creates interactive maps that will be displayed in RStudio's 'Viewer' tab.  
#' Individual monitor timeseries are reduced to 
#' a single value by applying \code{statistic}to the entire timeseries of each monitor with \code{na.rm=TRUE}.
#' These values are then plotted over a map of the United States.
#' @details The colors used for the monitors will be chosen based on the AQIStyle which should match the
#' width of any rolling mean that was applied to the data. Note that the AQI colors used for 1-hr and 3-hr
#' rolling means are the same.
#' 
#' If \code{slice} is a function (not a function name) it will be used with argument \code{na.rm=TRUE} to
#' collapse the time dimension. Thus, user defined functions must accept \code{na.rm} as a parameter.
#' 
#' See \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of "provider tiles".
#' @return Initiates the interactive dygraph plot in Rstudio's 'Viewer' tab.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' v_low <- AQI$breaks_1_3[4]
#' CA_unhealthy_monitors <- monitor_subset(airnow, stateCodes='CA', vlim=c(v_low, Inf))
#' monitor_leaflet(CA_unhealthy_monitors, providerTiles="Stamen.Terrain")
#' }

monitor_leaflet <- function(ws_monitor, slice=get('max'), AQIStyle='1_3', breaks=NULL,
                            radius=10, opacity=0.7, providerTiles=NULL,
                            popupInfo=c('siteName','monitorID','elevation')) {
  
  # Styling defaults, could be changed
  stroke <- FALSE
  label <- paste0(ws_monitor$meta$siteName,' (',ws_monitor$meta$monitorID,')')
  
  # BEGIN verbatim from monitor_map.R -----------------------------------------
  
  # Colors
  AQIStyle <- as.character(AQIStyle)
  
  # Sanity check
  if ( !(AQIStyle  %in% c('1_3','8','24')) )
    stop(paste0('AQIStyle "',AQIStyle,'" is not recognized. Please use "1_3", "8" or "24".'))
  
  # If custom breaks are missing or improperly defined, choose breaks based on AQIStyle
  if ( is.null(breaks) || (length(breaks) != 7) ) {
    breaks <- AQI[[ paste0("breaks_", AQIStyle) ]]
  }
  
  # Always set the lowest and highest breaks so that everything is included
  breaks[1] <- -1e12
  breaks[length(breaks)] <- 1e12
  
  # Classify AQI level based on statistic at each station
  if ( is.null(slice) ) {
    stop("Need to specify a slice")
  } else if ( class(slice) == "function" ) {
    # NOTE:  Need as.matrix in case we only have a single monitor
    allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
    data <- as.matrix(ws_monitor$data[,-1])
    pm25 <- apply(as.matrix(data[,!allMissingMask]), 2, slice, na.rm=TRUE)
  } else if (class(slice) == "integer" || class(slice) == "numeric") {
    pm25 <- ws_monitor$data[,as.integer(slice)]
  } else {
    stop("Improper use of slice parameter")
  }
  levels <- .bincode(pm25, breaks)
  
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
  
  
  # Assign colors based on levels
  cols <- AQI$colors[levels]
  
  # END verbatim from monitor_map.R -------------------------------------------
  
  # Extract view information
  lonRange <- range(ws_monitor$meta$longitude)
  latRange <- range(ws_monitor$meta$latitude)
  maxRange <- max(diff(lonRange),diff(latRange))
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
  SPDF <- sp::SpatialPointsDataFrame(coords=cbind(ws_monitor$meta$longitude,ws_monitor$meta$latitude),
                                     data=ws_monitor$meta)
  
  # Create leaflet map
  if ( is.null(providerTiles) ) {
    
    leaflet::leaflet(SPDF) %>%
      leaflet::setView(lng=mean(lonRange), lat=mean(latRange), zoom=zoom) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        radius=radius, 
        fillColor=cols, 
        stroke=stroke, 
        fillOpacity=opacity,
        label=label,
        popup=ws_monitor$meta$popupText) %>%
      leaflet::addLegend(
        position='bottomright',
        colors=rev(AQI$colors), # show low levels at the bottom
        labels=rev(AQI$names),  # show low levels at the bottom
        opacity = 1,
        title='AQI Levels')
    
  } else {
    
    leaflet::leaflet(SPDF) %>%
      leaflet::setView(lng=mean(lonRange), lat=mean(latRange), zoom=zoom) %>%
      leaflet::addProviderTiles(providerTiles) %>%
      leaflet::addCircleMarkers(
        radius=radius, 
        fillColor=cols, 
        stroke=stroke, 
        fillOpacity=opacity,
        label=label,
        popup=ws_monitor$meta$popupText) %>%
      leaflet::addLegend(
        position='bottomright',
        colors=rev(AQI$colors), # show low levels at the bottom
        labels=rev(AQI$names),  # show low levels at the bottom
        opacity = 1,
        title='AQI Levels')
    
  }
  
}
