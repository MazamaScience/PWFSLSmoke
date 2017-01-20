#' @keywords ws_monitor
#' @export
#' @import maps mapproj
#' @title Create Map of Monitoring Stations
#' @param ws_monitor ws_monitor object
#' @param slice either a time index or a function used to collapse the time axis
#' @param breaks set of breaks used to assign colors
#' @param colors set of colors must be one less than the number of breaks
#' @param cex the amount that the points will be magnified on the map
#' @param labels a set of text labels, one for each color
#' @param legendTitle a title for the legend if showLegend=TRUE
#' @param showLegend logical specifying whether to add a legend (default: \code{TRUE})
#' @param stateCol color for state outlines on the map
#' @param stateLwd width for state outlines
#' @param countyCol color for county outline on the map
#' @param countyLwd width for county outlines
#' @param add logical flag that specifies whether to add to the current plot
#' @param ... additional arguments passed to maps::map() such as 'projection' or 'parameters'
#' @description Creates a map of monitoring stations in a given ws_monitor object. 
#' Individual monitor timeseries are reduced to a single value by applying the function 
#' passed in as \code{slice} to the entire timeseries of each monitor with \code{na.rm=TRUE}.
#' These values are then plotted over a map of the United States. Any additional 
#' arguments specified in '...' are passed on to the points() function.
#' 
#' If \code{slice} is an integer, it will be used as an index to pull out a single timestep.
#' 
#' If \code{slice} is a function (not a function name) it will be used with argument 
#' \code{na.rm=TRUE} to collapse the time dimension. Thus, any user defined functions
#' passed in as \code{slice} must accept \code{na.rm} as a parameter.
#' @details Using a single number for the \code{breaks} argument will result in the use
#' of quantiles to determine a set of breaks appropriate for the number of colors.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150901, 20150930)
#' nw <- monitor_subset(airnow, stateCodes=c('WA','OR'), countryCodes="US")
#' nw_daily <- monitor_dailyStatistic(nw, FUN=mean)
#' map('county',c('WA','OR'))
#' monitorMap(nw_daily, add=TRUE)
#' title('Max Daily PM2.5 Levels in September, 2015')
#'}

# if(FALSE) {
#   ws_monitor
#   slice=get('max')
#   breaks=AQI$breaks_24
#   colors=AQI$colors
#   labels=AQI$names
#   legendTitle="Max AQI Level"
#   showLegend=TRUE
#   stateCol="grey60"
#   stateLwd=2
#   countyCol="grey70"
#   countyLwd=1
#   add=FALSE
# }

monitorMap <- function(ws_monitor, 
                       slice=get('max'),
                       breaks=AQI$breaks_24,
                       colors=AQI$colors,
                       cex=par('cex'),  
                       labels=AQI$names,
                       legendTitle="Max AQI Level",
                       showLegend=TRUE,
                       stateCol="grey60",
                       stateLwd=2,
                       countyCol="grey70",
                       countyLwd=1,
                       add=FALSE, 
                       ...) {
  
  lon <- ws_monitor$meta$longitude
  lat <- ws_monitor$meta$latitude
  
  # Create the 'slice'
  # NOTE:  Need as.matrix in case we only have a single monitor
  allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
  data <- as.matrix(ws_monitor$data[,-1])
  if (class(slice) == "function") {
    pm25 <- apply(data[,!allMissingMask], 2, slice, na.rm=TRUE)
  } else if (class(slice) == "integer" || class(slice) == "numeric") {
    pm25 <- data[as.integer(slice),] # single row
  } else {
    stop("Improper use of slice parameter")
  }
  
  # ----- Figure out names for a legend and colors for each point ---- 
  
  # Create levels and use them to create a set of colors
  levels <- .bincode(pm25, breaks, include.lowest=TRUE)  
  cols <- colors[levels]
  
  # ------------------------------- GRAPHICS --------------------------------------

  # list of states to be plotted as base map
  stateCode <- as.data.frame(unique(ws_monitor$meta$stateCode))
  colnames(stateCode) <- "abb"
  state.fips <- maps::state.fips
  duplicateIndex <- duplicated(state.fips$abb)
  state.fips <- state.fips[!duplicateIndex,]
  suppressWarnings(stateName <- dplyr::left_join(stateCode, state.fips, by="abb"))
  stateName <- apply(as.data.frame(stateName$polyname),2,function(x){stringr::str_split_fixed(x, ':', 2)})[1:nrow(stateName)]
  
  # in case map complains multiple col arguments
  if ( !add ) {
  # Plot the base map
  maps::map("state", stateName, col=stateCol, lwd=stateLwd, ...)
  maps::map('county', stateName, col=countyCol, lwd=countyLwd, add=TRUE, ...)
  }
  
  # Now we add the (potentially projected) monitor points

  # Set default graphical parameters unless they are passed in
  argsList <- list(...)
  
  if( is.null(argsList$projection) ) {
    points(lon, lat, pch=16, cex=cex, col=cols)
  } else {
   points( mapproj::mapproject(lon,lat,argsList$projection, argsList$parameters, argsList$orientation), 
          pch=16, cex=cex, col=cols )
  }
  
  par(xpd=TRUE)
  if (showLegend) { addLegend(cex=cex*0.7, col=rev(colors), legend=rev(labels), title=legendTitle)}
}
