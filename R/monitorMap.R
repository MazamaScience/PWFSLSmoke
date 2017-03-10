#' @keywords ws_monitor
#' @export
#' @import maps mapproj
#' @title Create Map of Monitoring Stations
#' @param ws_monitor emph{ws_monitor} object
#' @param slice either a time index or a function used to collapse the time axis
#' @param breaks set of breaks used to assign colors
#' @param colors set of colors must be one less than the number of breaks
#' @param cex the amount that the points will be magnified on the map
#' @param stateCol color for state outlines on the map
#' @param stateLwd width for state outlines
#' @param countyCol color for county outline on the map
#' @param countyLwd width for county outlines
#' @param add logical specifying whether to add to the current plot
#' @param ... additional arguments passed to \code{maps::map()} such as \code{'projection'} or \code{'parameters'}
#' @description Creates a map of monitoring stations in a given ws_monitor object. 
#' Individual monitor timeseries are reduced to a single value by applying the function 
#' passed in as \code{slice} to the entire timeseries of each monitor with \code{na.rm=TRUE}.
#' These values are then plotted over a map of the United States. Any additional 
#' arguments specified in '...' are passed on to the \code{points()} function.
#' 
#' If \code{slice} is an integer, it will be used as an index to pull out a single timestep.
#' 
#' If \code{slice} is a function (not a function name) it will be used with argument 
#' \code{na.rm=TRUE} to collapse the time dimension. Thus, any user defined functions
#' passed in as \code{slice} must accept \code{na.rm} as a parameter.
#' @details Using a single number for the \code{breaks} argument will result in the use
#' of quantiles to determine a set of breaks appropriate for the number of colors.
#' @examples
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150821,20150828))
#' monitorMap(N_M, cex=2)
#' addAQILegend()

monitorMap <- function(ws_monitor, 
                       slice=get('max'),
                       breaks=AQI$breaks_24,
                       colors=AQI$colors,
                       cex=par('cex'),  
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
    pm25 <- apply(as.matrix(data[,!allMissingMask]), 2, slice, na.rm=TRUE)
  } else if (class(slice) == "integer" || class(slice) == "numeric") {
    pm25 <- data[as.integer(slice),] # single row
  } else {
    stop("Improper use of slice parameter")
  }
  
  # Colors for each point 
  
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
  
}
