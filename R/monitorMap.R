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
  
  # Create the basemap
  if ( !add ) {

    stateCodes <- unique(ws_monitor$meta$stateCode)
    
    if ( is.null(stateCodes) || stateCodes == '' ) {
      
      # No stateCodes found. Use xlim and ylim.
      xlim <- range(ws_monitor$meta$longitude)
      ylim <- range(ws_monitor$meta$latitude)
      # add a 1 degree buffer
      xlim[1] <- xlim[1] - 1.0
      xlim[2] <- xlim[2] + 1.0
      ylim[1] <- ylim[1] - 1.0
      ylim[2] <- ylim[2] + 1.0
      # Plot the base map: state first, counties on top
      maps::map("state", xlim=xlim, ylim=ylim, col=stateCol, lwd=stateLwd, ...)
      maps::map('county', xlim=xlim, ylim=ylim, col=countyCol, lwd=countyLwd, add=TRUE, ...)
      
    } else {
      
      # Plot only the states containing the monitors
      # Need to get the maps package state names to be plotted in base map
      # We need to deal with things like "washington:whidbey island"
      stateCodes <- as.character(stateCodes)
      df <- maps::state.fips
      df$polyname <- stringr::str_replace(df$polyname, ":.*", "")
      df <- df %>% dplyr::filter(df$abb %in% stateCodes) %>% dplyr::distinct()
      stateNames <- df$polyname
      # Plot the base map: state first, counties on top
      maps::map("state", stateNames, col=stateCol, lwd=stateLwd, ...)
      maps::map('county', stateNames, col=countyCol, lwd=countyLwd, add=TRUE, ...)
      
    }
    
  }
  
  # Now we add the (potentially projected) monitor points
  
  # Set default graphical parameters unless they are passed in
  argsList <- list(...)
  
  if( is.null(argsList$projection) ) {
    points(lon, lat, pch=16, cex=cex, col=cols, xpd=NA)
  } else {
    points( mapproj::mapproject(lon,lat,argsList$projection, argsList$parameters, argsList$orientation), 
            pch=16, cex=cex, col=cols, xpd=NA )
  }
  
}
