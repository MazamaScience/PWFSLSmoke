#' @keywords ws_monitor
#' @export
#' @import maps
#' @title Create Map of Monitoring Stations
#' @param ws_monitor ws_monitor object
#' @param slice either a time index or a function used to collapse the time axis
#' @param breaks set of breaks used to assign colors
#' @param colors set of colors must be one less than the number of breaks
#' @param add logical specifying whether to add points to an existing map
#' @param showCounties logical specifying whether to plot county boundaries -- defaults to \code{FALSE}
#' @param showLegend logical specifying whether to add a legend -- defaults to \code{TRUE}
#' @param ... additional arguments passed to points() such as 'col' or 'pch'
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
#' nw <- monitor_subset(airnow, stateCodes=c('WA','OR'))
#' nw_daily <- monitor_dailyStatistic(nw, FUN=mean)
#' map('county',c('WA','OR'))
#' monitor_map(nw_daily, add=TRUE)
#' title('Max Daily PM2.5 Levels in September, 2015')
#'}

monitor_map <- function(ws_monitor,
                        slice=get('max'),
                        breaks=AQI$breaks_24,
                        colors=AQI$colors,
                        showCounties=TRUE, showLegend=TRUE,
                        add=FALSE, ...) {
  
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
  
  if ( all(breaks == AQI$breaks_24) && all(colors == AQI$colors) ) {
    legendColors <- colors
    legendLabels <- AQI$names
    legendTitle <- 'AQI Levels'
  } else {
    # For each break, use the lower number as the name in the legend.
    legendColors <- colors
    legendLabels <- paste(sprintf("%.1f",breaks[-length(breaks)]),'--',sprintf("%.1f",breaks[-1]))
    legendTitle <- 'Custom Levels'
  }
  
  # Create levels and use them to create a set of colors
  levels <- .bincode(pm25, breaks, include.lowest=TRUE)  
  cols <- colors[levels]
  
  # ------------------------------- GRAPHICS --------------------------------------
  
  # Set default graphical parameters unless they are passed in
  argsList <- list(...)
  argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 16)
  argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, 1)
  # For some reason using ifelse('col' %in% names(argsList), argsList$col, cols) doesn't work,
  # Instead of asigning the whole cols vector, it assigns the first value from the col vector.
  # Also doesn't work for xlim and ylim.... 
  if( 'xlim' %in% names(argsList) ) {
    argsList$xlim <- argsList$xlim
  } else {
    argsList$xlim <- adjustRange(lon, 1.5, 0.5)
  }
  if( 'ylim' %in% names(argsList) ) {
    argsList$ylim <- argsList$ylim
  } else {
    argsList$ylim <- adjustRange(lat, 1.5, 0.5)
  }
  if ( !('col' %in% names(argsList)) ) {
    argsList$col <- cols
  }
  
  # Assign the x and y arguments
  argsList$x <- lon
  argsList$y <- lat
  
  if ( !add ) {
    # Plot the base map
    maps::map("state", xlim=argsList$xlim, ylim=argsList$ylim)    
    if (showCounties) maps::map('county',col='gray90',add=TRUE)    
  }
  
  # Call the points() function
  do.call(points, argsList)
  
  # Create outlines for each point
  argsList$pch <- 1
  argsList$cex <- argsList$cex * 1.2
  argsList$col <- 'black'
  do.call(points, argsList)
  
  if (showLegend) legend("bottomright", col=rev(legendColors), legend=rev(legendLabels), pch=16, cex=.7)
}
