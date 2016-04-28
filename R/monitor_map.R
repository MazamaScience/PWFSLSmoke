#' @keywords ws_monitor
#' @import maps
#' @export
#' @title Create Map of Monitoring Stations
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param slice either a time index or a function used to collapse the time axis -- defautls to \code{get('max')}
#' @param AQIStyle AQI breaks and colors to use ('1_3'|'8'|'24') -- defaults to '1_3'
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks
#' @param paletteFunc a palette generating function as returned by \code{colorRampPalette}
#' @param add logical specifying whether to add points to an existing map
#' @param showCounties logical specifying whether to plot county boundaries -- defaults to \code{FALSE}
#' @param showLegend logical specifying whether to add a legend -- defaults to \code{TRUE}
#' @param ... additional arguments passed to points() such as 'col' or 'pch'
#' @description Creates a map of monitoring stations in the US. Individual monitor timeseries are reduced to 
#' a single value by applying the function passed in as \code{slice} to the entire timeseries of each monitor
#' with \code{na.rm=TRUE}. These values are then plotted over a map of the United States. All arguments specified in '...' are passed
#' on to the points() function.
#' 
#' If \code{slice} is an integer, it will be used as an index to pull out a single timestep.
#' 
#' If \code{slice} is a function (not a function name) it will be used with argument \code{na.rm=TRUE} to
#' collapse the time dimension. Thus, user defined functions must accept \code{na.rm} as a parameter.
#' @details The colors used for the monitors will be chosen based on the AQIStyle which should match the
#' width of any rolling mean that was applied to the data. Note that the AQI colors used for 1-hr and 3-hr
#' rolling means are the same.
#' 
#' Both breaks and paletteFunc must both be specified in order to use them. 
#' 
#' Using a single number for the \code{breaks} argument will cause the algorithm to use
#' quantiles to determine breaks. This is reasonable behavior when coloring by size, or
#' fire radiative power. In the case of coloring by 'datetime' or some other
#' attribute where you need linear breaks you will have to create those breaks
#' manually and pass them into the breaks argument.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150901, 20150930)
#' airnow_conus <- monitor_subset(airnow, stateCodes=CONUS)
#' monitor_map(airnow_conus)
#' title('Max PM2.5 Levels in September, 2015)
#'}

monitor_map <- function(ws_monitor, slice=get('max'), AQIStyle='1_3',
                        breaks = NULL, paletteFunc=NULL,
                        showCounties=TRUE, showLegend=TRUE,
                        add=FALSE, ...) {
  
  lon <- ws_monitor$meta$longitude
  lat <- ws_monitor$meta$latitude
  
  AQIStyle <- as.character(AQIStyle)
  
  # Sanity check
  if ( !(AQIStyle  %in% c('1_3','8','24')) )
    stop(paste0('AQIStyle "',AQIStyle,'" is not recognized. Please use "1_3", "8" or "24".'))
  
  # Classify AQI level based on statistic at each station
  if (is.null(slice)) {
    stop("Need to specify a slice")
  } else if (class(slice) == "function") {
    # NOTE:  Need as.matrix in case we only have a single monitor
    allMissingMask <- apply(as.matrix(ws_monitor$data[,-1]), 2, function(x) { all(is.na(x)) } )
    data <- as.matrix(ws_monitor$data[,-1])
    pm25 <- apply(data[,!allMissingMask], 2, slice, na.rm=TRUE)
  } else if (class(slice) == "integer" || class(slice) == "numeric") {
    pm25 <- ws_monitor$data[,as.integer(slice)]
  } else {
    stop("Improper use of slice parameter")
  }
  
  # If the user only specifies breaks and not the paletteFunc or vice versa then complain
  if (xor(is.null(breaks), is.null(paletteFunc))) {
    stop(paste0("The breaks paramater ", ifelse(is.null(breaks), "wasn't", "was"),
                " specified but the paletteFunc ", ifelse(is.null(paletteFunc), "wasn't", "was"),
                " specified. You must specify both paramaters or neither."))
  }
  
  # ----- Figure out names for a legend and colors for each point ---- 
  
  # If the user didn't use custom breaks then use AQI names and colors
  if ( is.null(breaks)) {
    breaks <- AQI[[ paste0("breaks_", AQIStyle) ]]
    names <- AQI$names
    levelColors <- AQI$colors
  } else {
    # If the used a scalar for the breaks then use that many quantiles for breaks
    if ( length(breaks) == 1 ) {
      probs <- seq(0,1,length.out=(breaks+1))
      breaks <- quantile(pm25, probs=probs, na.rm=TRUE)
    }
    # For each break, use the lower number as the name in the legend.
    names <- unname(head(breaks, -1))
    levelColors <- paletteFunc(length(breaks) - 1)
  }
  
  # Create levels and use them to create a color mask
  levels <- .bincode(pm25, breaks, include.lowest = TRUE)  
  cols <- levelColors[levels]
  
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
  if (!('col' %in% names(argsList))) {
    argsList$col <- cols
  }
  
  # Assign the x and y arguments
  argsList$x <- lon
  argsList$y <- lat
  
  if (!add) {
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
  
  if (showLegend) legend("bottomright", col=rev(levelColors), legend=rev(names), pch=16, cex=.7)
}
