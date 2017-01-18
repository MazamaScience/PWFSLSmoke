#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create a Timeseries Plot of ws_monitor Object PM2.5 Data
#' @param ws_monitor ws_monitor object
#' @param AQIStyle AQI styling, one of \code{"1_3|8|24"}
#' @param useGMT If TRUE, \code{monitor_timeseriesPlot} will display time as
#' GMT. Otherwise, it will attempt to use local time, provided all monitors
#' are in the same timezone. There will be an error if \code{useGMT} is FALSE but
#' monitors are in multiple timezones.
#' @param shadedNight Shade nighttime values if TRUE, provided \code{useGMT} is FALSE. 
#' This is because we only allow shading for multiple monitors if they are in
#' the same timezone; in this case, they have approximately the same sunrise and
#' sunset times.
#' @param add A logical specifying whether you want to add the plot on top of an existing time series plot
#' @param ... Any additional graphical parameters that can be passed in to the function
#' @description Creates a time series plot of PM2.5 data from a ws_monitor object (see note below). Optional arguments
#' color code by AQI index, add shading to indicate nighttime, and adjust the time display (GMT vs. local).
#' @note Remember that a ws_monitor object can contain data from more than one monitor, and thus, this function may produce
#' a time series of data from multiple monitors. To plot a time series of an individual monitor's data, use the monitor_subset
#' function (see example below).
#' @examples
#' \dontrun{ 
#' airnow <- airnow_load(20150801, 20150831)
#' Roseburg <- monitor_subset(airnow, monitorIDs=c('410190002'))
#' monitorPlot_timeseries(Roseburg)
#' }

#TODO: Looks like the first two AQI styles aren't working...AQI doesn't appear to have 1_3 or 8 style configured.
#TODO: 

monitorPlot_timeseries <- function(ws_monitor, AQIStyle='', useGMT=FALSE, shadedNight=FALSE, add=FALSE,
                                   ...) {
  
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Determine the time axis to use for plotting
  if (useGMT) {
    times <- data[,1]
  } else {
    timezoneCount <- length(unique(meta$timezone))
    if ( timezoneCount == 1 ) {
      timezone <- as.character(meta$timezone[1])
      times <- lubridate::with_tz(data$datetime, tzone=timezone)
    } else {
      stop(paste0('Cannot calculate local time for monitors in ',timezoneCount,' timezones.'))      
    }
  }
  
  # TODO: better ylim smarts 
  # set range for plotting
  if ( !('ylim' %in% names(list(...))) ){
    ymax <- max(data[,-1], na.rm=TRUE)
    ylim <- c(0, ymax*1.1)
  } else {
    ylim <- list(...)$ylim
  }
  
  # set transparency based on number of points 
  dims <- dim(as.matrix(data[,-1]))
  num_na <- length(which(is.na(data[,-1])))
  size <- dims[1]*dims[2] - num_na
  transparency <- min(8/log(size), 1)
  
  # base plot for background
  if (!add) {
    plot(times, data[,2], ylim=ylim, col='transparent', axes=FALSE,
         xlab='', ylab='PM2.5')    
  }
  
  box()
  
  # Add axes if we are not adding points on top of an existing plot
  if (!add) {
    # Y axis
    axis(2, las=1)
    
    # TODO:  Nicely formatted time axis
    axis.POSIXct(1, times)
  }
  
  # choose AQI breaks 
  if (AQIStyle !='') {
    style <- paste0("breaks_", as.character(AQIStyle))
    breaks <- AQI[[style]]
    
    for (id in meta$monitorID) {
      # set style options 
      values <- data[[id]] # same as data[,id]
      levels <- .bincode(values, breaks)
      cols <- AQI$colors[levels]
      cols <- adjustcolor(cols, alpha.f=transparency)
      cexs <- values / 200 + .3
      cexs <- pmin(cexs,2)
      
      points(times, values, type='p', pch=16, cex=cexs, col=cols)
    }
  } else {
    
    for (id in meta$monitorID) {
      # set style options 
      values <- data[[id]] # same as data[,id]
      
      points(times, values, ...)
    }
  }
  
  # shade nighttime hours 
  # NOTE:  Allow nighttime shading as long as they are all in the same timezone.
  # NOTE:  Yes, if you have monitors in Washington and SoCal this can be misleading.
  # NOTE:  But the scenario where we are plotting multiple nearby monitors demands that we
  # NOTE:  allow nighttime shading for multiple monitors.
  ###  if(nrow(meta) == 1 & !useGMT & shadedNight) {
  if ( shadedNight && !useGMT ) {
    mon_1 <- monitor_subset(ws_monitor, monitorIDs=meta$monitorID[1])
    time <- mon_1$data$datetime
    lon <- mon_1$meta$longitude
    lat <- mon_1$meta$latitude
    timezone <- mon_1$meta$timezone
    timeInfo <- timeInfo(time, lon, lat, timezone)
    addShadedNights(timeInfo)
  }
  
}
