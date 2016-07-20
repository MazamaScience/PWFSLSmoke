#' @export
#' @title dailyBarPlot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @param width width of window (in hours) used to calculate means
#' @param data.thresh minimum number of observations as a percent of width required, or NA is returned
#' @param align alignment of averaging window relative to point being calculated ["left"|"center"|"right"]
#' @description A bar graph showing daily average PM 2.5 values for
#' a specific monitor. Each graph is colored with AQI Breaks.  
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150725, 20150805)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_rollingMeanPlot(ws_monitor, monitor)
#' }

monitor_rollingMeanPlot <- function(ws_monitor, monitorID=NULL, width=24, data.thresh=75, align="center") {
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }
  
  rollingMeans <- monitor_rollingMean(ws_monitor, width=width, data.thresh=data.thresh, align=align)
  
  # de-emphasize the points
  col <- adjustcolor('black',0.2)

  # Grid lines
  lty_grid <- 'dotted'
  col_grid <- 'gray80'
  lwd_grid <- 1.6
  lwd_gridLight <- 0.8 # for 3 hour intervals
  
  # Grid line locations
  minTime <- ws_monitor$data$datetime[1]
  maxTime <- tail(ws_monitor$data$datetime, 1)
  xGrid24Hour <- seq(minTime,maxTime,"day")
  xGrid3Hour <- seq(minTime,maxTime,"3 hour")
  
  # Plot light grey circles for actual PM2.5 data
  plot(ws_monitor$data$datetime, ws_monitor$data[[monitorID]],
       col=col,
       xlab = 'Datetime', ylab = 'PM2.5')
  
  # Grid
  grid(nx=NA,ny=NULL, col=col_grid, lwd=lwd_grid, lty=lty_grid)
  abline(v=xGrid3Hour, col=col_grid, lwd=lwd_gridLight, lty=lty_grid)
  abline(v=xGrid24Hour, col=col_grid, lwd=lwd_grid, lty=lty_grid)
  
  # Add rolling means
  lines(ws_monitor$data$datetime, rollingMeans$data[[monitorID]], col='black', lwd=2, lty='solid')
  
  # Annotations
  legend("topleft",
         legend=paste0(width, "-hour Rolling Mean"),
         cex=par('cex.lab'),
         col='black', lwd=2, lty='solid')

}