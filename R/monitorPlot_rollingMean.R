#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Individual Value and Rolling Mean Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the ws_monitor object (optional if only one monitor
#' in the ws_monitor object)
#' @param tlim time limit for plot
#' @param ylim optional y limits for plot
#' @param width number of periods to average (e.g. for hourly data, \code{width = 24} plots 24-hour rolling means)
#' @param data.thresh minimum number of valid observations required as a percent of \code{width};
#' NA is returned if insufficicnet valid data to calculate mean
#' @param align alignment of averaging window relative to point being calculated; one of \code{"left|center|right"}
#' @param aqiLines horizontal lines indicating AQI levels
#' @description Creates a plot of individual (e.g. hourly) and rolling mean PM2.5 values for a specific monitor.
#' @details \code{align = "left"} and \code{align = "right"} calculate backward and forward rolling averages, respectively (e.g. current period and prior/subsequent \code{width - 1} periods)
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150725, 20150805)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitorPlot_rollingMean(ws_monitor, monitor)
#' }

monitorPlot_rollingMean <- function(ws_monitor,
                                    monitorID=NULL,
                                    tlim=NULL,
                                    ylim=NULL,
                                    width=24,
                                    data.thresh=75,
                                    align="center",
                                    aqiLines=TRUE) {
  
  # ----- Style ---------------------------------------------------------------

  # points
  col_points <- adjustcolor('black',0.2)
  pch_points <- 16
  cex_points <- .8
  
  # rolling mean
  col_mean <- 'black'
  lwd_mean <- 1
    
  # Grid lines
  lty_grid <- 'dotted'
  col_grid <- 'gray80'
  lwd_grid <- 1.6
  lwd_gridLight <- 0.8 # for 3 hour intervals
  
  # ----- Data Preparaion ------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",
                  paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  #TODO: local time vs. GMT
  #TODO: tlim application (see spaghetti plot)
  #TODO: review axes to ensure that dates and major/minor lines align with appropriate date/times
  
  rollingMeans <- monitor_rollingMean(ws_monitor, width=width, data.thresh=data.thresh, align=align)
  
  # Grid line locations
  minTime <- ws_monitor$data$datetime[1]
  maxTime <- utils::tail(ws_monitor$data$datetime, 1)
  xGrid24Hour <- seq(minTime,maxTime,"day")
  xGrid3Hour <- seq(minTime,maxTime,"3 hour")
  
  # ----- Plotting --------------------------------
  
  # Plot light grey circles for actual PM2.5 data
  plot(ws_monitor$data$datetime, ws_monitor$data[[monitorID]],
       col=col_points,
       pch=pch_points,
       cex=cex_points,
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
