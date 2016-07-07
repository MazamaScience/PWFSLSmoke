#' @export
#' @title dailyBarPlot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @description A bar graph showing daily average PM 2.5 values for
#' a specific monitor. Each graph is colored with AQI Breaks.  
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150820)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_dailyBarPlot(ws_monitor, monitor)
#' }

monitor_hourlyPlot <- function(ws_monitor, monitorID) {
    
  if (plotType == "hour_1") {
    
    # Choose color levels
    aqiBreaks_24  <- c(0, 12, 35.5, 55.5, 150.5, 250.5, 10000)
    
    # Plot
    plot(insituTime, insituPM25,
         ylim=ylim,
         pch=pch, col=col, cex=cex,
         xlab=textList$xlab, ylab=textList$ylab)
    
    # Background colors
    if (infoList$useAQIColors) {
      addAQIColors(aqiBreaks)
      points(insituTime, insituPM25,
             pch=pch, col=col, cex=cex)
      addAQILabels(aqiBreaks)
    }
    
    # Grid
    if (infoList$useGrid) {
      grid(nx=NA,ny=NULL, col=col_grid, lwd=lwd_grid, lty=lty_grid)
      abline(v=xGrid3Hour, col=col_grid, lwd=lwd_gridLight, lty=lty_grid)
      abline(v=xGrid24Hour, col=col_grid, lwd=lwd_grid, lty=lty_grid)
    } else {
      abline(v=xGrid24Hour, col=col_grid, lwd=lwd_grid, lty=lty_grid)
    }
    
    # Annotations
    if (infoList$useAQIColors) {
      mtext("Color levels for 1-3 hr average PM2.5", side=3, line=-2.0, adj=0.95, font=3, cex=cex_mtext)
    }
    
    # ----- rolling means plots --------------------------------------------------
    
  }
  }
  