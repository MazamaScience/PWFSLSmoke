#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Creates Blank Plot with "No Data" Message
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the given ws_monitor object
#' @description Creates a blank plot that indicates there is no data for the monitor of interest

monitor_noDataPlot <- function(ws_monitor, monitorID=NULL) {

  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }
  
  # Blank plot
  plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab='',ylab='',col='transparent')
  box()

  # Text in the middle
  text(0.5,0.6,'No data for ',cex=2.5)

  text(0.5,0.4,monitorID,cex=1.5)

}
