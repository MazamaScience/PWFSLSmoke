#' @export
#' @title No Data Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @description Plot that indicates there is no data

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
