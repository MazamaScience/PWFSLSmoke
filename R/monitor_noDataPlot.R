#' @keywords internal
#' @export
#' @import graphics
#' @title Create Blank Plot with "No Data" Message
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID id for a specific monitor in the given ws_monitor object
#' @param cex text size of "No data for"; monitor ID is one size smaller
#' @description Creates a blank plot that indicates there is no data for the monitor of interest

monitor_noDataPlot <- function(ws_monitor, monitorID=NULL, cex=2.5) {

  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }

  # Change margins
  par(mar=c(1,1,1,1))

  # Blank plot
  plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab='',ylab='',col='transparent')
  box()

  # Text in the middle
  text(0.5,0.6,'No data for ',cex=cex)

  text(0.5,0.4,monitorID,cex=0.8*cex) #NOTE: Might want to set up separate cex argument for this

  # reset default margins
  par(mar=c(5,4,4,2)+.1)

}
