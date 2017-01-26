#' @export
#' @title Add Shaded Background
#' @param param vector of data to be represented
#' @param timeAxis vector of times of the same length as param
#' @param breaks break methodology
#' @param col color for vertical lines
#' @param maxOpacity maximum opacity
#' @param lwd line width
#' @description Adds vertical lines to an existing plot using any variable that shares the same
# length as the time axis of the current plot. Line widths corresponds to magnitude of values.

########################################################################################
#
# This function adds shading to an existing plot using any variable that shares the same
# length as the time axis of the current plot.
# 
########################################################################################

addShadedBackground <- function(param,
                                timeAxis,
                                breaks=stats::quantile(param, na.rm = TRUE),
                                col='blue',
                                maxOpacity=0.2,
                                lwd=1) {

  assignedBin <- .bincode(param, breaks, include.lowest=TRUE)
  colors <- c()
  for (i in 1:length(breaks)-1) {
    opacity <- maxOpacity*(i-1)/(length(breaks)-1)
    colors[i] <- adjustcolor(col,opacity)
  }

  abline(v=timeAxis, col=colors[assignedBin], lwd=lwd)

}