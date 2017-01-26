#' @keywords legend
#' @export
#' @title Add a Legend to a Map of Monitoring Stations
#' @param x x coordinate passed on to the legend() command
#' @param y y coordinate passed on to the legend() command
#' @param col the color for points/lines in the legend
#' @param legend a character vector to be shown in the legend
#' @param  pch plotting symbols in the legend
#' @param title title for the legend
#' @param ... additional arguments to be passed to legend()
#' @description This function is a convenience wrapper around graphics::legend(). It will show the AQI colors and 
#' names by default if \code{col} and \code{legend} are not specified.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150901, 20150930)
#' nw <- monitor_subset(airnow, stateCodes=c('WA','OR'), countryCodes="US")
#' monitorMap(nw, showLegend=FALSE)
#' addLegend(x="left", bty="n", cex=0.8, inset=c(-0.2,0), xpd=TRUE)
#' }

addLegend <- function( x="topright",
                       y=NULL,
                       col=rev(AQI$colors),
                       legend=rev(AQI$names),
                       pch=16,
                       title="Air Quality Index",
                       ...) {
  legend( x=x, y=y, col=col, legend=legend, pch=pch, title=title, ...)
}