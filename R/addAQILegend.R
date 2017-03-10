#' @keywords plotting
#' @export
#' @title Add an AQI Legend to a Map
#' @param x x coordinate passed on to the \code{legend()} command
#' @param y y coordinate passed on to the \code{legend()} command
#' @param col the color for points/lines in the legend
#' @param legend a character vector to be shown in the legend
#' @param pch plotting symbols in the legend
#' @param title title for the legend
#' @param ... additional arguments to be passed to \code{legend()}
#' @description This function is a convenience wrapper around \code{graphics::legend()}. It will show the AQI colors and 
#' names by default if \code{col} and \code{legend} are not specified.

addAQILegend <- function(x="topright",
                         y=NULL,
                         col=rev(AQI$colors),
                         legend=rev(AQI$names),
                         pch=16,
                         title="Air Quality Index",
                         ...) {
  
  legend( x=x, y=y, col=col, legend=legend, pch=pch, title=title, ... )
  
}
