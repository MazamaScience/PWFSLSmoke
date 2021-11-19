#' @keywords ws_monitor
#' @keywords plotting
#' @export
#' @title Generate AQI Colors
#' @param x vector or matrix of PM2.5 values or a \emph{ws_monitor} object
#' @param palette color palette (see \code{leaflet::colorBin()})
#' @param domain range of valid data (see \code{leaflet::colorBin()})
#' @param bins color bins (see \code{leaflet::colorBin()})
#' @param na.color missing value color (see \code{leaflet::colorBin()})
#' @return A vector or matrix of AQI colors to be used in maps and plots.
#' @description This function uses the \code{leaflet::colorBin()} function
#' to return a vector or matrix of colors derived from PM2.5 values.
#' @examples
#' wa <- monitor_subset(Northwest_Megafires, stateCodes='WA', tlim=c(20150821,20150828))
#' colorMatrix <- aqiColors(wa)
#' time <- wa$data$datetime
#' pm25 <- wa$data[,-1]
#' plot(time, pm25[,1], col=colorMatrix[,1],
#'      ylim=range(pm25, na.rm=TRUE),
#'      xlab="2015", ylab="PM 2.5 (ug/m3)", main="Washington State Smoke")
#' for ( i in seq_along(pm25) ) {
#'   points(time, pm25[,i], col=colorMatrix[,i], pch=16)
#' }

aqiColors <- function(
  x,
  palette = AQI$colors,
  domain = c(0,1e6),
  bins = AQI$breaks_24,
  na.color = NA
) {

  # Pull data out of ws_monitor object if necessary
  if ( monitor_isMonitor(x) ) {
    pm25 <- x$data[,-1]
  } else {
    pm25 <- x
  }

  # Convert to matrix if necessary
  ncol <- 1
  if ( !is.null(ncol(pm25)) ) {
    ncol <- ncol(pm25)
    pm25 <- as.matrix(pm25)
  }

  # Force conversion to a numeric vector
  vals <- as.numeric(pm25)

  # Generate color function
  colorFunc <- leaflet::colorBin(palette = palette,
                                 domain = domain,
                                 bins = bins,
                                 na.color = na.color)

  # Assign colors
  cols <- colorFunc(vals)

  # Restore shape and return
  if ( ncol > 1 ) {
    cols <- matrix(cols, ncol = ncol, byrow = FALSE)
  }

  return(cols)

}
