#' @export
#' @title Add Nighttime Shading on an Existing Plot
#' @param timeInfo dataframe with local time, sunrise, and sunset
#' @param col color used to shade nights -- defaults to \code{adjustcolor('black',0.2)}
#' @description Draw shading rectangles on a plot to indicate nighttime hours.
#' @examples
#' \dontrun{
#' setSmokeDataDir("~/Data/Smoke")
#' bs <- bluesky_load(model='PNW-1.33km', modelRun=2015070600)
#' targetLon <- -123.801 ; targetLat <- 47.704
#' timeInfo <- timeInfo(bs$time, targetLon, targetLat)
#' bsLocal <- grid_subsetByDistance(bs, targetLon, targetLat, 10)
#' monitor_timeseriesPlot(bsLocal, col='red')
#' addShadedNights(timeInfo)
#' }
#' @seealso \link{addBullseye}

addShadedNights <- function(timeInfo, col=adjustcolor('black',0.1)) {
  
  localTime <- timeInfo$localTime
  sunrise <- timeInfo$sunrise[!duplicated(timeInfo$sunrise)]
  sunset <- timeInfo$sunset[!duplicated(timeInfo$sunset)]
  
  # Sanity check
  if (any(sunset < sunrise)) {
    stop('sunset before sunrise!!!')
  }
  
  # Left edge to first sunrise
  if ( localTime[1] < sunrise[1] ) {
    rect(par('usr')[1], ybottom=par('usr')[3], 
         xright=sunrise[1], ytop=par('usr')[4], 
         col=col, lwd=0)
  }
  
  # Complete nights
  for (i in seq(length(sunset)-1)) {
    rect(xleft=sunset[i], ybottom=par('usr')[3], 
         xright=sunrise[i+1], ytop=par('usr')[4], 
         col=col, lwd=0)              
  }
  
  # Last sunset to right edge
  rect(xleft=sunset[length(sunset)], ybottom=par('usr')[3], 
       xright=par('usr')[2], ytop=par('usr')[4], 
       col=col, lwd=0)        
  
}

