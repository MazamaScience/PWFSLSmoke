#' @keywords plotting
#' @export
#' @title Add a Bullseye at the Specified Location
#' @param x x-location on plot
#' @param y y-locaiton on plot
#' @description Draws a bullseye with concentric rings of black and white.
#' @examples
#' \dontrun{
#' setSmokeDataDir("~/Data/Smoke/")
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2015070600)
#' xlim <- c(-124.801, -122.801)
#' ylim <- c(47.004, 48.404)
#' grid_map(bs, xlim = xlim, ylim = ylim)
#' addBullseye(-123.801, 47.704)
#' }
#' @seealso \link{addShadedNights}

addBullseye <- function(x,y) {
  points(x,y,pch=1,cex=2.0,lwd=2,col='black')
  points(x,y,pch=1,cex=1.5,lwd=2,col='white')
  points(x,y,pch=1,cex=1.0,lwd=2,col='black')  
}

