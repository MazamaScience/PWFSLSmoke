#' @keywords plotting
#' @export
#' @title Add a Bullseye at the Specified Location
#' @param x x-location on plot
#' @param y y-locaiton on plot
#' @description Draws a bullseye with concentric rings of black and white.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' KingFire <- monitor_subsetByDistance(airnow, lon=-120.604, lat=38.782, radius=50)
#' map('county',region='ca', col='gray80')
#' monitorMap(KingFire, add=TRUE)
#' addBullseye(-120.604, 38.782)
#' text(-120.604, 38.782, "King Fire", pos=4, font=2)
#' }
#' @seealso \link{addShadedNights}

addBullseye <- function(x,y) {
  points(x,y,pch=1,cex=2.0,lwd=2,col='black')
  points(x,y,pch=1,cex=1.5,lwd=2,col='white')
  points(x,y,pch=1,cex=1.0,lwd=2,col='black')  
}

