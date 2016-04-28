#' @export
#' @title Get Time Relatied Information
#' @param time POSIXct vector with specified timezone
#' @param lon longitude of the location of interest
#' @param lat latitude of the location of interest
#' @param timezone Olson timezone at the location of interest
#' @description Calculate the local time at the target location, sunrise, sunset and solar 
#' noon times, and create several temporal masks.
#' 
#' If the \code{timezone} is provided it will be used. Otherwise, the \pkg{MazamaSpatialUtils}
#' package will be used to determine the timezone from \code{lon} and \code{lat}.
#' 
#' The returned dataframe will have as many rows as the length of the incoming UTC \code{time} vector
#' and will contain the following columns:
#' \itemize{
#' \item{\code{localTime} -- local clock time}
#' \item{\code{sunrise} -- time of sunrise on each localTime day}
#' \item{\code{sunset} -- time of sunset on each localTime day}
#' \item{\code{solarnoon} -- time of solar noon on each localTime day}
#' \item{\code{day} -- logical mask = TRUE between sunrise and sunset}
#' \item{\code{morning} -- logical mask = TRUE between sunrise and solarnoon}
#' \item{\code{afternoon} -- logical mask = TRUE between solarnoon and sunset}
#' \item{\code{night} -- logical mask = opposite of day}
#' }
#' @return A dataframe with times and masks. 
#' @examples 
#' \dontrun{ 
#' setSmokeDataDir("~/Data/Smoke")
#' bs <- bluesky_load(model='PNW-1.33km', modelRun='2015070600')
#' targetLon <- -123.801 ; targetLat <- 47.704
#' timeInfo <- timeInfo(bs$time, targetLon, targetLat)
#' }

timeInfo <- function(time, lon=NULL, lat=NULL, timezone=NULL) {
  
  # Sanity check
  if ( is.null(lon) || is.null(lat) ) stop('timeInfo cannot be calculated: missing lon or lat.')

  if ( is.null(timezone) ||  !(timezone %in% lubridate::olson_time_zones()) ) {
    # get timezone from target location
    timezone <- MazamaSpatialUtils::getTimezone(lon, lat, useBuffering=TRUE)
  }
  
  # convert to local time
  localTime <- lubridate::with_tz(time, tzone=timezone)
  
  # sunriset reqires matrix or spatial object for input
  coords <- matrix(c(lon, lat), nrow=1)
  
  # calculate sunrise, sunset, and solar noon times using fancy algorithm
  sunrise <- maptools::sunriset(coords, localTime, direction="sunrise", POSIXct.out=TRUE)
  sunset <- maptools::sunriset(coords, localTime, direction="sunset", POSIXct.out=TRUE)
  solarnoon <- maptools::solarnoon(coords, localTime, POSIXct.out=TRUE)
  
  sunrise <- sunrise[,2] ; sunset <- sunset[,2] ; solarnoon <- solarnoon[,2]
  
  # create masks
  dayMask <- (localTime >= sunrise) & (localTime < sunset)
  nightMask <- !dayMask
  morningMask <- (localTime > sunrise) & (localTime <= solarnoon)
  afternoonMask <- (localTime > solarnoon) & (localTime <= sunset)
  
  # return in dataframe
  timeInfo <- data.frame(localTime=localTime, sunrise=sunrise,
                         sunset=sunset, solarnoon=solarnoon, day=dayMask, 
                         morning=morningMask, afternoon=afternoonMask, 
                         night=nightMask)
  return(timeInfo)
}



