#' @export
#' @title Get Time Related Information
#' @param time POSIXct vector with specified timezone
#' @param longitude longitude of the location of interest
#' @param latitude latitude of the location of interest
#' @param timezone Olson timezone at the location of interest
#' @description Calculate the local time at the target location, sunrise, sunset and solar 
#' noon times, and create several temporal masks.
#' 
#' If the \code{timezone} is provided it will be used. Otherwise, the \pkg{MazamaSpatialUtils}
#' package will be used to determine the timezone from \code{longitude} and \code{latitude}.
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
#' carmel <- monitor_subset(Carmel_Valley, tlim=c(20160801,20160810))
#' 
#' # Create timeInfo object for this monitor
#' ti <- timeInfo(carmel$data$datetime,
#'                carmel$meta$longitude,
#'                carmel$meta$latitude,
#'                carmel$meta$timezone)
#' 
#' # Subset the data based on day/night masks
#' data_day <- carmel$data[ti$day,]
#' data_night <- carmel$data[ti$night,]
#' 
#' # Build two monitor objects
#' carmel_day <- list(meta=carmel$meta, data=data_day)
#' carmel_night <- list(meta=carmel$meta, data=data_night)
#' 
#' # Plot them
#' monitorPlot_timeseries(carmel_day, shadedNight=TRUE, pch=8, col='goldenrod')
#' monitorPlot_timeseries(carmel_night, pch=16, col='darkblue', add=TRUE)
 
timeInfo <- function(time, longitude=NULL, latitude=NULL, timezone=NULL) {

  # Sanity check
  if ( is.null(longitude) || is.null(latitude) ) {
    stop(paste0("Required parameter 'longitude' or 'latitude' is missing"))
  }
  
  if ( is.null(timezone) ||  !(timezone %in% base::OlsonNames()) ) {
    # get timezone from target location
    timezone <- MazamaSpatialUtils::getTimezone(longitude, latitude, useBuffering=TRUE)
  }
  
  # convert to local time
  localTime <- lubridate::with_tz(time, tzone=timezone)
  
  # sunriset reqires matrix or spatial object for input
  coords <- matrix(c(longitude, latitude), nrow=1)
  
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



