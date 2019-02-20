#' @title Get time related information
#'
#' @description Calculate the local time at the target location, as well as
#' sunrise, sunset and solar noon times, and create several temporal masks.
#'
#' If the \code{timezone} is provided it will be used. Otherwise, the
#' \pkg{MazamaSpatialUtils} package will be used to determine the timezone from
#' \code{longitude} and \code{latitude}.
#'
#' The returned dataframe will have as many rows as the length of the incoming
#' UTC \code{time} vector and will contain the following columns:
#'
#' \itemize{
#' \item{\code{localStdTime_UTC} -- UTC representation of local \strong{standard} time}
#' \item{\code{daylightSavings} -- logical mask = TRUE if daylight savings is in effect}
#' \item{\code{localTime} -- local clock time}
#' \item{\code{sunrise} -- time of sunrise on each localTime day}
#' \item{\code{sunset} -- time of sunset on each localTime day}
#' \item{\code{solarnoon} -- time of solar noon on each localTime day}
#' \item{\code{day} -- logical mask = TRUE between sunrise and sunset}
#' \item{\code{morning} -- logical mask = TRUE between sunrise and solarnoon}
#' \item{\code{afternoon} -- logical mask = TRUE between solarnoon and sunset}
#' \item{\code{night} -- logical mask = opposite of day}
#' }
#'
#' @details
#' While the \pkg{lubridate} package makes it easy to work in local timezones,
#' there is no easy way in R to work in "Local Standard Time" (LST) as is often
#' required when working with air qualitiy data. EPA regulations mandate that
#' daily averages be calculated based on LST.
#'
#' The \code{localStdTime_UTC} is primarily for use internally and provides
#' an important tool for creating LST daily averages and LST axis labeling.
#'
#' @param time POSIXct vector with specified timezone,
#' @param longitude Longitude of the location of interest.
#' @param latitude Latitude of the location of interest.
#' @param timezone Olson timezone at the location of interest.
#'
#' @return A dataframe with times and masks.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
#' @importFrom lubridate is.POSIXct
#' @export
#'
#' @examples
#' carmel <- monitor_subset(Carmel_Valley, tlim = c(20160801,20160810))
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
#' carmel_day <- list(meta = carmel$meta, data = data_day)
#' carmel_night <- list(meta = carmel$meta, data = data_night)
#'
#' # Plot them
#' monitor_timeseriesPlot(carmel_day, shadedNight = TRUE, pch = 8, col = 'goldenrod')
#' monitor_timeseriesPlot(carmel_night, pch = 16, col = 'darkblue', add = TRUE)

timeInfo <- function(time,
                     longitude = NULL,
                     latitude = NULL,
                     timezone = NULL) {

  # Debugging ------------------------------------------------------------------

  if ( FALSE ) {

    Thompson_Falls <- monitor_load(2018110307, 2018110607,
                                   monitorIDs = "300890007_01")
    time <- Thompson_Falls$data$datetime
    timezone <- Thompson_Falls$meta$timezone
    longitude <- Thompson_Falls$meta$longitude
    latitude <- Thompson_Falls$meta$latitude
    timeInfo <- timeInfo(time, longitude, latitude, timezone)
    t(timeInfo[24:27,])

  }

  # Validate parameters --------------------------------------------------------

  if ( is.null(time) ) {
    stop(paste0("Required parameter 'time' is missing"))
  } else if ( !is.POSIXct(time) ) {
    stop(paste0("Required parameter 'time' must be of class POSIXct"))
  }

  if ( is.null(longitude) ) {
    stop(paste0("Required parameter 'longitude' is missing"))
  } else if ( !is.numeric(longitude) ) {
    stop(paste0("Required parameter 'longitude' must be of class numeric"))
  }

  if ( is.null(latitude) ) {
    stop(paste0("Required parameter 'latitude' is missing"))
  } else if ( !is.numeric(latitude) ) {
    stop(paste0("Required parameter 'latitude' must be of class numeric"))
  }

  if ( is.null(timezone) || !(timezone %in% base::OlsonNames()) ) {
    # get timezone from target location
    timezone <- MazamaSpatialUtils::getTimezone(longitude, latitude, useBuffering = TRUE)
  }

  # Solar times ----------------------------------------------------------------

  # convert to local time
  localTime <- lubridate::with_tz(time, tzone = timezone)

  # sunriset reqires matrix or spatial object for input
  coords <- matrix(c(longitude, latitude), nrow = 1)

  # calculate sunrise, sunset, and solar noon times using fancy algorithm
  sunrise <- maptools::sunriset(coords, localTime, direction = "sunrise", POSIXct.out = TRUE)
  sunset <- maptools::sunriset(coords, localTime, direction = "sunset", POSIXct.out = TRUE)
  solarnoon <- maptools::solarnoon(coords, localTime, POSIXct.out = TRUE)

  sunrise <- sunrise[,2] ; sunset <- sunset[,2] ; solarnoon <- solarnoon[,2]

  # create masks
  dayMask <- (localTime >= sunrise) & (localTime < sunset)
  nightMask <- !dayMask
  morningMask <- (localTime > sunrise) & (localTime <= solarnoon)
  afternoonMask <- (localTime > solarnoon) & (localTime <= sunset)

  # localStandardTime_UTC ------------------------------------------------------

  # Change the name so it doesn't conflict with "timezone" inside @data
  tz <- timezone

  # Calculate the Local Standard Time offset
  lst_offset <- MazamaSpatialUtils::SimpleTimezones@data %>%
    filter(.data$timezone == tz) %>%
    dplyr::pull(.data$UTC_offset)

  # 'datetime' should be UTC but set it just in case
  localStandardTime_UTC <- lubridate::with_tz(localTime, "UTC") +
    lst_offset * lubridate::dhours(1)

  # Return ---------------------------------------------------------------------

  # Assemble dataframe
  timeInfo <- data.frame(
    localStandardTime_UTC = localStandardTime_UTC,
    daylightSavings = lubridate::dst(localTime),
    localTime = localTime,
    sunrise = sunrise,
    sunset = sunset,
    solarnoon = solarnoon,
    day = dayMask,
    morning = morningMask,
    afternoon = afternoonMask,
    night = nightMask
  )

  return(timeInfo)

}



