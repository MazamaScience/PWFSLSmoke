#' @keywords AirNow
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download and aggregate multiple hourly data files from AirNow
#'
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param hours desired number of hours of data to assemble
#' @description This function makes repeated calls to \link{airnow_downloadHourlyData}
#' to obtain data from AirNow. All data obtained are then
#' combined into a single tibble and returned.
#'
#' Parameters included in AirNow data include at least the following list:
#' \enumerate{
#' \item{BARPR}
#' \item{BC}
#' \item{CO}
#' \item{NO}
#' \item{NO2}
#' \item{NO2Y}
#' \item{NO2X}
#' \item{NOX}
#' \item{NOOY}
#' \item{OC}
#' \item{OZONE}
#' \item{PM10}
#' \item{PM2.5}
#' \item{PRECIP}
#' \item{RHUM}
#' \item{SO2}
#' \item{SRAD}
#' \item{TEMP}
#' \item{UV-AETH}
#' \item{WD}
#' \item{WS}
#' }
#'
#' Passing a vector of one ore more of the above names as the \code{parameters} argument will cause the resulting
#' tibble to be filtered to contain only records for those parameters.
#'
#' @note As of 2016-12-27, it appears that hourly data are available only for 2016 and
#' not for earlier years.
#' @return Tibble of aggregated AirNow data.
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadHourlyData}
#' @examples
#' \dontrun{
#' tbl <- airnow_downloadParseData("PM2.5", 2016070112, hours = 24)
#' }

airnow_downloadParseData <- function(
  parameters = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d00", tz = "UTC"),
  hours = 24
) {

  logger.debug(" ----- airnow_downloadParseData() ----- ")

  # Format the startdate integer using lubridate
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")

  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  tblList <- vector(mode="list", length=hours)

  logger.trace("Downloading %d hourly data files from AirNow ...", floor(hours))

  # Loop through the airnow_downloadHourlyData function and store each datafame in the list
  for (i in 1:hours) {

    datetime <- starttime + lubridate::dhours(i-1)
    datestamp <- strftime(datetime, "%Y%m%d%H", tz="UTC")

    logger.trace("Downloading AirNow data for %s", datestamp)

    # Obtain an hour of AirNow data
    result <- try( tbl <- airnow_downloadHourlyData(datestamp),
                   silent=TRUE)
    if ( "try-error" %in% class(result) ) {
      err_msg <- stringr::str_trim(geterrmessage())
      logger.warn("Unable to download data: %s",err_msg)
      next
    }

    if ( is.null(parameters) ) {

      tblList[[i]] <- tbl

    } else {

      # NOTE:  Filter inside the loop to avoid generating very large tibbles in memory

      logger.trace("Filtering to retain only data for: %s", paste(parameters, collapse=", "))
      # Generate a mask of records to retain
      parametersMask <- rep(FALSE, nrow(tbl))
      for (parameter in parameters) {
        if ( !parameter %in% unique(tbl$ParameterName) ) {
          logger.warn("Parameter '%s' is not found in the data", parameter)
        } else {
          parametersMask <- parametersMask | tbl$ParameterName == parameter
        }
      }
      # Mask is complete, now apply it
      tblList[[i]] <- tbl[parametersMask,]

    }

  }

  # Combine all tibbles, rmoving duplicates
  tbl <- dplyr::bind_rows(tblList) %>%
    dplyr::distinct()

  if ( is.null(parameters) ) {
    logger.trace("Downloaded and parsed %d rows of AirNow data", nrow(tbl))
  } else {
    logger.trace("Downloaded and parsed %d rows of AirNow data for: %s", nrow(tbl), paste(parameters, collapse=", "))
  }

  return(tbl)

}
