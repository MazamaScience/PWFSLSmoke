#' @keywords AirNow
#' @export
#' @title Download and Aggregate a Month of Hourly Data Files from AirNow
#' @param user user name
#' @param pass password
#' @param parameter name of desired pollutant
#' @param yearMonth desired year and month  (integer or character representing YYYYMM)
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical flag to generate verbose web connection output
#' @description This function is a convenience wrapper for \link{airnow_downloadData}
#' which automatically determines the number of hours of data to download for a particular month.
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
#' @return Dataframe of aggregated AirNow data.
#' @seealso \link{airnow_downloadData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadMonthlyData(USER, PASS, 201507)
#' }

airnow_downloadMonthlyData <- function(user='', pass='', parameter="PM2.5", yearMonth='', tries=6, verbose=FALSE) {
  
  logger.info('Downloading AirNow data for %s',yearMonth)
  
  # Calculate the number of hours in the month of the interest
  startdate <- paste0(yearMonth, '0100')
  starttime <- lubridate::ymd_h(startdate)
  hours <- 24 * as.numeric(lubridate::days_in_month(starttime))
  
  # Create the data frame that holds a month worth AirNow data
  df <- airnow_downloadData(user, pass, parameter=parameter, startdate=startdate, hours=hours, tries=tries, verbose=verbose)
  
  return(df)
  
}
  
