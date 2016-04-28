#' @keywords AirNow
#' @export
#' @title Download and Aggregate a Month of Hourly Data Files from AirNow
#' @param user user name
#' @param pass password
#' @param yearMonth desired year and month  (integer or character representing YYYYMM)
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical flag to generate verbose output
#' @description This function is a convenience wrapper for \link{airnow_downloadData}
#' which automatically determines the number of hours of data to download for a particular month.
#' @return Dataframe of aggregated AirNow data.
#' @seealso \link{airnow_downloadData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadMonthlyData(USER, PASS, 201507)
#' }

airnow_downloadMonthlyData <- function(user='', pass='', yearMonth='', tries=6, verbose=FALSE) {
  
  if (verbose) cat(paste0('Downloading AirNow data for ',yearMonth,' ...\n'))
  
  # Calculate the number of hours in the month of the interest
  startdate <- paste0(yearMonth, '0100')
  starttime <- lubridate::ymd_h(startdate)
  hours <- 24 * as.numeric(lubridate::days_in_month(starttime))
  
  # Create the data frame that holds a month worth AirNow data
  df <- airnow_downloadData(user, pass, startdate=startdate, hours=hours, tries=tries, verbose=verbose)
  
  return(df)
  
}
  
