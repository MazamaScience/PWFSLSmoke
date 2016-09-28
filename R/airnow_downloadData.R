#' @keywords AirNow
#' @export
#' @importFrom utils read.table
#' @title Download and Aggregate Multiple Hourly Data Files from AirNow
#' @param user user name
#' @param pass password
#' @param startdate desired staring date (integer or character representing YYYYMMDDHH)
#' @param hours desired number of hours of data to assemble
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical flag to generate verbose output
#' @param curl previously initialized CURL context/handle (see RCurl::getURL())
#' @description This function makes repeated calls to \link{airnow_downloadHourlyData}
#' to obtain data from AirNow. A single curl handle is created and reused to improve 
#' peformance and avoid errors like "Maximum login connections". All data obtained are then
#' combined into a single dataframe and returned.
#' @return Dataframe of aggregated AirNow data.
#' @seealso \link{airnow_downloadMonthlyData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadData(USER, PASS, 2015070112, hours=48)
#' }

airnow_downloadData <- function(user='', pass='', startdate='', hours=24, tries=6, verbose=FALSE,
                                curl=NULL) {
  
  # Format the startdate integer using lubridate
  runStart <- parseDatetime(startdate)

  # NOTE:  The function RCurl::getCurlHandle() must be used 'once' in this function. 
  # NOTE:  It createas a curl handle which is then passed to the airnow_downloadHourlyData for each iteration. 

  if ( is.null(curl) ) {
    # Create a curl handle with appriprate options
    # NOTE:  Specific set of curl options to avoid "421 Maximum login connections"
    .opts <- list(maxconnects=2,
                  ftp.use.epsv=FALSE,
                  verbose=FALSE) # NOTE:  Change this to TRUE for detailed debugging output
    curl=RCurl::getCurlHandle(.opts=.opts)
  }
  
  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  dfList <- vector(mode="list", length=hours)
  
  # Loop through the airnow_downloadHourlyData function and store each datafame in the list
  for (i in 1:hours) {
    datetime <- runStart + lubridate::dhours(i-1)
    datestamp <- format(datetime, '%Y%m%d%H')
    if (verbose) {
      if ( stringr::str_detect(datestamp,'00$') ) cat(paste0('\nGetting data for ',datestamp))
      cat ('.')
    }
    dfList[[i]] <- airnow_downloadHourlyData(user, pass, datestamp, tries=tries, curl=curl)
  }
  
  if (verbose) cat('\n')
  
  # Combine all dataframes
  df <- dplyr::bind_rows(dfList)
  
  # Only return distinct rows (no duplicates)
  return(dplyr::distinct(df))
  
}
