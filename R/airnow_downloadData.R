#' @keywords AirNow
#' @export
#' @title Download and Aggregate Multiple Hourly Data Files from AirNow
#' @param user user name
#' @param pass password
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param startdate desired staring date (integer or character representing YYYYMMDD[HH])
#' @param hours desired number of hours of data to assemble
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical flag to generate verbose web connection output
#' @param curl previously initialized CURL context/handle (see RCurl::getURL())
#' @description This function makes repeated calls to \link{airnow_downloadHourlyData}
#' to obtain data from AirNow. A single curl handle is created and reused to improve 
#' peformance and avoid errors like "Maximum login connections". All data obtained are then
#' combined into a single dataframe and returned.
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
#' dataframe to be filtered to contain only records for those parameters.
#' 
#' @return Dataframe of aggregated AirNow data.
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadHourlyData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadData(USER, PASS, "PM2.5", 2015070112, hours=48)
#' }

airnow_downloadData <- function(user='', pass='', parameters=NULL, startdate='', hours=24, tries=6, verbose=FALSE,
                                curl=NULL) {
  
  # Format the startdate integer using lubridate
  starttime <- parseDatetime(startdate)

  # NOTE:  The function RCurl::getCurlHandle() must be used 'once' in this function. 
  # NOTE:  It createas a curl handle which is then passed to the airnow_downloadHourlyData for each iteration. 

  if ( is.null(curl) ) {
    # Create a curl handle with appriprate options
    # NOTE:  Specific set of curl options to avoid "421 Maximum login connections"
    .opts <- list(maxconnects=2,
                  ftp.use.epsv=FALSE,
                  verbose=verbose)
    curl=RCurl::getCurlHandle(.opts=.opts)
  }
  
  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  dfList <- vector(mode="list", length=hours)
  
  logger.info('Downloading %d hourly data files from AirNow...',hours)
  
  # Loop through the airnow_downloadHourlyData function and store each datafame in the list
  for (i in 1:hours) {
    
    datetime <- starttime + lubridate::dhours(i-1)
    datestamp <- strftime(datetime, "%Y%m%d%H", tz="UTC")
    
    df <- airnow_downloadHourlyData(user, pass, datestamp, tries=tries, curl=curl)
    
    if ( is.null(parameters) ) {

      dfList[[i]] <- df
      
    } else {

      # NOTE:  Filter inside the loop to avoid generating very large dataframes in memory
      
      logger.debug('Filtering to retain only data for: %s', paste(parameters, collapse=", "))
      # Generate a mask of records to retain
      parametersMask <- rep(FALSE, nrow(df))
      for (parameter in parameters) {
        if ( !parameter %in% unique(df$ParameterName) ) {
          logger.warn('parameters argument %s is not found in the data', parameters)
        } else {
          parametersMask <- parametersMask | df$ParameterName == parameter
        }
      }
      # Mask is complete, now apply it
      dfList[[i]] <- df[parametersMask,]
      
    }
    
  }
  
  # Combine all dataframes
  df <- dplyr::bind_rows(dfList)
  
  # Remove any duplicate rows
  df <- dplyr::distinct(df)
  
  if (is.null(parameters)) {
    logger.info('Downloaded %d rows of AirNow data', nrow(df))
  } else {
    logger.info('Downloaded %d rows of AirNow data for: %s', nrow(df), paste(parameters, collapse=", "))
  }
  
  return(df)
  
}
