#' @keywords AirNow
#' @export
#' @title Download and Aggregate Multiple Hourly Data Files from AirNow
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param hours desired number of hours of data to assemble
#' @description This function makes repeated calls to \link{airnow_downloadHourlyData}
#' to obtain data from AirNow. All data obtained are then
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
#' @note:  As of 2016-12-27, it appears that hourly data are available only for 2016 and
#' not for earlier years.
#' @return Dataframe of aggregated AirNow data.
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadHourlyData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadData("PM2.5", 2016070112, hours=24)
#' }

airnow_downloadData <- function(parameters=NULL, startdate='', hours=24) {
  
  # Format the startdate integer using lubridate
  starttime <- parseDatetime(startdate)

  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  dfList <- vector(mode="list", length=hours)
  
  logger.info('Downloading %d hourly data files from AirNow...',hours)
  
  # Loop through the airnow_downloadHourlyData function and store each datafame in the list
  for (i in 1:hours) {

    datetime <- starttime + lubridate::dhours(i-1)
    datestamp <- strftime(datetime, "%Y%m%d%H", tz="UTC")

    logger.debug('Downloading AirNow data for %s', datestamp)
    
    # Obtain an hour of AirNow data
    result <- try( df <- airnow_downloadHourlyData(datestamp),
                   silent=TRUE)
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.error('Unable to obtain data: %s',err_msg)
      next
    }

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
