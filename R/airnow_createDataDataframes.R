#' @keywords AirNow
#' @export
#' @title Return Reshaped, Monthly Dataframes of AirNow Data
#' @param user user name
#' @param pass pass
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param yearMonth desired year and month  (integer or character representing YYYYMM)
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical requesting verbose output from libcurl
#' @description The airnow_createDataDataframe() function uses the \link{airnow_downloadData} function 
#' to download monthly dataframes of AirNow data and restructures that data into a format that is compatible
#' with the PWFSLSmoke package \emph{ws_monitor} data model.
#' 
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
#' Setting \code{parameters=NULL} will generate a separate dataframe for each of the above parameters.
#' @return Returns a list of data frames where each dataframe is contains all data for a unique parameter (e.g: "PM2.5", "NOX").
#' @seealso \link{airnow_downloadData}
#' @seealso \link{airnow_qualityControl}
#' @examples
#' \dontrun{
#' airnowData <- airnow_createDataDataframe(user, pass, "PM2.5", 201507)
#' }

airnow_createDataDataframes <- function(user, pass, parameters=NULL, yearMonth, tries=6, verbose=FALSE) {
  
  # ----- Data Download -------------------------------------------------------
  
  logger.info('Downloading AirNow data for %s...',yearMonth)
  
  # Calculate the number of hours in the month of the interest
  startdate <- paste0(yearMonth, '0100')
  starttime <- lubridate::ymd_h(startdate)
  hours <- 24 * as.numeric(lubridate::days_in_month(starttime))
  
  # Create the data frame that holds a month worth of AirNow data
  airnowRaw <- airnow_downloadData(user, pass, parameters=parameters, startdate=startdate, hours=hours, tries=tries, verbose=verbose)
  
  # ----- Data Reshaping ------------------------------------------------------
  
  logger.debug('Reshaping AirNow data for %s...',yearMonth)
  
  # NOTE:  Example lines from the aggregated dataframe:
  # NOTE:
  # NOTE:    ValidDate ValidTime     AQSID   SiteName GMTOffset ParameterName ReportingUnits Value                  DataSource
  # NOTE:  1  08/01/14     00:00 000010102 St. John's        -4         OZONE            PPB    12 Newfoundland & Labrador DEC
  # NOTE:  2  08/01/14     00:00 000020301 WELLINGTON        -4           NO2            PPB     0          Environment Canada
  # NOTE:  3  08/01/14     00:00 000020301 WELLINGTON        -4         OZONE            PPB    18          Environment Canada
  # NOTE:  4  08/01/14     00:00 000020301 WELLINGTON        -4         PM2.5          UG/M3     7          Environment Canada
  # NOTE:  5  08/01/14     00:00 000020301 WELLINGTON        -4            NO            PPB     0          Environment Canada
  
  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(airnowRaw$ParameterName))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(airnowRaw$ParameterName))
    invalidParameters <- dplyr::setdiff(parameters, unique(airnowRaw$ParameterName))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in AirNow data: %s", paste0(invalidParameters, collapse=", "))
    }
  }
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
  
  # Use dplyr and reshape2 packages to seprate the data by the types and restructure each data frame
  for (parameter in parameters) {
    
    logger.debug('Reshaping data for %s...', parameter)
    
    # Create datetime variable
    df <- dplyr::filter(airnowRaw, airnowRaw$ParameterName == parameter)
    datestamp <- paste0(df$ValidDate, ' ', df$ValidTime)
    df$datetime <- lubridate::mdy_hm(datestamp) # 'mdy_hm', not 'ymd_hm'
    # Guarantee unique rows
    df <- dplyr::distinct(df)
    # Melt and recast
    melted <- reshape2::melt(df, id.vars=c('datetime','AQSID'), measure.vars=c('Value'))
    dfList[[parameter]] <- reshape2::dcast(melted, datetime ~ AQSID)
    
  }
  
  # NOTE:  Some parameters, especially those with few monitors, may not have measurements for
  # NOTE:  for every single hour. Here we guarantee that the reshaped dataframes we return will
  # NOTE:  have a row for every single hour in a month, even if that row is filled with NAs.
  
  # Guarantee that all times are present by starting with a dataframe containing only a uniform time axis.
  timeAxis <- seq(starttime, starttime + lubridate::dhours(hours), by='hours')
  timeDF <- data.frame(datetime=timeAxis)
  
  logger.info('Putting data on a uniform time axis...')
  for (parameter in parameters) {
    # Join data to uniform time axis
    dfList[[parameter]] <- suppressMessages( dplyr::full_join(timeDF, dfList[[parameter]]) )
    
    # NOTE:  Check this URL for some EPA defined levels:
    # NOTE:    https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.csv
    
    # Assume this data has been QC'ed and let everything through
    airnow_qualityControl(dfList[[parameter]], limits=c(-Inf,Inf))
    
  }
  
  return(dfList)
  
}

