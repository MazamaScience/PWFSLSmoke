#' @keywords OpenAQ
#' @export
#' @title Obtain OpenAQ Data and Create ws_monitor Object
#' @param parameter pollutant name
#' @param startdate desired staring date (integer or character representing YYYYMMDD)
#' @param days desired number of days of data to assemble
#' @param countryCode two-character country code (ISO 3166-1 alpha-2)
#' @description Obtains monitor data from OPENAQ and converts
#' it into a metadata enhanced \code{ws_monitor} object
#' ready for use with all \code{monitor_~} functions.
#' 
#' Available parameters are limited to:
#' PM2.5, PM10, ozone (O3), sulfur dioxide (SO2), nitrogen dioxide (NO2), 
#' carbon monoxide (CO), and black carbon (BC)
#' \enumerate{
#' \item{pm25}{ -- PM2.5}
#' \item{pm10}{ -- PM10}
#' \item{o3}{ -- ozone }
#' \item{so2}{ -- sulfer dioxide }
#' \item{no2}{ -- nitrogen dioxide}
#' \item{co}{ -- carbon monoxide}
#' \item{bc}{ -- black carbon}
#' }
#' 
#' @return ws_monitor object with a unique `monitorID`.
#' @examples
#' \dontrun{
#' df <- openaq_downloadData(20160901, days=7)
#' }

openaq_createMonitorObject <- function(parameter='pm25',
                                       startdate='', days=1, countryCode=NULL) {
  
  # Sanity check: only one parameter allowed
  if ( is.null(parameter) || length(parameter) > 1 ) {
    logger.error("parameter must be one of:  'pm25','pm10','o3','so2','no2','co','bc'")
    stop(paste0("parameter must be one of:  'pm25','pm10','o3','so2','no2','co','bc'")) 
  }
  
  # Sanity check: format of startdate
  if ( is.null(startdate) || startdate == '') {
    logger.error("Required parameter 'startdate' is missing")
    stop(paste0("Required parameter 'startdate' is missing.")) 
  } else if ( (as.numeric(startdate) + days) > stringr::str_replace_all(Sys.Date(),'-','') ) {
    logger.error("Parameter 'startdate' has to be an earlier date.")
    stop(paste0("Parameter 'startdate' has to be an earlier date."))
  }
  
  # download the openaq data as a dataframe
  logger.info('Downloading data...')
  df <- openaq_downloadData(parameter, startdate, days, countryCode)
  
  # ----- Quality Control begin -----------------------------------------------
  
  # Remove any records missing latitude or longitude
  badLocationMask <- is.na(df$longitude) | is.na(df$latitude)
  badLocationCount <- sum(badLocationMask)
  if ( badLocationCount > 0 ) {
    logger.info('Discarding %s rows with invalid location information', badLocationCount)
    badLocations <- paste('(',df$longitude[badLocationMask],',',df$latitude[badLocationMask],')',sep='')
    logger.debug('Bad locations: %s', paste0(badLocations, collapse=", "))
  }
  df <- df[!badLocationMask,]
  
  # ----- Quality Control end -------------------------------------------------
  
  # add additional columns to the dataframe
  logger.info('Adding \'datetime\', \'countryCode\', \'stateCode\', \'monitorID\' columns...')
  
  df$datetime <- lubridate::ymd_hms(df$utc)
  df$countryCode <- df$country
  df <- openaq_assignStateCode(df)
  
  # The monitorID column is composed of four lication elements as the unique identifier 
  df$monitorID <- make.names( with(df, paste(location,city,stateCode,countryCode) ) )

  # create metadata for the data frame
  logger.info('Creating \'meta\' dataframe...')
  meta <- openaq_createMetaDataframe(df)
  
  # create datadata for the data frame
  logger.info('Creating \'data\' dataframe...')
  data <- openaq_createDataDataframe(df)
  
  # create the ws_monitor object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor", "list"))
  
  return(ws_monitor)
}