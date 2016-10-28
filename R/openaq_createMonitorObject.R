#' @keywords OpenAQ
#' @export
#' @title Obtain OpenAQ Data and Create ws_monitor Object
#' @param parameter pollutant name
#' @param startdate desired staring date (integer or character representing YYYYMMDD)
#' @param days desired number of days of data to assemble
#' @param countryCode two-character country code (ISO 3166-1 alpha-2)
#' @param saveFile optional filename where raw CSV will be written
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
                                       startdate='', days=1, countryCode='US',
                                       saveFile=NULL) {
  
  # Sanity check: format of startdate
  if ( is.null(startdate) | startdate == '') {
    logger.error("Required parameter 'startdate' is missing")
    stop(paste0("Required parameter 'startdate' is missing.")) 
  } else if ( (as.numeric(startdate) + days) > stringr::str_replace_all(Sys.Date(),'-','') ) {
    logger.error("Parameter 'startdate' has to be an earlier date.")
    stop(paste0("Parameter 'startdate' has to be an earlier date."))
  }
  
  # download the openaq data as a dataframe
  logger.info('Downloading data...')
  df <- openaq_downloadData(parameter, startdate, days, countryCode)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try(write.csv(df,saveFile),silent = TRUE)
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.warn('Unable to save data to local file %s: %s', saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # add additional columns to the dataframe
  logger.info('Adding \'datetime\', \'stateCode\', \'monitorID\' columns...')
  
  # add datetime and monitorID column
  df$datetime <- lubridate::ymd_hms(df$local)
  
  # extract unique combinations of latitudes and longitudes for faster buffering process
  uniqueLatLon <- unique(paste(df$latitude, df$longitude))
  uniqueLatLon <- stringr::str_split_fixed(uniqueLatLon, ' ', 2)
  colnames(uniqueLatLon) <- c("latitude", "longitude")
  uniqueLatLon <- apply(uniqueLatLon,2,as.numeric)
  stateCodes <- getStateCode(uniqueLatLon[,"longitude"], uniqueLatLon[,"latitude"], useBuffering = TRUE) 
  
  # correct non-US state codes  
  stateCodes[which(stateCodes == '')] <- 'PR'
  stateCodes[which(stateCodes == 'TM' | stateCodes == 'CH')] <- 'TX'
  stateCodes[which(stateCodes == 'BC')] <- 'ID'
  
  # assign state codes accordingly
  df$stateCode <- NA
  for (i in 1:nrow(df)) {
    latIndex <- which(uniqueLatLon[, 1] == df$latitude[i])
    lonIndex <- which(uniqueLatLon[, 2] == df$longitude[i])
    df$stateCode[i] <- stateCodes[intersect(latIndex,lonIndex)]
  }
  
  # create a monitorID column as unique identifier 
  df$monitorID <- with(df,paste(location,city,stateCode,sep=', '))
  
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