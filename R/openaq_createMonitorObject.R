#' @keywords OpenAQ
#' @export
#' @title Obtain OpenAQ Data and Create ws_monitor Object
#' @param countryCode two-character country code (ISO 3166-1 alpha-2)
#' @param parameter pollutant name
#' @param startdate desired staring date (integer or character representing YYYYMMDD)
#' @param days desired number of days of data to assemble
#' @param verbose logical flag to generate verbose output
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

openaq_createMonitorObject <- function(countryCode='US', parameter='pm25',
                                       startdate='', days=1, verbose=FALSE) {
  
  # download the openaq data as a dataframe
  df <- openaq_downloadData(countryCode, parameter, startdate, days, verbose)
  
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
  meta <- openaq_createMetaDataframe(df)
  
  # create datadata for the data frame
  data <- openaq_createDataDataframe(df)
  
  # create the ws_monitor object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor", "list"))
  
  return(ws_monitor)
}