#' @keywords OpenAQ
#' @export
#' @title Load OpenAQ Monitoring Data
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param monitorIDs vector of monitor IDs used for subsetting
#' When set to NULL, the function returns all the monitor IDs available in the metadata
#' @param parameter parameter of interest
#' @param baseUrl base URL for OpenAQ meta and data files (note: the default baseURL cannot be used yet)
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description When given the startdate, enddate, monitorIDs and parameter of interest, the function retrieves the 
#' meta and data files from the archive baseUrl (or local directory) and return the subsetted ws_monitor object.
#' 
#' Available parameters include:
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
#' Note that data for some days in 2015 are missing and data only becomes available starting 06/29/2015, imcomplete 
#' months with their available days are:
#' \enumerate{
#' \item{201506}{:  6/29 - 6/30}
#' \item{201507}{:  7/1 - 7/4, 7/6 - 7/18, 7/20 - 7/24}
#' \item{201508}{:  8/2 - 8/31}
#' \item{201512}{:  12/1 - 12/30}
#' }
#' 
#' @examples
#' \dontrun{
#' openaq <- openaq_load(20150901, 20150930)
#' openaq_conus <- monitor_subset(openaq, stateCodes=CONUS, countryCodes=US)
#' monitorInteractiveMap(openaq_conus)
#' }


openaq_load <- function(startdate, enddate, monitorIDs=NULL, parameter='pm25',
                        baseUrl='https://smoke.airfire.org/RData/OpenAQ/') {
  
  # Sanity Check
  if ( is.null(startdate) | is.null(enddate) ) {
    
    stop("The time interval is not defined")
    
  }
  
  
  dataList <- list()
  metaList <- list()
  
  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate) + 60*60*24 - 1
  
  timeVec <- unique(strftime(seq(starttime, endtime, by='days'), '%Y%m', tz='GMT'))
  
  if (length(timeVec) == 1) {
    
    YearMonth <- timeVec
    
    if (stringr::str_sub(baseUrl,1,4) == 'http') {
      
      joinedData <- get(load(url(paste0(baseUrl,'openAQ_', parameter, "_", YearMonth, '_Datadata.RData'))))
      joinedMeta <- get(load(url(paste0(baseUrl,'openAQ_', parameter, "_", YearMonth, '_Metadata.RData'))))
      
    } else {
      
      joinedData <- get(load(paste0(baseUrl, 'openAQ_', parameter, "_", YearMonth, '_Datadata.RData')))
      joinedMeta <- get(load(paste0(baseUrl, 'openAQ_', parameter, "_", YearMonth, '_Metadata.RData')))
      
    }
    
  } else {
    
    if (stringr::str_sub(baseUrl,1,4) == 'http') {
      
      for (time in timeVec) {
        
        YearMonth <- time
        dataList[[YearMonth]] <- get(load(url(paste0(baseUrl, 'openAQ_',parameter, "_",YearMonth, '_Datadata.RData'))))
        metaList[[YearMonth]] <- get(load(url(paste0(baseUrl, 'openAQ_',parameter, "_",YearMonth, '_Metadata.RData'))))
        
      }
      
    } else {
      
      for (time in timeVec) {
        
        YearMonth <- time
        dataList[[YearMonth]] <- get(load(paste0(baseUrl,'openAQ_',parameter, "_",YearMonth, '_Datadata.RData')))
        metaList[[YearMonth]] <- get(load(paste0(baseUrl,'openAQ_',parameter, "_",YearMonth, '_Metadata.RData')))
        
      }
      
    }
  
    joinedData <- suppressMessages(dplyr::bind_rows(dataList))
    joinedData <- joinedData[!duplicated(joinedData$datetime),]
    joinedData <- as.data.frame(joinedData)
    
    joinedMeta <- suppressMessages(dplyr::bind_rows(metaList))
    joinedMeta <- joinedMeta[!duplicated(joinedMeta$monitorID),]
    joinedMeta <- as.data.frame(joinedMeta)
    
  }
  
  # Subset time and monitor ID
  
  ws_monitor <- list(meta=joinedMeta, data=joinedData)
  tlim <- c(starttime,endtime)
  
  ##### TODO: subset by parameter, may need to modify openaq_createDataDataframes
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim, monitorIDs=monitorIDs)
  
  # Return ws_monitor object
  
  return(structure(ws_monitor, class = c("ws_monitor", "list")))
    
  
}
