#' @keywords AirNow
#' @export
#' @title Load AirNow Monitoring Data
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param monitorIDs The set of monitor IDs to be subsetted.
#' When set to NULL, the function returns all the monitor IDs available in the metadata
#' @param parameter The parameter of interest
#' @param baseUrl base URL for AirNow meta and data files
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description When given the startdate, enddate, monitorIDs and parameter of interest, the function retrieves the 
#' meta and data files from the archive baseUrl (or local directory) and return the subsetted ws_monitor object.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150901, 20150930)
#' airnow_conus <- monitor_subset(airnow, stateCodes=CONUS)
#' monitor_leaflet(airnow_conus)
#' }

airnow_load <- function(startdate, enddate, monitorIDs=NULL, parameter='PM2.5',
                        baseUrl='http://smoke.airfire.org/RData/AirNowTech/') {
  
  # Sanity Check
  
  if ( is.null(startdate) | is.null(enddate) ) {
    
    stop("The time interval is not defined")
    
  }
  
  # Get relevant metadata (The url will change once PWFSL has its own archive url)
  
  if (stringr::str_sub(baseUrl,1,4) == 'http') {
    
    meta <- get(load(url(paste0(baseUrl,'AirNowTech_', 'PM2.5_', 'SitesMetadata', '.RData'))))
    
  } else if (stringr::str_sub(baseUrl,1,6) == '/Users') { #  TODO:  See logic in hms_loadSmoke for an improvement
    
    meta <- get(load(paste0(baseUrl,'AirNowTech_', 'PM2.5_', 'SitesMetadata', '.RData')))
    
  } else {
    
    stop("Please provide a web url or an absolute path to the local directory")
  }
  
  # Get relevant data
  # Concatenate using dplyr::row_bind if there are multiple files
  
  dataList <- list()
  
  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate)
  
  timeVec <- unique(strftime(seq(starttime, endtime, by='days'), '%Y%m', tz='GMT'))
  
  if (length(timeVec) == 1) {
    
    YearMonth <- timeVec
    
    if (stringr::str_sub(baseUrl,1,4) == 'http') {
      
      joinedData <- get(load(url(paste0(baseUrl,'AirNowTech_',parameter,'_', YearMonth,'.RData'))))
      
    } else {
      
      joinedData <- get(load(paste0(baseUrl,'AirNowTech_',parameter,'_', YearMonth,'.RData')))
      
    }
    
  } else {
    
    if (stringr::str_sub(baseUrl,1,4) == 'http') {
      
      for (time in timeVec) {
        
        YearMonth <- time
        dataList[[YearMonth]] <- get(load(url(paste0(baseUrl,'AirNowTech_',parameter,'_', YearMonth,'.RData'))))
        
      }
      
    } else {
      
      for (time in timeVec) {
        
        YearMonth <- time
        dataList[[YearMonth]] <- get(load(paste0(baseUrl,'AirNowTech_',parameter,'_', YearMonth,'.RData')))
        
      }
      
    }
    
    joinedData <- suppressMessages(dplyr::bind_rows(dataList))
    joinedData <- joinedData[!duplicated(joinedData$datetime),]
    joinedData <- as.data.frame(joinedData)
    
  }
  
  # Subset time and monitor ID
  
  ws_monitor <- list(meta=meta, data=joinedData)
  tlim <- c(starttime,endtime)
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim, monitorIDs=monitorIDs)
  
  # Return ws_monitor object
  
  return(structure(ws_monitor, class = c("ws_monitor", "list")))
  
}
