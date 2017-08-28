#' @export
#' @title Load Current Airsis Data
#' @param lookbackDays number of days prior to now considered to be "current"
#' @param providers vector of data providers
#' @param usernames vector of usernames corresponding to \code{providers}
#' @param passwords vector of passwords corresponding to \code{providers}
#' @description Creates a ws_monitor object with data from AIRSIS monitors active within the last \code{lookbackDays} days.
#' @return \code{ws_monitor} object
#' 

airsis_currentData <- function(lookbackDays=7, providers=c("usfs","apcd","arb2","epa"), usernames, passwords) {
  
  # get combined currentStatus table
  currentStatusList <- list()
  for ( i in 1:length(providers) ) {
    logger.info(paste0("Generating currentStatus table for: ", providers[i]))
    currentStatusList[[providers[i]]] <- airsis_currentStatus(providers[i], usernames[i], passwords[i])
  }
  currentStatus <- dplyr::bind_rows(currentStatusList)
  
  # filter currentStatus to monitors updated within last lookbackDays days
  periodStart <- lubridate::now(tzone = "UTC") - lubridate::days(lookbackDays)
  currentStatus <- currentStatus[currentStatus$lastUpdateTime >= periodStart,]
  currentStatus <- currentStatus[!is.na(currentStatus$lastUpdateTime),]

  # arguments for airsis_createMonitorObject
  # NOTE: may wish to have different start date from date corresponding to lookbackDays
  startdate <- format(periodStart, "%Y%m%d")
  enddate <- format(lubridate::now(tzone = "UTC")+lubridate::days(1), "%Y%m%d")
  
  # get monitor objects for each monitor in currentStatus
  monitorList <- list()
  for ( rowID in 1:nrow(currentStatus) ) {
    provider <- currentStatus$provider[rowID]
    unitID <- currentStatus$unitID[rowID]
    logger.info(paste0("Creating ws_monitor object for ", provider, " ", unitID))
    ws_monitorTemp <- try(airsis_createMonitorObject(startdate, enddate, provider, unitID), silent = TRUE)
    if ( !( "try-error" %in% class(ws_monitorTemp))) {
      monitorList[[paste0(provider, "_", unitID)]] <- ws_monitorTemp
    }
  }
  ws_monitor <- monitor_combine(monitorList)
  
  return(ws_monitor)
  
}