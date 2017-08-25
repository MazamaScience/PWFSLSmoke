#' @export
#' @title Load Current Airsis Data
#' @param lookbackDays number of days prior to now considered to be "current"
#' @param providers vector of data providers
#' @param usernames vector of usernames corresponding to \code{providers}
#' @param passwords vector of passwords corresponding to \code{providers}
#' @return \code{ws_monitor} object with data for monitors with new data in last \code{lookbackDays} days
#' 

airsis_currentData <- function(lookbackDays=7, providers=c("usfs","apcd","arb2","epa"), usernames, passwords) {
  
  # get combined currentStatus table
  currentStatusList <- list()
  for ( i in 1:length(providers) ) {
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
    monitorList[[paste0(provider, "_", unitID)]] <- airsis_createMonitorObject(startdate, enddate, provider, unitID)
  }
  ws_monitor <- monitor_combine(monitorList)
  
  return(ws_monitor)
  
}