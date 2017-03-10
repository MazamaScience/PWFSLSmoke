#' @keywords ws_monitor
#' @export
#' @title Trim ws_monitor Time Axis to Remove NA Periods From Beginning and End
#' @param ws_monitor emph{ws_monitor} object
#' @return A emph{ws_monitor} object with missing data trimmed.
#' @description Trims the time axis of a emph{ws_monitor} object to exclude timestamps prior to the first and
#' after the last valid datapoint for any monitor.
#' @examples
#' \dontrun{
#' sm13 <- wrcc_createMonitorObject(20150101, 20151231, stationID='sm13')
#' sm13$meta[,c('stateCode','countyName','siteName','monitorID')]
#' Deschutes <- monitor_subset(sm13, monitorIDs='Smoke..13__001')
#' Deschutes <- monitor_trim(Deschutes)
#' monitorPlot_dailyBarplot(Deschutes)
#' }
monitor_trim <- function(ws_monitor) {
  
  # Vectors of first and last valid indices excluding 'datetime'
  firstValids <- unlist(lapply(ws_monitor$data, function(x) { min(which(!is.na(x))) }))[-1]
  lastValids <- unlist(lapply(ws_monitor$data, function(x) { max(which(!is.na(x))) }))[-1]
  
  # Subset the data dataframe
  ws_monitor$data <- ws_monitor$data[max(firstValids):min(lastValids),]
  
  return(ws_monitor)
}
