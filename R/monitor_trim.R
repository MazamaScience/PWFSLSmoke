#' @keywords ws_monitor
#' @export
#' @title Trim ws_monitor Time Axis to Remove NA Periods From Beginning and End
#' @param ws_monitor ws_monitor object
#' @description Trims the time axis of a ws_monitor object to exclude timestamps prior to the first and
#' after the last valid datapoint for any monitor.
#' @return ws_monitor object

monitor_trim <- function(ws_monitor) {
  
  # Vectors of first and last valid indices excluding 'datetime'
  firstValids <- unlist(lapply(ws_monitor$data, function(x) { min(which(!is.na(x))) }))[-1]
  lastValids <- unlist(lapply(ws_monitor$data, function(x) { max(which(!is.na(x))) }))[-1]
  
  # Subset the data dataframe
  ws_monitor$data <- ws_monitor$data[max(firstValids):min(lastValids),]
  
  return(ws_monitor)
}
