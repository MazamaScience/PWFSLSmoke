#' @keywords monitor
#' @export
#' @title Trim a Monitor Time Axis
#' @param ws_monitor data list of class \code{ws_monitor}
#' @description The incoming \code{ws_monitor} object has its time axis trimmed
#' to include everything from the first to last valid datapoints for any monitor
#' in the object. All timesteps before or after any valid data are removed
#' @return A \code{ws_monitor} objects.

monitor_trim <- function(ws_monitor) {
  
  # Vectors of first and last valid indices excluding 'datetime'
  firstValids <- unlist(lapply(ws_monitor$data, function(x) { min(which(!is.na(x))) }))[-1]
  lastValids <- unlist(lapply(ws_monitor$data, function(x) { max(which(!is.na(x))) }))[-1]
  
  # Subset the data dataframe
  ws_monitor$data <- ws_monitor$data[max(firstValids):min(lastValids),]
  
  return(ws_monitor)
}
