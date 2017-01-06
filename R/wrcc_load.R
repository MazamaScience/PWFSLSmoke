#' @keywords wrcc
#' @export
#' @title Load WRCC monitoring data
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param stateCodes optional vector of stateCodes used to subset the data
#' @param monitorIDs optional vector of monitorIDs used to subset the data
#' @param url local file path or url containing the \code{ws_monitor} object to be loaded
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description When given the startdate, enddate, monitorIDs and stateCodes, the function retrieves 
#' data from url and returns the subsetted ws_monitor object.
#' @examples
#' \dontrun{
#' wrcc <- wrcc_load(20150901, 20150930)
#' wrcc_conus <- monitor_subset(wrcc, stateCodes=CONUS)
#' monitor_leaflet(wrcc_conus)
#' }

wrcc_load <- function(startdate, enddate, monitorIDs=NULL, stateCodes=NULL, 
                      url='https://smoke.airfire.org/RData/WRCC/WRCC_monitors.RData') {
  
  # Load either from a URL for a local file
  if ( stringr::str_detect(url, '^http://') ) {
    ws_monitor <- get(load(url(url)))
  } else {
    ws_monitor <- get(load(url))
  }
  
  tlim <- c(parseDatetime(startdate), parseDatetime(enddate))
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim,monitorIDs=monitorIDs,
                               stateCodes=stateCodes, dropMonitors=TRUE)
  
  return(ws_monitor)
}
