#' @keywords wrcc
#' @export
#' @title Load WRCC monitoring data
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param stateCodes optional vector of stateCodes used to subset the data
#' @param monitorIDs optional vector of monitorIDs used to subset the data
#' @param url The location of the meta and data files (Default = 'http://smoke.airfire.org/RData/AirNowTech/')
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description When given the startdate, enddate, monitorIDs and parameter of interest, the function retrieves 
#' data from the archive url and returns the subsetted ws_monitor object.
#' @examples
#' \dontrun{
#' wrcc <- wrcc_load(20150901, 20150930)
#' wrcc_conus <- monitor_subset(wrcc, stateCodes=CONUS)
#' monitor_leaflet(wrcc_conus)
#' }

wrcc_load <- function(startdate, enddate, monitorIDs=NULL, stateCodes=NULL, 
                        url='http://smoke.airfire.org/RData/WRCC/WRCC_monitors.RData') {
  
  # WRCC data is already stored in a single ws_monitor object
  ws_monitor <- get(load(url(url)))
  
  tlim <- c(parseDatetime(startdate), parseDatetime(enddate))
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim,monitorIDs=monitorIDs,
                               stateCodes=stateCodes, dropMonitors=TRUE)

  return(ws_monitor)
}
