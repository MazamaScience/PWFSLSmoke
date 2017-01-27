#' @keywords AIRSIS
#' @export
#' @title Load AIRSIS Monitoring Data
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param stateCodes optional vector of stateCodes used to subset the data
#' @param monitorIDs optional vector of monitorIDs used to subset the data
#' @param url location of the meta and data files (url or local file)
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description When given the startdate, enddate, monitorIDs and parameter of interest, the function retrieves 
#' data from the archive url and returns the subsetted ws_monitor object.
#' @examples
#' \dontrun{
#' airsis <- airsis_load(20140901, 20141130)
#' }

airsis_load <- function(startdate=20000101,
                        enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                        monitorIDs=NULL, stateCodes=NULL, 
                        url='https://smoke.airfire.org/RData/AIRSIS/AIRSIS_monitors.RData') {
  
  # WRCC data is already stored in a single ws_monitor object
  if (stringr::str_detect(url,'http:\\/\\/')) {
    ws_monitor <- get(load(url(url)))
  } else {
    ws_monitor <- get(load(url))
  }
  
  tlim <- c(parseDatetime(startdate), parseDatetime(enddate))
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim,monitorIDs=monitorIDs,
                               stateCodes=stateCodes, dropMonitors=TRUE)
  
  return(ws_monitor)
}
