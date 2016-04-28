#' @keywords ws_monitor
#' @export
#' @title Subset Monitoring Data
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param xlim optional longitude lim with lo and hi longitude values
#' @param ylim optional latitude lim with lo and hi latitude values
#' @param tlim optional time lim with lo and hi time values (POSIXct)
#' @param vlim optional data lim with lo and hi data values
#' @param stateCodes optional vector of stateCodes
#' @param monitorIDs optional vector of monitorIDs
#' @param dropMonitors flag specifying whether to remove monitors with no data
#' @description The incoming monitoring data list is filtered according to the parameters
#' passed in.  If any parameter is not specified, that parameter will not be used in the filtering.
#' @details By default, filtering by tlim or vlim will always return a 'data' dataframe with the
#'     same number of columns as the incoming dataframe. Some columns may consist sof all \code{NA}s.
#'     If \code{dropMonitors=TRUE}, columns
#'     will be removed if there are not valid data for a specific monitor after subsetting.
#' @return monitoring data list filtered as specified
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' xlim <- c(-124.801, -122.801)
#' ylim <- c(47.004, 48.404)
#' Olympic_Peninsula <- monitor_subset(airnow, xlim, ylim)
#' monitor_map(Olympic_Peninsula)
#' }

monitor_subset <- function(ws_monitor, xlim=NULL, ylim=NULL, tlim=NULL, vlim=NULL,
                           monitorIDs=NULL, stateCodes=NULL, dropMonitors=TRUE) {
  
  # subset metadata
  meta <- monitor_subsetMeta(ws_monitor$meta, xlim=xlim, ylim=ylim, stateCodes=stateCodes, monitorIDs=monitorIDs)
  
  # sanity check
  if ( is.null(meta) ) {
    warning("No matching monitors found")
    return (list(data=NULL, meta=NULL))
  }
  
  # Determine potentially reduced subset of monitorIDs
  # NOTE:  We can only work with monitors that have metadata and data so
  # NOTE:  only inclde those in the list of valid monitors.
  dataMonIDs <- colnames(ws_monitor$data)[-1]
  validMonIDs <- dplyr::intersect(meta$monitorID, dataMonIDs)
  
  # Sanity check -- accept numeric values for tlim
  if ( !is.null(tlim) ) {
    if (class(tlim)[1] == 'numeric') tlim <- parseDatetime(tlim)
  }
   
  # Subset data based on time, values and monitorIDs
  data <- monitor_subsetData(ws_monitor$data, tlim=tlim, vlim=vlim,
                             monitorIDs=validMonIDs, dropMonitors=dropMonitors)
  
  # sanity check
  if ( is.null(data) ) {
    warning("No matching monitors found")
    return ( list(data=NULL, meta=NULL) )
  }
  
  # Determine potentially reduced subset of monitorIDs due to tlim, vlim constraints
  validMonIDs <- names(data)[-1]
  
  # subset metadata one more time
  meta <- monitor_subsetMeta(ws_monitor$meta, monitorIDs=validMonIDs)
  
  ws_monitor <- list(data=data, meta=meta)
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}
