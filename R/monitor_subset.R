#' @keywords ws_monitor
#' @export
#' @title Create a Subset of a ws_monitor Object
#' @param ws_monitor \emph{ws_monitor} object
#' @param xlim optional vector with low and high longitude limits
#' @param ylim optional vector with low and high latitude limits
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH] or \code{POSIXct})
#' @param vlim optional vector with low and high data value limits
#' @param monitorIDs optional vector of monitor IDs used to filter the data
#' @param stateCodes optional vector of state codes used to filter the data
#' @param countryCodes optional vector of country codes used to filter the data
#' @param dropMonitors flag specifying whether to remove monitors with no data
#' @param timezone Olson timezone passed to \code{link{parseDatetime}} when parsing numeric \code{tlim}
#' @return A \emph{ws_monitor} object with a subset of \code{ws_monitor}.
#' @description Creates a subset \emph{ws_monitor} based on one or more optional input parameters.
#' If any input parameter is not specified, that parameter will not be used to subset \code{ws_monitor}.
#' @details By default, this function will return a \emph{ws_monitor} object whose \code{data} dataframe has the
#' same number of columns as the incoming dataframe, unless any of the columns consist of all \code{NA}s, 
#' in which case such columns will be removed (\emph{e.g.} if there are no valid data for a specific monitor 
#' after subsetting by \code{tlim} or \code{vlim}).
#'If \code{dropMonitors=FALSE}, columns that consist of all \code{NA}s will be retained.
#' @examples
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150701,20150731))
#' xlim <- c(-124.73, -122.80)
#' ylim <- c(47.20, 48.40)
#' Olympic_Peninsula <- monitor_subset(N_M, xlim, ylim)
#' monitorMap(Olympic_Peninsula, cex=2)
#' rect(xlim[1], ylim[1], xlim[2], ylim[2], col=adjustcolor('black',0.1))

# TODO: In the "details" section above, might want to mention whether the 'meta' data is retained for monitors w/o valid data after subsetting

monitor_subset <- function(ws_monitor, xlim=NULL, ylim=NULL, tlim=NULL, vlim=NULL,
                           monitorIDs=NULL, stateCodes=NULL, countryCodes=NULL,
                           dropMonitors=TRUE, timezone="UTC") {
  
  # subset metadata
  meta <- monitor_subsetMeta(ws_monitor$meta, xlim=xlim, ylim=ylim, 
                             stateCodes=stateCodes, countryCodes=countryCodes,monitorIDs=monitorIDs)
  
  # sanity check
  if ( is.null(meta) ) {
    warning("No matching monitors found")
    ws_monitor <- list(meta=NULL, data=NULL)
    ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
    return (ws_monitor)
  }
  
  # Determine potentially reduced subset of monitorIDs
  # NOTE:  We can only work with monitors that have metadata and data so
  # NOTE:  only inclde those in the list of valid monitors.
  dataMonIDs <- colnames(ws_monitor$data)[-1]
  validMonIDs <- dplyr::intersect(meta$monitorID, dataMonIDs)
  
  # Subset data based on time, values and monitorIDs
  data <- monitor_subsetData(ws_monitor$data, tlim=tlim, vlim=vlim,
                             monitorIDs=validMonIDs, dropMonitors=dropMonitors, timezone=timezone)
  
  # sanity check
  if ( is.null(data) ) {
    warning("No matching monitors found")
    return ( list(data=NULL, meta=NULL) )
  }
  
  # Determine potentially reduced subset of monitorIDs due to tlim, vlim constraints
  validMonIDs <- names(data)[-1]
  
  # subset metadata one more time
  meta <- monitor_subsetMeta(ws_monitor$meta, monitorIDs=validMonIDs)
  
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
