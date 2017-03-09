#' @keywords ws_monitor
#' @export
#' @title Combine List of ws_monitor Objects into Single ws_monitor Object
#' @param monitorList list containing \emph{ws_monitor} objects
#' @return A emph{ws_monitor} object combining all monitoring data from \code{monitorList}.
#' @description Combines a list of \emph{ws_monitor} objects into a single \emph{ws_monitor} object 
#' by merging the \code{meta} and \code{data} dataframes from each object in \code{monitorList}.
#' @examples
#' \dontrun{
#' monitorList <- list()
#' monitorList[[1]] <- airsis_createMonitorObject(20160701, 20161231, 'USFS', '1031')
#' monitorList[[2]] <- airsis_createMonitorObject(20160701, 20161231, 'USFS', '1032')
#' monitorList[[3]] <- airsis_createMonitorObject(20160701, 20161231, 'USFS', '1033')
#' monitorList[[4]] <- airsis_createMonitorObject(20160701, 20161231, 'USFS', '1034')
#' ws_monitor <- monitor_combine(monitorList)
#' monitorLeaflet(ws_monitor)
#' } 

monitor_combine <- function(monitorList) {
  
  # Extract lists of 'meta' and 'data' dataframes
  metaList <- lapply(monitorList, function(x) { return(x$meta) })
  dataList <- lapply(monitorList, function(x) { return(x$data) })
  
  # Create combined 'meta'
  meta <- dplyr::bind_rows(metaList)
  rownames(meta) <- meta$monitorID
  
  # Create combined 'data'
  data <- dataList[[1]]
  for (i in 2:length(dataList)) {
    data <- dplyr::full_join(data,dataList[[i]],by="datetime")
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=as.data.frame(meta), data=as.data.frame(data))
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
