#' @keywords ws_monitor
#' @export
#' @title Combine List of ws_monitor Objects into Single ws_monitor Object
#' @param monitorList list containing one or more \emph{ws_monitor} objects
#' @return A \emph{ws_monitor} object combining all monitoring data from \code{monitorList}.
#' @description Combines a list of one or more \emph{ws_monitor} objects into a single \emph{ws_monitor} object 
#' by merging the \code{meta} and \code{data} dataframes from each object in \code{monitorList}.
#' 
#' When \code{monitorList} contains only two \emph{ws_monitor} objects the \code{monitor_combine()} 
#' function can be used to extend time ranges for monitorIDs that are found in both \emph{ws_monitor}
#' objects. This can be used to 'grow' a \emph{ws_monitor} object by appending subsequent months
#' or years. (Note, however, that this can be CPU intensive process.)
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
  
  # Stop and return first element of monitorList as ws_monitor object if only one element in list
  # Note that there can be multiple monitors within a single element
  if ( length(monitorList) == 1 ) {
    return(monitorList[[1]])
  }
  
  # Check for multiple occurrences of monitorID
  allMonitorIDs <- unlist( lapply(monitorList, function(x) { return(x$meta$monitorID) }) )
  duplicateIDs <- allMonitorIDs[which(duplicated(allMonitorIDs))]
  if ( length(duplicateIDs) > 0 ) {
    
    # If there are only two ws_monitor objects we can join them
    if ( length(monitorList) > 2 ) {
      stop("Joining of duplicate monitors requires that monitorList have only two ws_monitor objects.")
    }
    
    warning('Joining data with shared monitorIDs')
    
    # Create a new monitorList which separates mon1-only, mon2-only and joined
    monitorIDs1 <- setdiff(monitorList[[1]]$meta$monitorID, duplicateIDs)
    monitorIDs2 <- setdiff(monitorList[[2]]$meta$monitorID, duplicateIDs)
    mon1 <- monitor_subset(monitorList[[1]], monitorIDs=monitorIDs1)
    mon2 <- monitor_subset(monitorList[[2]], monitorIDs=monitorIDs2)
    joined_dups <- monitor_join(monitorList[[1]], monitorList[[2]], duplicateIDs)
    monitorList <- list(mon1, mon2, joined_dups)
    
  }
  
  # Extract lists of 'meta' and 'data' dataframes
  metaList <- lapply(monitorList, function(x) { return(x$meta) })
  dataList <- lapply(monitorList, function(x) { return(x$data) })
  
  # Create combined 'meta'
  meta <- dplyr::bind_rows(metaList)
  meta <- as.data.frame(meta, stringsAsFactors=FALSE) # Guarantee we are still a dataframe, not a tibble
  rownames(meta) <- meta$monitorID
  
  # Create combined 'data'
  data <- dataList[[1]]
  for (i in 2:length(dataList)) {
    data <- dplyr::full_join(data, dataList[[i]], by="datetime")
  }
  data <- as.data.frame(data, stringsAsFactors=FALSE)
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
