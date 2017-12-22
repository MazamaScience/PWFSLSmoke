#' @keywords ws_monitor
#' @export
#' @title Stitch Together Monitors with Shared monitorIDs
#' @param ws_monitor1 \emph{ws_monitor} object
#' @param ws_monitor2 \emph{ws_monitor} object
#' @param monitorIDs vector of shared monitorIDs that are to be joined together
#' @return A \emph{ws_monitor} object with merged timeseries.
#' @description For each monitor in \code{monitorIDs}, an attempt is made to merge
#' the associated data from \code{ws_monitor} and \code{ws_monitor2} and.
#' 
#' This is useful when the same \code{monitorID} appears in different \emph{ws_monitor}
#' objects representing different time periods.
#' @examples
#' \dontrun{
#' Jul <- monitor_subset(Northwest_Megafires,
#'                       tlim=c(20150701,20150730),
#'                       timezone='America/Los_Angeles')
#' Aug <- monitor_subset(Northwest_Megafires,
#'                       tlim=c(20150801,20150831),
#'                       timezone='America/Los_Angeles')
#' Methow_Valley <- monitor_join(Jul, Aug, monitorIDs=c('530470010','530470009'))
#' }
monitor_join <- function(ws_monitor1=NULL,
                         ws_monitor2=NULL,
                         monitorIDs=NULL) {
  
  # Sanity check
  if ( !"ws_monitor" %in% class(ws_monitor1) ) {
    logger.error("Required argument 'ws_monitor1' is not a ws_monitor object")
    stop("Required argument 'ws_monitor1' is not a ws_monitor object")
  }
  if ( !"ws_monitor" %in% class(ws_monitor2) ) {
    logger.error("Required argument 'ws_monitor2' is not a ws_monitor object")
    stop("Required argument 'ws_monitor2' is not a ws_monitor object")
  }
  if ( is.null(monitorIDs) ) {
    logger.error("Required argument 'monitorIDs' is missing")
    stop("Required argument 'monitorIDs' is missing")
  }
  
  # Create an overall time axis  
  starttime <- min(ws_monitor1$data$datetime, ws_monitor2$data$datetime)
  endtime <- max(ws_monitor1$data$datetime, ws_monitor2$data$datetime)
  datetime <- seq(starttime, endtime, by="hours")
  hourlyDF <- data.frame(datetime=datetime)
  
  monList <- list()
  for ( monitorID in monitorIDs ) {
    
    # Get trimmed, single monitors
    mon1 <- monitor_subset(ws_monitor1, monitorIDs=monitorID) %>% monitor_trim()
    mon2 <- monitor_subset(ws_monitor2, monitorIDs=monitorID) %>% monitor_trim()
    # Put data from each monitor on the shared time axis
    data1 <- dplyr::full_join(hourlyDF, mon1$data, by="datetime")
    data2 <- dplyr::full_join(hourlyDF, mon2$data, by="datetime")
    # Find out where each has data and where both have data
    mask1 <- !is.na(data1[[monitorID]])
    mask2 <- !is.na(data2[[monitorID]])
    mask3 <- mask1 & mask2
    
    # Join the data
    joinedData <- data1
    joinedData[[monitorID]][mask2] <- data2[[monitorID]][mask2]
    if ( sum(mask3) > 0 ) {
      logger.warn("Averaging distinct data values for %d shared timesteps for monitorID '%s'", sum(mask3), monitorID)
      indices <- which(mask3)
      joinedData[[monitorID]][indices] <- mean(c(data1[[monitorID]][indices],data2[[monitorID]][indices]))
    }

    # Create the 'ws_monitor' object
    mon <- list(meta=mon1$meta, data=joinedData)
    monList[[monitorID]] <- structure(mon, class = c("ws_monitor", "list"))
    
  }  
  
  ws_monitor <- monitor_combine(monList)
  
  return(ws_monitor)
  
}
