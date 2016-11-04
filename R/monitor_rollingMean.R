#' @keywords monitor
#' @export
#' @title Compute Rolling Means of Monitor Data
#' @param ws_monitor ws_monitor object
#' @param width width of window (in hours) used to calculate means
#' @param data.thresh minimum number of observations as a percent of width required, or NA is returned
#' @param align alignment of averaging window relative to point being calculated ["left"|"center"|"right"]
#' @description Calculate rolling means for each monitor in the \code{ws_monitor} object using the
#' \code{openair::rollingMean()} function
#' @return \code{ws_monitor} object with rolling mean data 
#' @examples 
#' \dontrun{
#' airnow <- airnow_load(20150801, 20150808)
#' WA_smoky <- monitor_subset(airnow, stateCodes='WA', vlim=c(100,Inf))
#' WA_smoky_3hr <- monitor_rollingMean(WA_smoky, width=3, align="center")
#' WA_smoky_24hr <- monitor_rollingMean(WA_smoky, width=24, align="right")
#' monitor_timeseriesPlot(WA_smoky, type='l', shadedNight=TRUE)
#' monitor_timeseriesPlot(WA_smoky_3hr, type='l', col='red', add=TRUE)
#' monitor_timeseriesPlot(WA_smoky_24hr, type='l', col='blue', lwd=2, add=TRUE)
#' legend('topright', c("hourly","3-hourly","24-hourly"),
#'        col=c('black','red','blue'), lwd=c(1,1,2))
#' title('Smoky Monitors in Washington -- August, 2015')
#' }
 
monitor_rollingMean <- function(ws_monitor, width=8, data.thresh=75, align="center") {
  
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  names(data)[1] <- "date"
  monitorIDs <- names(data)[-1]
  rollingData <- data.frame(datetime = data$date)
  
  for (id in monitorIDs) {
    rawData <- data[,c("date", id)]
    smoothData <- openair::rollingMean(rawData, pollutant=id, width=width, data.thresh=data.thresh, align=align, new.name=id)
    rollingData[[id]] <- smoothData[[id]]  
  }

  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=rollingData)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return (ws_monitor)
}
