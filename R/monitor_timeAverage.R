#' @keywords nowcast
#' @export
#' @title Calculate Time Averages
#' @param ws_monitor ws_monitor object
#' @param ... additional arguments to be passed to openair::timeAverage()
#' @description This function extracts the \code{data} dataframe from the \code{ws_monitor}
#' and renames the 'datetime' column so that it can be processed by the \code{openair} package's
#' \code{timeAverage()} function. (See that function for details.)
#' @return ws_monitor object with data that have been proccessed by \code{openair::timeAverage()}
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20160801,20160831)
#' CarmelValley <- monitor_subset(airnow, monitorIDs="060530002",
#'                                tlim=c(2016080800,2016081023),
#'                                timezone='America/Los_Angeles')
#' CarmelValley_3hrChunks <- monitor_timeAverage(CarmelValley, avg.time="3 hour")
#' layout(matrix(seq(2)))
#' monitorPlot_hourlyBarplot(CarmelValley, main='1-Hourly Average PM2.5')
#' monitorPlot_hourlyBarplot(CarmelValley_3hrChunks, main='3-Hourly Average PM2.5')
#' layout(1)
#' }


monitor_timeAverage <- function(ws_monitor, ...) {
  
  # Extract and prepare data
  meta <- ws_monitor$meta
  data <- ws_monitor$data
  names(data)[1] <- 'date'
  
  # Apply openair::timeAverage()
  argsList <- list(...)
  argsList$mydata <- data
  averagedData <- as.data.frame( do.call(openair::timeAverage, argsList) )
  
  # Rebuild ws_monitor object
  names(averagedData)[1] <- 'datetime'
  ws_monitor <- list(meta=meta, data=averagedData)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  return(ws_monitor)
}

