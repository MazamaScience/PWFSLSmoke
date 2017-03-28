#' @keywords ws_monitor
#' @export
#' @title Calculate Time Averages
#' @param ws_monitor emph{ws_monitor} object
#' @param ... additional arguments to be passed to \code{openair::timeAverage()}
#' @return A emph{ws_monitor} object with data that have been proccessed by \code{openair::timeAverage()}.
#' @description This function extracts the \code{data} dataframe from \code{ws_monitor} object
#' and renames the \code{'datetime'} column so that it can be processed by the \pkg{openair} package's
#' \code{timeAverage()} function. (See that function for details.)
#' @examples
#' C_V <- monitor_subset(CarmelValley, tlim=c(2016080800,2016081023),
#'                       timezone='America/Los_Angeles')
#' C_V_3hourly <- monitor_timeAverage(C_V, avg.time="3 hour")
#' head(C_V$data, n=15)
#' head(C_V_3hourly$data, n=5)


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
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}

