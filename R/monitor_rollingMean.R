#' @keywords ws_monitor
#' @export
#' @title Calculate Rolling Means of ws_monitor PM2.5 Data
#' @param ws_monitor ws_monitor object
#' @param width number of periods to average (e.g. for hourly data, \code{width = 24} calculates 24-hour rolling means)
#' @param data.thresh minimum number of valid observations required as a percent of \code{width}; NA is returned if insufficicnet valid data to calculate mean
#' @param align alignment of averaging window relative to point being calculated; one of \code{"left|center|right"}
#' @return A ws_monitor object with data thaty have been processed by a rolling mean algorithm.
#' @description Calculates rolling means for each monitor in the ws_monitor object using the
#' \code{openair::rollingMean()} function
#' @details \itemize{
#' \item{\code{align = 'left'}: Forward roll, using hour of interest and the (\code{width}-1) subsequent hours 
#' (e.g. 3-hr left-aligned roll for Hr 5 will consist of average of Hrs 5, 6 and 7)}
#' \item{\code{align = 'right'}: Backwards roll, using hour of interest and the (\code{width}-1) prior hours
#' (e.g. 3-hr right-aligned roll for Hr 5 will consist of average of Hrs 3, 4 and 5)}
#' \item{\code{align = 'center'} for odd \code{width}: Average of hour of interest and (\code{width}-1)/2 on either side
#' (e.g. 3-hr center-aligned roll for Hr 5 will consist of average of Hrs 4, 5 and 6)}
#' \item{\code{align = 'center'} for even \code{width}: Average of hour of interest and (\code{width}/2)-1 hours prior and 
#' \code{width}/2 hours after (e.g. 4-hr center-aligned roll for Hr 5 will consist of average of Hrs 4, 5, 6 and 7)}
#' }
#' @examples 
#' N_M <- Northwest_Megafires
#' wa_smoky <- monitor_subset(N_M, stateCodes='WA', tlim=c(20150801, 20150808), vlim=c(100,Inf))
#' wa_smoky_3hr <- monitor_rollingMean(wa_smoky, width=3, align="center")
#' wa_smoky_24hr <- monitor_rollingMean(wa_smoky, width=24, align="right")
#' monitorPlot_timeseries(wa_smoky, type='l', shadedNight=TRUE)
#' monitorPlot_timeseries(wa_smoky_3hr, type='l', col='red', add=TRUE)
#' monitorPlot_timeseries(wa_smoky_24hr, type='l', col='blue', lwd=2, add=TRUE)
#' legend('topright', c("hourly","3-hourly","24-hourly"),
#'        col=c('black','red','blue'), lwd=c(1,1,2))
#' title('Smoky Monitors in Washington -- August, 2015')

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
