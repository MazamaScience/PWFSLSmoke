# #' @keywords ws_monitor
# #' @export
# #' @import graphics
# #' @title Create Hour by Day Bar Plot
# #' @param ws_monitor ws_monitor object
# #' @param monitorID id for a specific monitor in the given ws_monitor
# #' @param stacks number of stacks in a 24 hour period
# #' @description A bar plot.
# #' @examples
# #' \dontrun{
# #' ws_monitor <- wrcc_load(20150801, 20150810)
# #' monitor <- ws_monitor$meta$monitorID[1]
# #' monitor_hourByDayStackedBarPlot(ws_monitor, monitor)
# #' }
# 
# # TODO:  Remove stacks?
# # TODO:  Use internal AQI object
# 
# monitor_hourByDayStackedBarPlot <- function(ws_monitor, monitorID=NULL, stacks=4) {
# 
#   # Allow single monitor objects to be used without specifying monitorID
#   if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
#     monitorID <- ws_monitor$meta$monitorID[1]
#   }
#   
#   aqiColors <- adjustcolor(AQI$colors, 0.5)
# 
#   # Create a DF
#   pm25 <- ws_monitor$data[[monitorID]]
#   UTCTime <- ws_monitor$data$datetime 
#   index <- which(ws_monitor$meta$monitorID %in% monitorID)
#   
#   # Create a local time vector and then subset it into the desired number of stacks.
#   localTime <- lubridate::with_tz(UTCTime,ws_monitor$meta$timezone[index])
#   localTime <- localTime[(lubridate::hour(localTime) %% (24 / stacks)) == 0]
#   day <- lubridate::day(localTime)
#   hour <- lubridate::hour(localTime)
#   
#   # Create averages for each PM 2.5 stack
#   pm25 <- pm25[1:length(pm25) - length(pm25) %% stacks]
#   pm25Matrix <- matrix(pm25, ncol = (24 / stacks))
#   averagePM25 <- apply(pm25Matrix, 1, mean)
#   
#   # Create a new dataframe for local use
#   df <- data.frame(localTime,averagePM25,day,hour)
#   df$cols <- aqiColors[ .bincode(df$averagePM25, AQI$breaks_24, include.lowest=TRUE) ]
#   
#   dayRange <- seq(ws_monitor$data$datetime[1], utils::tail(ws_monitor$data$datetime, 1), by=paste0(24 / stacks, ' hour'))
#   dayRange <- dayRange[1:length(dayRange) - length(dayRange) %% 4]
#   barPlotFrame <- matrix(dayRange, nrow = stacks, )
#   barplot(barPlotFrame, space = 0, col = df$cols)
#   
# }
