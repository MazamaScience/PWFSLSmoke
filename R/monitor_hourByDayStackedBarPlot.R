#' @export
#' @title Hour by Day Bar Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @description A bar plot.
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150810)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_hourByDayStackedBarPlot(ws_monitor, monitor)
#' }

monitor_hourByDayStackedBarPlot <- function(ws_monitor, monitorID, stacks = 4) {
  # Latest aqiBreaks from http://www.arb.ca.gov/carpa/toolkit/data-to-mes/wildfire-smoke-guide.pdf
  # NOTE:  The low end of each break category is used as the breakpoint.
  # Latest aqiColors from http://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/
  aqiBreaks_24  <- c(0, 12, 35.5, 55.5, 150.5, 250.5, 10000)
  
  aqiColors <- c("#009966","#FFDE33","#FF9933","#CC0033","#660099","#730023")
  aqiColors <- adjustcolor(aqiColors, 0.5)
  aqiNames <- c('good','moderate','USG','unhealthy','very unhealthy','extreme')
  
  # Create a DF
  pm25 <- ws_monitor$data[[monitorID]]
  GMTTime <- ws_monitor$data$datetime 
  index <- which(ws_monitor$meta$monitorID %in% monitorID)
  
  # Create a local time vector and then subset it into the desired number of stacks.
  localTime <- lubridate::with_tz(GMTTime,ws_monitor$meta$timezone[index])
  localTime <- localTime[(lubridate::hour(localTime) %% (24 / stacks)) == 0]
  day <- lubridate::day(localTime)
  hour <- lubridate::hour(localTime)
  
  # Create averages for each PM 2.5 stack
  pm25 <- pm25[1:length(pm25) - length(pm25) %% stacks]
  pm25Matrix <- matrix(pm25, ncol = (24 / stacks))
  averagePM25 <- apply(pm25Matrix, 1, mean)
  
  # Create a new dataframe for local use
  df <- data.frame(localTime,averagePM25,day,hour)
  df$cols <- aqiColors[ .bincode(df$averagePM25, aqiBreaks_24, include.lowest=TRUE) ]
  
  dayRange <- seq(ws_monitor$data$datetime[1], tail(ws_monitor$data$datetime, 1), by=paste0(24 / stacks, ' hour'))
  dayRange <- dayRange[1:length(dayRange) - length(dayRange) %% 4]
  barPlotFrame <- matrix(dayRange, nrow = stacks, )
  barplot(barPlotFrame, space = 0, col = df$cols)
  
}