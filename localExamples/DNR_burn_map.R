DNR_burn_map <- function( startdate = startdate, timeRange = 2 ) {
  ## TO DO: add logger for checking data existence
  
  # load data from DNR_downloadData
  load("localData/DNR_monitorList.RData")
  
  # load data from DNR_SmokeManagementApprovals
  df <- readr::read_csv("SmokeManagementApprovals.csv")
  
  #url <- "http://smoke.airfire.org/bluesky-daily/output/standard/PNW-1.33km/2016091600/
  #forecast/data/fire_locations.csv"
  
  # load satelite data and subset it for WA state
  events <- readr::read_csv("fire_locations.csv")
  events <- events[which(events$state == 'WA'),]
  
  library(maps)
  # reset margin to see a  clearer plot
  par(mar = c(0,0,0,0))
  map('county', 'wa')
  
  library(magrittr)
  
  points(df$Longitude, df$Latitude, pch = 2)
  points(monitorList$FishHatchery$meta$longitude, monitorList$FishHatchery$meta$latitude, 
         cex = 3, col='red')
  points(events$longitude, events$latitude, col = 'green', pch = 2, cex = 3)
  
  # to look at the next 48 hours, might need to adjust
  # timeRange <- 2
  
  df$numDate <- strftime(df$datetime, "%Y%m%d") %>% as.numeric()
  subDF <- df[intersect(which(df$numDate >= startdate), 
                        which(df$numDate <= startdate + timeRange)),]
  
  # further clean the data by removing NA values
  subDF <- subDF[!is.na(subDF$Latitude),]
  
  # get satelite data within the time window
  events$numDate <- stringr::str_split_fixed(events$date_time, '0000', 2)[,1] %>% as.numeric()
  subEvents <- events[intersect(which(events$numDate >= startdate),
                                which(events$numDate <= startdate + timeRange)),]
  
  # create a dataframe to store information extracted from monitorList
  nMonitor <- length(monitorList)
  monitorDF <- data.frame(matrix(nrow = nMonitor, ncol = 3))
  names(monitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(monitorDF) <- names(monitorList)
   
  ## TODO: change startdate to posixct earlier so don't need to make numDate above
  startDate <- strptime(as.character(startdate),'%Y%m%d', tz='UTC') + 7*60*60
  endDate <- startDate + 48*60*60
  for (i in 1:nMonitor) {
    monitorDF$longitude[i] <- monitorList[[i]]$meta$longitude
    monitorDF$latitude[i] <- monitorList[[i]]$meta$latitude
    indexes <- intersect(which(monitorList[[i]]$data$datetime >= startDate),
                         which(monitorList[[i]]$data$datetime <= endDate))
    monitorDF$MaxPm25[i] <- max(monitorList[[i]]$data[,2][indexes])
  }
  
  ## TODO: NA occurred from this loop -- to be solved
  
  
}