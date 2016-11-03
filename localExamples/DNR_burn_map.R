DNR_burn_map <- function( startdate = 20160915, timeRange = 2 ) {
  ## TO DO: add logger for checking data existence
  
  # load data from DNR_downloadData
  load("localData/DNR_monitorList.RData")
  
  # load data from DNR_SmokeManagementApprovals
  df <- readr::read_csv("SmokeManagementApprovals.csv")
  
  #url <- "http://smoke.airfire.org/bluesky-daily/output/standard/PNW-1.33km/2016091600/
  #forecast/data/fire_locations.csv"
  
  # load satelite data and subset it for WA state
  events <- readr::read_csv("fire_locations.csv") #there's only 9/15-9/18
  events <- events[which(events$state == 'WA'),]
  
  startDate <- strptime(as.character(startdate),'%Y%m%d', tz='UTC') + 7*60*60
  endDate <- startDate + 48*60*60
  
  library(magrittr)
  subDF <- df[intersect(which(df$datetime >= startDate), 
                        which(df$datetime <= endDate)),]
  
  # further clean the data by removing NA values
  subDF <- subDF[!is.na(subDF$Latitude),]
  
  # get satelite data within the time window
  events$datetime <- stringr::str_split_fixed(events$date_time, '0000', 2)[,1] %>% 
    strptime('%Y%m%d', tz='UTC') + 7*60*60
  subEvents <- events[intersect(which(events$datetime >= startDate),
                                which(events$datetime <= endDate)),]
  
  # create a dataframe to store information extracted from monitorList
  nMonitor <- length(monitorList)
  monitorDF <- data.frame(matrix(nrow = nMonitor, ncol = 3))
  names(monitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(monitorDF) <- names(monitorList)
   
  for (i in 1:nMonitor) {
    monitorDF$longitude[i] <- monitorList[[i]]$meta$longitude
    monitorDF$latitude[i] <- monitorList[[i]]$meta$latitude
    indexes <- intersect(which(monitorList[[i]]$data$datetime >= startDate),
                         which(monitorList[[i]]$data$datetime <= endDate))
    monitorDF$MaxPm25[i] <- max(na.omit(monitorList[[i]]$data[,2][indexes]))
  }
  
  library(maps)
  # reset margin to see a  clearer plot
  par(mar = c(0,0,0,0))
  map('county', 'wa')
  
  points(subDF$Longitude, subDF$Latitude, pch=2, col='red', cex=2)
  text(monitorDF$longitude, monitorDF$latitude, labels=as.character(monitorDF$MaxPm25), col='darkgreen')
  points(subEvents$longitude, subEvents$latitude, col='blue', pch=1, cex=2)
  }