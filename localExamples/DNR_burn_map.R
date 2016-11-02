DNR_burn_map <- function( startdate = startdate ) {
  ## TO DO: add logger for checking data existence
  
  # load data from DNR_downloadData
  load("localData/DNR_monitorList.RData")
  
  # load data from DNR_SmokeManagementApprovals
  df <- readr::read_csv("SmokeManagementApprovals.csv")
  
  #url <- "http://smoke.airfire.org/bluesky-daily/output/standard/PNW-1.33km/2016091600/
  #forecast/data/fire_locations.csv"
  
  # load satelite data and subset to for WA state
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
  timeRange <- 2
  
  #df$numDate <- strftime(df$datetime, "%Y%m%d") %>% as.numeric()
  #subDF <- df[intersect(which(df$numDate >= startdate), 
  #                      which(df$numDate <= startdate + timeRange)),]
  
  events$numDate <- stringr::str_split_fixed(events$date_time, '0000', 2)[1] %>% as.numeric()
  subEvents <- events[intersect(which(events$numDate >= startdate))]
}