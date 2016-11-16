library(PWFSLSmoke)
library(openair)
setwd("~/Projects/PWFSLSmoke/localData")

load("DNR_monitorList.RData")
load("DNR_rawList.RData")

combinedData <- monitor_combine(monitorList)

# Map the stations in our database, using the meta data in the ws_monitor object (created using monitor_combine above)
monitor_map(combinedData)

timeRanges <- data.frame()

for (i in names(monitorList)) {
  timeRanges[i,1] <- as.POSIXct(min(monitorList[[i]]$data$datetime))
  timeRanges[i,2] <- as.POSIXct(max(monitorList[[i]]$data$datetime))
}

plot(rawList$Plain$COncRT, col = "blue")
points(rawList$Plain$ConcHr)

plot(rawList$Plain[7:12])
names(rawList$Plain)

# CONVERT TO POSIXct
ts <- lubridate::mdy_hms(df$TimeStamp)