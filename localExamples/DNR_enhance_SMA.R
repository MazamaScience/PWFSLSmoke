#####################################################################
# After loading in the data from DNR_SmokeManagementApprovals.R and
# DNR_downlaodData.R, this script adds a column for the closest 
# monitor from the burn and a column for the distance between them.
#####################################################################

df <- readr::read_csv("SmokeManagementApprovals.csv")
load("localData/DNR_monitorList.RData")

df$Longitude <- as.numeric(df$Longitude)
df$Latitude <- as.numeric(df$Latitude)
df$closestMonitor <- df$closestMonitorDistance <- NA
n <- length(monitorList)
for(i in 1:nrow(df)){
  if (!is.na(df$Longitude[i])){
    dat <- data.frame(matrix(0, nrow = n, ncol = 2))
    for (j in 1:n) {
      dat[j,1] <- row.names(monitorList[[j]]$meta)
      dat[j,2] <- monitor_distance(monitorList[[j]], df$Longitude[i], df$Latitude[i])
    }
    df$closestMonitorDistance[i] <- min(dat[,2])
    df$closestMonitor[i] <- dat[,1][which(dat[,2] == min(dat[,2]))]
  }
}

# An alternative way
# pull out unique pairs of longitudes and latitudes
# then do a dplyr::left_join
