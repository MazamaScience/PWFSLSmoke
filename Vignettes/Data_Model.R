## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ------------------------------------------------------------------------
library(PWFSLSmoke)

# Set the location of the local data archive
setSmokeDataDir('~/Data/Smoke')

# Get some airnow data for Washington
airnow <- airnow_load(startdate=20150801, enddate=20150831)
WA <- monitor_subset(airnow, stateCodes='WA')

# 'ws_monitor' objects can be identified by their class
class(WA)

# Examine the 'meta' dataframe
dim(WA$meta)
rownames(WA$meta)
colnames(WA$meta)

# Examine the 'data' dataframe
dim(WA$data)
colnames(WA$data)

# This should always be true
all(rownames(WA$meta) == colnames(WA$data[,-1]))

## ------------------------------------------------------------------------
# Use special knowledge of AirNow IDs to subset airnow data for Spokane county monitors
SpokaneCountyIDs <- airnow$meta$monitorID[stringr::str_detect(airnow$meta$monitorID, "^53063")]
Spokane <- monitor_subset(airnow, monitorIDs=SpokaneCountyIDs)

# Apply 3-hr rolling mean
Spokane_3hr <- monitor_rollingMean(Spokane, 3, align="center")

# Replace data columns with their squared values (exponentiation is not supplied by the package)
Spokane_3hr$data[,-1] <- (Spokane_3hr$data[,-1])^2

# NOTE:  Exponentiation is only used as an example. It does not generate a meaningful result.

# Create a daily averaged 'ws_monitor' object
Spokane_daily_3hr <- monitor_dailyStatistic(Spokane_3hr)

# Check out the correlation between monitors (correlation is not supplied by the package)
data <- Spokane_daily_3hr$data[,-1] # omit the 'datetime' column
cor(data, use='complete.obs')

