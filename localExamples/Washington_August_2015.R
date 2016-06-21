# Washginton_August_2015.R
#
# This example explores montoring data available during the intense fires
# in north-central Washington State during August of 2015.

library(PWFSLSmoke)


# ---- AirNow monitors ----

# Load airnow monitor data for Washington for August, 2015
airnow <- airnow_load(startdate = 20150801, enddate = 20150831)
airnow_wa <- monitor_subset(airnow, stateCodes = 'WA')

# Interactive plot of maximum values with monitor_leaflet(),
monitor_leaflet(airnow_wa)

# Subset based on value >= AQI "very unhealthy"
# * create value limits ('vlim') extending from 'unhealthy' levels to Infinity
# * apply 3-hour rolling mean
# * subset the rolling mean data with 'unhealthy' levels
bad_limits <- c(AQI$breaks_24[which(AQI$names == 'unhealthy')], Inf)
airnow_wa_3hr <- monitor_rollingMean(airnow_wa, width = 3)
airnow_wa_unhealthy <- monitor_subset(airnow_wa_3hr, vlim = bad_limits)

# Plot a static map
map('state','WA')
monitor_map(airnow_wa_unhealthy, mean, cex=2, add=TRUE)
title('AirNow:  Monthly Mean of 3-Hourly PM 2.5 in August, 2015')


# ---- WRCC monitors ----

wrcc <- wrcc_load(startdate = 20150801, enddate = 20150831)
wrcc_wa <- monitor_subset(wrcc, stateCodes = 'WA')
wrcc_wa_3hr <- monitor_rollingMean(wrcc_wa, width = 3)
wrcc_wa_unhealthy <- monitor_subset(wrcc_wa_3hr, vlim = bad_limits)

# Plot a static map
map('state','WA')
monitor_map(wrcc_wa_unhealthy, mean, cex=2, add=TRUE)
title('WRCC:  Monthly Mean of 3-Hourly PM 2.5 in August, 2015')


# ---- AIRSIS monitors ----

### airsis <- airsis_load(startdate = 20150801, enddate = 20150831)
### airsis_wa <- monitor_subset(airsis, stateCodes = 'WA')

# NOTE: airsis for this time period does not have any monitors in Washington


# ---- merged monitors ----

# Merge all "unhealthy" subsets into a single ws_monitor object
merged_unhealthy <- monitor_combine(airnow_wa_unhealthy, wrcc_wa_unhealthy)

# Interactive plot of maximum values over terrain
monitor_leaflet(merged_unhealthy, providerTiles="Stamen.Terrain")

# Find the five monitors with the highest measured smoke levles
monitor_max <- apply(merged_unhealthy$data[,-1], 2, max, na.rm=TRUE)
top_five_IDs = names( sort(monitor_max, decreasing=TRUE)[1:5] )
merged_worst <- monitor_subset(merged_unhealthy, monitorIDs=top_five_IDs)

# Plot with monitor_map
monitor_leaflet(merged_worst, providerTiles="Stamen.Terrain")

# Interactive timeseries plot
monitor_dygraph(merged_worst, title='Smokiest Washginton Monitors in August, 2015')

# Plot with monitor_timeseriesPlot
monitor_timeseriesPlot(merged_worst, AQIStyle=TRUE)
legend('topleft', legend=rev(AQI$names), pch=16, col=rev(AQI$colors))
title('Terrible Smoke in late August')

# Create a new ws_monitor object by appling 24hr right-aligned rolling mean
merged_worst_24hr <- monitor_rollingMean(merged_worst, width=24, align="right")

# Plot map and timeseries
monitor_dygraph(merged_worst_24hr, title='24 Hour Average PM2.5')

# In the second half of August, how many hours per day was the air unhealthy?
late_august <- monitor_subset(merged_worst, tlim=c(20150816,20150831))
daily_worst <- monitor_dailyThreshold(late_august, threshold="unhealthy")
print(daily_worst$data)

