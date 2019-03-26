
# ----- Sacramento smoke -------------------------------------------------------

library(PWFSLSmoke)

camp_fire <-
  monitor_loadAnnual(2018) %>%
  monitor_subset(stateCodes = 'CA') %>%
  monitor_subset(tlim = c(20181108,20181123))

monitor_leaflet(camp_fire)

Sacramento <-
  camp_fire %>%
  monitor_subset(monitorIDs = '060670010_01')

monitor_timeseriesPlot(
  Sacramento,
  style='aqidots',
  pch=16,
  xlab="2017"
)
addAQIStackedBar()
addAQILines()
addAQILegend()
title("Sacramento Smoke")

Sacramento_area <-
  camp_fire %>%
  monitor_subsetByDistance(
    longitude = Sacramento$meta$longitude,
    latitude = Sacramento$meta$latitude,
    radius = 50
  )

monitor_leaflet(Sacramento_area)

Sacramento_area %>%
  monitor_collapse() %>%
  monitor_dailyStatistic() %>%
  monitor_extractData()


# ----- Kennewick School -------------------------------------------------------

library(PWFSLSmoke)
library(PWFSLSmokePlots)

# Get the appropriate data
wa_2017 <-
  monitor_loadAnnual(2017) %>%
  monitor_subset(stateCodes = 'WA') %>%
  monitor_subset(tlim = c(20170901,20170914))

# Find the monitorID for Kennewick
monitor_leaflet(wa_2017)

# Create a Kennewick-only ws_monitor object
Kennewick <-
  wa_2017 %>%
  monitor_subset(monitorIDs = '530050002_01')

# Create a plot
ggplot_pm25Timeseries(Kennewick) +
  stat_dailyAQCategory(alpha = .5) +
  geom_pm25Points(shape = "square", alpha = 1.0) +
  custom_aqiStackedBar(width = 0.01) +
  ggtitle("Kennewick, September 2017")

