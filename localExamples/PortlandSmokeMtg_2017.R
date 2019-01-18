
library(PWFSLSmoke)

# Washington, Oregon August 22, 2015 ------------------------------------------

pnw <- monitor_loadAnnual(2015) %>%
  monitor_subset(stateCodes=c('or','wa'), tlim=c(20150731,20150901))

# Timeseries overview
monitor_timeseriesPlot(pnw, style='gnats')

# Oklahoma recent -------------------------------------------------------------

monitor_loadLatest() %>%
  monitor_subset(stateCodes='OK') %>%
  monitor_map()


