
library(PWFSLSmoke)

# Washington, Oregon August 22, 2015 -------------------------------------------

pnw <- monitor_loadAnnual(2015) %>%
  monitor_subset(stateCodes=c('or','wa'), tlim=c(20150731,20150901))

# Timeseries overview
monitor_timeseriesPlot(pnw, style='gnats')

# Oklahoma recent --------------------------------------------------------------

monitor_loadLatest() %>%
  monitor_subset(stateCodes='OK') %>%
  monitor_map()

# Portland annual averages -----------------------------------------------------

P_list <- list()
P_avg_list <- list()

for ( year in 2008:2018 ) {

  print(paste0("working on ", year))
  name <- paste0("P_",year)
  result <- try({
    P_list[[name]]  <- monitor_loadAnnual(year) %>%
      monitor_subset(stateCodes = c('or')) %>%
      monitor_subsetByDistance(-123.0,
                               45.6,
                               radius = 50) %>%
      monitor_collapse(monitorID = name)

    P_avg_list[[name]] <- mean(P_list[[name]]$data[,-1], na.rm = TRUE)
  })

}

> unlist(P_avg_list)
#   P_2008   P_2009   P_2010   P_2011   P_2012   P_2013   P_2015   P_2016   P_2017   P_2018
# 7.576901 7.400177 5.635646 7.022231 6.258708 7.590440 6.448365 4.916264 7.150733 7.060266

layout(matrix(seq(6)))

for ( year in 2008:2013 ) {
  name <- paste0("P_",year)
  monitorPlot_dailyBarplot(P_list[[name]],
                           main = paste0(year," Daily Averages"),
                           labels_y_nudge = 10,
                           ylim = c(0,35))
}
