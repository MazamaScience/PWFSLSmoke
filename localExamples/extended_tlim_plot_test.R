# 2019-01-29 test of the behavior of base plot functions with ws_monitor objects
# representing 5-months, 5-weeks and 5-days of dat.
#
# RESULT:  With some fussing, these functions can make pretty plots over a wide
# RESULT:  of time periods. No new functionality is needed.

library(PWFSLSmoke)

or <- monitor_loadAnnual(2018) %>%
  monitor_subset(stateCodes = 'OR') %>%
  monitor_subset(tlim=c("2018-06-01","2018-10-31"))

Medford_m <- monitor_subset(or, monitorID = "410292129_01")
Medford_w <- monitor_subset(Medford_m, tlim=c(20180901,20181015))
Medford_d <- monitor_subset(Medford_m, tlim=c(20180906,20180911))


# ----- monitor_timeseries() ---------------------------------------------------

# 5 months
monitor_timeseriesPlot(Medford_m, style='gnats')
addAQIStackedBar()
addAQILines()

# 5 weeks
monitor_timeseriesPlot(Medford_w, pch = 1, shadedNight = TRUE)
addAQIStackedBar()
addAQILines()

# 5 days
monitor_timeseriesPlot(Medford_d, type='b', pch=16, shadedNight = TRUE)
addAQIStackedBar()
addAQILines()


# ----- monitor_dailyBarplot() -------------------------------------------------

# 5 months
monitor_dailyBarplot(Medford_m, gridPos = "under", border = NA,
                     labels_x_nudge = 2.0, labels_y_nudge = 3)

# 5 weeks
monitor_dailyBarplot(Medford_w,
                     labels_x_nudge = 0.5, labels_y_nudge = 1)
addAQIStackedBar()

# 5 days
monitor_dailyBarplot(Medford_d,
                     labels_x_nudge = 0.0, labels_y_nudge = 0)
addAQIStackedBar()
addAQIStackedBar(pos="right")


# ----- monitor_hourlyBarplot() ------------------------------------------------

# 5 months
monitor_hourlyBarplot(Medford_m, border = NA, dayLwd = 0, hourLwd = 0, shadedNight=FALSE)
# TODO:  No time axis

# 5 weeks
monitor_hourlyBarplot(Medford_w, border = NA, dayLwd = 1, hourLwd = 0)

# 5 days
monitor_hourlyBarplot(Medford_d)


################################################################################
# Deprecated below here
################################################################################

# ----- monitor_rollingMeanPlot() ----------------------------------------------

# 5 months
monitor_rollingMeanPlot(Medford_m)

# 5 weeks
monitor_rollingMeanPlot(Medford_w)

# 5 days
monitor_rollingMeanPlot(Medford_d)


