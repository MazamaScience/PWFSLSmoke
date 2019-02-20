
library(PWFSLSmoke)

# Get # hours/day above a threshold
wa <- monitor_loadAnnual(2018) %>%
  monitor_subset(stateCodes = 'WA') %>%
  monitor_dailyThreshold(threshold = "unhealthy")

# Annual summary at each monitor (omitting 'datetime' column)
annualHours <- colSums(wa$data[,-1])

annual <- wa
annual$data <- wa$data[1,]
annual$data[1,-1] <- annualHours

monitor_map(annual, slice = 1)

# Fourth of July plot ----------------------------------------------------------

par(mfrow = c(4,5))
par(mar = c(0,0,2,0))

for ( year in 2000:2018 ) {

  # NOTE:  We're using daystart = "sunrise" so we need to extend the data at least
  # NOTE:  part way intoJuly 5th.
  july4 <- lubridate::ymd_h(paste0(year,"-07-04 01"), tz="America/Los_Angeles")
  july6 <- lubridate::ymd_h(paste0(year,"-07-06 01"), tz="America/Los_Angeles")

  monitor_loadAnnual(year) %>%
    monitor_subset(stateCodes = 'WA', tlim = c(july4,july6)) %>%
    monitor_dailyStatistic(FUN = max, dayStart = "sunrise") %>%
    monitor_map(slice = 1, cex=2)

  title(paste0('Fourth of July in ', year))

}



