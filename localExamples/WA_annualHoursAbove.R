
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

monitorMap(annual, slice = 1)
