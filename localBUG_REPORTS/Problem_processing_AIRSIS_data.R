# On Aug 08, everything fell apart for AIRSIS data processing

library(PWFSLSmoke)

# Set up MazamaSpatialUtils
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")

logger.setup()
logger.setLevel(TRACE)

# Configurable number of days in "latest"
dayCount <- 10
hourCount <- dayCount * 24

now <- lubridate::now(tzone='UTC') %>% lubridate::floor_date(unit='hour')
starttime <- now - lubridate::dhours(hourCount)
startdate <- strftime(starttime , "%Y%m%d%H", tz='UTC')
enddate <- strftime(now, "%Y%m%d%H", tz='UTC')

provider <- 'usfs'
unitID <- '1013'


monitor <- airsis_createMonitorObject(startdate=startdate, enddate=enddate, provider=provider, unitID=unitID)

