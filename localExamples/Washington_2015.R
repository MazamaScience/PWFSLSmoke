# Washington_2015
#
# Various functions for a Washington Summer of 2015 project
#

library(PWFSLSmoke)

logger.setup()
logger.setLevel(INFO)

library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

wrcc_getData <- function(startdate=20150601, enddate=20150930) {
  
  # Get all of the WRCC cache monitors
  # NOTE:  No cache monitors data is available for summer of 2015
  cacheList <- list()
  i <- 0
  for ( stationID in WRCC$unitIDs$cache ) {
    logger.info('Working on %s', stationID)
    i <- i + 1
    result <- try( mon <- wrcc_createMonitorObject(startdate, enddate, stationID),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      next
    } else {
      cacheList[[i]] <- mon
    }
  }
  cache <- monitor_combine(cacheList)
  
  # Get all of the WRCC miscellaneous monitors
  # NOTE:  No miscellaneous monitors data is available for summer of 2015
  miscellaneousList <- list()
  i <- 0
  for ( stationID in WRCC$unitIDs$miscellaneous ) {
    logger.info('Working on %s', stationID)
    i <- i + 1
    result <- try( mon <- wrcc_createMonitorObject(startdate, enddate, stationID),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      next
    } else {
      miscellaneousList[[i]] <- mon
    }
  }
  miscellaneous <- monitor_combine(miscellaneousList)
  
  # Get all of the WRCC USFS regional monitors
  # NOTE:  No usfs_regional monitors data is available for summer of 2015
  usfs_regionalList <- list()
  i <- 0
  for ( stationID in WRCC$unitIDs$usfs_regional ) {
    logger.info('Working on %s', stationID)
    i <- i + 1
    result <- try( mon <- wrcc_createMonitorObject(startdate, enddate, stationID),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      next
    } else {
      usfsRegionalList[[i]] <- mon
    }
  }
  usfs_regional <- monitor_combine(usfs_regionalList)
  
}
