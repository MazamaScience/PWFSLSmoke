# airsis_2017.R
#
# Script demonstrating how to assemble all of the AIRSIS monitor data for 2017
#

# Required setup
library(PWFSLSmoke)
logger.setup()
logger.setLevel(WARN)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up the temporal range
startdate <- 20170701
enddate <- 20170801

# Identify the "providers" of interest -- the 'usfs' part of usfs.airsis.com
providers <- c('usfs', 'apcd', 'arb2', 'epa')

# Create a list to hold ws_monitor objects associated with a single provider
providerList <- list()

# Loop over each provider
for ( provider in providers ) {

  # Get a list of available unitIDs
  unitIDs <- airsis_availableUnits(startdate, enddate, provider)
  
  if ( length(unitIDs) == 0 ) {
    
    logger.info('Skipping %s:  no available units', provider)
    
    # Nothing to add to providerList
    
  } else {
    
    logger.info('Processing data for %d unitIDs for %s', length(unitIDs), provider)
    
    # Create a list to hold ws_monitor objects associated with a single unitID
    unitList <- list()
    for ( i in 1:length(unitIDs) ) {
      
      unitID <- unitIDs[i]
      
      # Skip over errors
      result <- try({
        unitList[[unitID]] <- airsis_createMonitorObject(startdate, enddate, provider, unitID)
      }, silent=TRUE)
      
      if ( "try-error" %in% class(result) ) {
        err_msg <- geterrmessage()
        logger.error(err_msg)
      }
      
    }
    
    # Combine unit-level ws_monitor objects into a single provider-level ws_monitor object
    providerList[[provider]] <- monitor_combine(unitList)
    
  }
  
}

# Combine provider-level ws_monitor objects into a single 'airsis' ws_monitor object

airsis <- monitor_combine(providerList)



