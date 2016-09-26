###############################################################################
# Create merged dataframe from AIRSIS dump file
#
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(DEBUG)

USFS_ebam_file <- '~/Data/monitors/USFS_esam.csv'

logger.info('Reading data...')
fileString <- readr::read_file(USFS_ebam_file)

# Special parsing for dump files as the format is different from the AIRSIS CSV webservice
logger.info('Parsing data...')
df <- airsisDump_parseData(fileString)

# At this point df has multiple monitors in it.

# Standard quality control still works
logger.info('Applying QC logic...')
df <- airsis_qualityControl(df)

# Change the name here so that we can continue to use 'df' in the loop to match code in airsis_createMonitorObject.R.
dfCombined <- df

# Loop through df$Alias and run each chunk through further data processing
metaList <- list()
dataList <- list()
for (singleAlias in unique(dfCombined$Alias)) {
  # NOTE:  You cannot assign the alias name to "Alias" in each loop as the dplyr expression
  # NOTE:  "Alias == Alias" will always evaluate to TRUE.
  logger.info('Processing data for %s...', singleAlias)
  df <- dplyr::filter(dfCombined, Alias == singleAlias)
  
  # Add clustering information to identify unique deployments
  logger.info('Clustering...')
  df <- addClustering(df, lonVar='Longitude', latVar='Latitude', clusterDiameter=1000)
  
  # Create 'meta' dataframe of site properties organized as monitorID-by-property
  # NOTE:  This step will create a uniformly named set of properties and will
  # NOTE:  add site-specific information like timezone, elevation, address, etc.
  logger.info('Creating \'meta\' dataframe...')
  meta <- airsis_createMetaDataframe(df)
  
  # Create 'data' dataframe of PM2.5 values organized as hour-by-monitorID
  logger.info('Creating \'data\' dataframe...')
  data <- airsis_createDataDataframe(df, meta)
  
  metaList[[singleAlias]] <- meta
  dataList[[singleAlias]] <- data
}

# NOTE:  Could have done this inside the loop but leaving metaList and dataList in tact
# NOTE:  for debugging purposes.

# Create combined 'meta'
meta <- dplyr::bind_rows(metaList)

# Create combined 'data'
singleAlias <- names(dataList[1])
data <- dataList[[singleAlias]]
for (singleAlias in names(dataList)[-1]) {
  data <- dplyr::full_join(data,dataList[[singleAlias]],by="datetime")
}

# Create the 'ws_monitor' object
ws_monitor <- list(meta=as.data.frame(meta), data=as.data.frame(data))
ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))


