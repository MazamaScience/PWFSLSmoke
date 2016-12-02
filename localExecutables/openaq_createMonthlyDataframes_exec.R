#!/opt/local/bin/Rscript

# November, 2016 update:
#
# This script will download one month of hourly data from openAQ website and generate a
# suite of "data" dataframes appropriate for use in "ws_monitor" objects defined by the
# PWFSLSmoke package. One dataframe will be created for each parameter availabe in the
# downloaded data.
#
# This script can be onece a month in a run in a cron job as in the example below:
# 
# # m h  dom mon dow   command
# 
# # Update openAQ monitoring data
# 00 01  12   *   *   /Users/aliceyang/Projects/PWFSLSmoke/localExecutables/openaq_createMonthlyDataframes_exec.R --startdate=20161001 --days=1 --outputDir=Users/alieyang/Data/openAQ --logDir=Users/aliceyang/Data/Logs --meta
VERSION = "0.1.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
###suppressPackageStartupMessages( library(PWFSLSmoke) )
suppressPackageStartupMessages(library(PWFSLSmoke))
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

################################################################################

createDataDataframes <- function(df, startdate, outputDir) {
  
  # Download, separate and reshape data for all parameters
  dataDataframe <- suppressMessages(openaq_createDataDataframe(df))
  
  filename <- paste0("openAQ_PM2.5_",startdate,".RData")
  filepath <- file.path(outputDir,filename)
  
  logger.debug('Writing %s...', filepath)
  
  result <- try( save(dataDataframe, file=filepath),
                 silent=TRUE )
  
  if ( class(result)[1] == "try-error" ) {
    msg <- paste("Error writing openAQ 'data' dataframes: ", geterrmessage())
    logger.error(msg)
  }
  
  logger.info("Finished writing openAQ 'data' dataframes for %s", startdate)
}

################################################################################

createMetaDataframes <- function(df, startdate, outputDir) {
  
  # Download, separate and reshape data for all parameters
  metaDataframe <- suppressMessages(openaq_createMetaDataframe(df))
  
  filename <- paste("openAQ_PM2.5_Sites_",startdate,"_Metadata.RData")
  filepath <- file.path(outputDir,filename)
  
  logger.debug('Writing %s...', filepath)
  
  result <- try( save(metaDataframe, file=filepath),
                 silent=TRUE )
  
  if ( class(result)[1] == "try-error" ) {
    msg <- paste("Error writing openAQ 'meta' dataframes: ", geterrmessage())
    logger.error(msg)
  }
  
  logger.info("Finished writing openAQ'meta' dataframes")
}


################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up OptionParser
option_list <- list(
  make_option(c("--startdate"),default='20160101', help="starting date of the data to be downloaded"),
  make_option(c("--days"), default='5', help="number of days of data to be sucked in"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated .csv files [default=\"%default\"]"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("--meta"), action="store_true", default=FALSE, help="Also create metadata dataframes [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createGeoJSON_exec.R ',VERSION,'\n'))
  quit()
}

# For interactive debugging:
# if (FALSE) {
#   
#   VERSION = "0.1.0"
#   opt <- list(startdate='20161101',
#               days='10',
#               outputDir='~/Data/openAQ',
#               logDir='~/Data/Logs',
#               spatialDataDir='~/Data/Spatial')
#   
# }

# Sanity checks
if ( !file.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !file.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('openaq_createMonthlyDataframes_',opt$startdate,'_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('openaq_createMonthlyDataframes_',opt$startdate,'_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('openaq_createMonthlyDataframes_',opt$startdate,'_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir) ##FOR bash

loadSpatialData("NaturalEarthAdm1")

logger.info('Running openaq_createMonthlyDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ------- downloading and processing openAQ data -----------------------------------

logger.info('Downloading data...')
df <- openaq_downloadData(startdate=opt$startdate, days=as.numeric(opt$days) )

# add additional columns to the dataframe
logger.info('Adding \'datetime\', \'stateCode\', \'monitorID\' columns...')

# add datetime and monitorID column
df$datetime <- lubridate::ymd_hms(df$local)

# extract unique combinations of latitudes and longitudes for faster buffering process
uniqueLatLon <- unique(paste(df$latitude, df$longitude))
uniqueLatLon <- stringr::str_split_fixed(uniqueLatLon, ' ', 2)
colnames(uniqueLatLon) <- c("latitude", "longitude")
uniqueLatLon <- apply(uniqueLatLon,2,as.numeric)
stateCodes <- suppressMessages(getStateCode(uniqueLatLon[,"longitude"], uniqueLatLon[,"latitude"], useBuffering = TRUE) )

# # correct non-US state codes  
# stateCodes[which(stateCodes == '')] <- 'PR'
# stateCodes[which(stateCodes == 'TM' | stateCodes == 'CH')] <- 'TX'
# stateCodes[which(stateCodes == 'BC')] <- 'ID'

# assign state codes accordingly
df$stateCode <- NA
for (i in 1:nrow(df)) {
  latIndex <- which(uniqueLatLon[, 1] == df$latitude[i])
  lonIndex <- which(uniqueLatLon[, 2] == df$longitude[i])
  df$stateCode[i] <- stateCodes[intersect(latIndex,lonIndex)]
}

# create a monitorID column as unique identifier 
df$monitorID <- with(df,paste(location,city,stateCode,sep=', '))

# ----- Always create openAQ "meta" dataframes ----------------------------

if ( opt$meta ) {
  
  result <- try( createMetaDataframes(df, opt$startdate, opt$outputDir) )
  
  if ( class(result)[1] == "try-error" ) {
    msg <- paste("Error creating openAQ 'meta' dataframes: ", geterrmessage())
    logger.fatal(msg)
  }
  
}


# ----- Always create openAQ "data" dataframes --------------------------------

result <- try( createDataDataframes(df, opt$startdate, opt$outputDir) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating openAQ 'data' dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}



