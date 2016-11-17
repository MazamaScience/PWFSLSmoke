#!/usr/bin/env Rscript

# November, 2016 update:
#
# This script will download the most recent 1 day of hourly data from openAQ and merge it with 
# pre-downloaded LATEST 31 days of data by chopping off the oldest day and adding the newly downloaded 
# 1 day of data. This csript generates a suite of "data" dataframes appropriate for use in 
# "ws_monitor" objects defined by the PWFSLSmoke package. One dataframe will be created 
# for each parameter availabe in the downloaded data.
#
# This script is intended to be run hourly in a run in a cron job as in the example below:
# 
# # m h  dom mon dow   command
# 
# # Update AirNow monitoring data
#  15 *    *   *   *  /home/bluesky/monitoring/bin/airnow_createLatestDataframes_exec.R --user=USER --pass=PASS --outputDir=/home/web_data/monitoring --logDir=/home/web_logs/monitoring

VERSION = "0.1.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages( library(PWFSLSmoke) )
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

################################################################################

createLatestDataDataframes <- function(df,outputDir) {
  
  # Download, separate and reshape the latest data for all parameters
  currentDF <- openaq_createDataDataframe(df)
  
  # Assign dataframes to their parameter name and merge them with any previous version
  # Get the latest dataframe for this parameter
  filename <- paste0("openAQ_PM2.5_LATEST_31days.RData")
  filepath <- file.path(outputDir,filename)
  
  if ( file.exists(filepath) ) {
    
    # ----- Merge latest data with previous data ----------------------------
    
    logger.debug('Merging latest %s data with previous data...', parameter)
    
    # NOTE:  Sadly, none of the dplyr::~_join() functions allow you to join-with-replacement
    # NOTE:  when values are found in the same cell of both x and y. This is precisely what
    # NOTE:  we need to do here so that newer data can replace older data. We'll have to
    # NOTE:  create our own merge-with-overwrite logic.
    
    previousDF <- get(load(filepath))
    
    #########
    #### TODO: update the day before yesterday's data #####
    #########
    
    # Get rid of the oldest day in previous DF
    previousDF <- previousDF[-c(1:24),]
    
    timeDF[,1] <- seq(as.POSIXct(as.character(Sys.Date()-31)),as.POSIXct(as.character(Sys.Date()))+23*60*60,by='hours')
    

    # Create a uniform timeAxis (45 days ago until tomorrow)
    startdate <- as.POSIXct(lubridate::today() - lubridate::ddays(45))
    enddate <- as.POSIXct(lubridate::today() + lubridate::ddays(1))
    timeAxis <- seq(startdate, enddate, by='hours')
    timeDF <- data.frame(datetime=lubridate::with_tz(timeAxis, 'UTC'))
    
    # Put latestDF and previousDF on new, uniform time axis
    previousDF <- dplyr::left_join(timeDF, previousDF, by='datetime')
    latestDF <- dplyr::left_join(timeDF, latestDF, by='datetime')
    
    # NOTE:  Now everything has the same time axis but previousDF and latestDF
    # NOTE:  might have different monitors. We separate out three groups:
    # NOTE:    1) monitors in previousDF but not latesteDF can be retained
    # NOTE:    2) monitors in latesetDF but not in previous can be added
    # NOTE:    3) monitors in both need to use all data from previousDF but overwrite
    # NOTE:       with data from latesetDF where it is not NA
    # NOTE:  Finally, we put all three groups back together
    
    previousOnlyMonitors <- dplyr::setdiff(names(previousDF), names(latestDF))
    latestOnlyMonitors <- dplyr::setdiff(names(latestDF), names(previousDF))
    sharedMonitors <- dplyr::intersect(names(previousDF), names(latestDF))[-1] # Omit 'datetime'
    
    if ( length(previousOnlyMonitors) > 0 ) {
      logger.debug('Old monitors not found in latest data: %s', paste0(previousOnlyMonitors, collapse=", "))
    }
    if ( length(latestOnlyMonitors) > 0 ) {
      logger.debug('New monitors not found in previous data: %s', paste0(latestOnlyMonitors, collapse=", "))
    }
    
    previousOnlyDF <- previousDF[,previousOnlyMonitors]
    latestOnlyDF <- latestDF[,latestOnlyMonitors]
    sharedPreviousDF <- previousDF[,sharedMonitors]
    sharedLatestDF <- latestDF[,sharedMonitors]
    
    # Replace values in sharedPreviousDF with new data from sharedLatestDF
    newDataMask <- !is.na(sharedLatestDF)
    sharedPreviousDF[newDataMask] <- sharedLatestDF[newDataMask]
    
    # Bind everything back together into a new dataframe which replaces latesteDF
    latestDF <- dplyr::bind_cols(timeDF, previousOnlyDF, sharedPreviousDF, latestOnlyDF)
    
    # ----- Save dataframe ----------------------------------------------------
    
    logger.debug('Writing %s...', filepath)
    
    # Assign the dataframe associated with "parameter" to an environment variable named after that parameter
    dfName <- paste0(parameter)
    assign(dfName, latestDF)
    
    # NOTE:  Now the environment variable "parameter" is a character string, e.g. "PM2.5"
    # NOTE:  while the einvironment variable "PM2.5" is a dataframe.
    result <- try( save(list=dfName, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing AirNow 'data' dataframes: ", geterrmessage())
      logger.error(msg)
    }
    
    
  } # END of paremter in dfList loop
  
  logger.info("Finished writing AirNow 'data' dataframes for %s", opt$yearMonth)
  
}

################################################################################

createMetaDataframes <- function(opt) {
  
  # Download, separate and reshape data for all parameters
  dfList <- airnow_createMetaDataframe(user=opt$user, pass=opt$pass)
  
  # Assign dataframes to their parameter name and save them
  for (parameter in names(dfList)) {
    
    # Assign the dataframe associated with "parameter" to an environment variable named after that parameter
    dfName <- paste0(parameter,"_sites")
    assign(dfName, dfList[[parameter]])
    
    # NOTE:  Now the environment variable "parameter" is a character string, e.g. "PM2.5"
    # NOTE:  while the einvironment variable "PM2.5_sites" is a dataframe.
    
    filename <- paste0("AirNowTech_",parameter,"_SitesMetadata.RData")
    filepath <- file.path(opt$outputDir,filename)
    
    logger.debug('Writing %s...', filepath)
    
    result <- try( save(list=dfName, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing AirNow 'meta' dataframes: ", geterrmessage())
      logger.error(msg)
    }
    
  }
  
  logger.info("Finished writing AirNow 'meta' dataframes")
  
}


################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up OptionParser
option_list <- list(
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
if (FALSE) {
  
  VERSION = "0.1.0"
  opt <- list(
    outputDir=paste0(getwd())
    logDir=paste0(getwd()),
    spatialDataDir='~/Data/Spatial')
  
}

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('openaq_createLATESTDataframes_LATEST_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('openaq_createLATESTDataframes_LATEST_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('openaq_createLATESTDataframes_LATEST_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir)
loadSpatialData("NaturalEarthAdm1")

logger.info('Running openaq_createLatestDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ------- downloading and processing openAQ data -----------------------------------

logger.info('Downloading data...')
df <- openaq_downloadData(startdate=format(Sys.Date(),"%Y%m%d"), days=1)

# add additional columns to the dataframe
logger.info('Adding \'datetime\', \'stateCode\', \'monitorID\' columns...')

# add datetime and monitorID column
df$datetime <- lubridate::ymd_hms(df$local)

# extract unique combinations of latitudes and longitudes for faster buffering process
uniqueLatLon <- unique(paste(df$latitude, df$longitude))
uniqueLatLon <- stringr::str_split_fixed(uniqueLatLon, ' ', 2)
colnames(uniqueLatLon) <- c("latitude", "longitude")
uniqueLatLon <- apply(uniqueLatLon,2,as.numeric)
stateCodes <- getStateCode(uniqueLatLon[,"longitude"], uniqueLatLon[,"latitude"], useBuffering = TRUE) 

# correct non-US state codes  
stateCodes[which(stateCodes == '')] <- 'PR'
stateCodes[which(stateCodes == 'TM' | stateCodes == 'CH')] <- 'TX'
stateCodes[which(stateCodes == 'BC')] <- 'ID'

# assign state codes accordingly
df$stateCode <- NA
for (i in 1:nrow(df)) {
  latIndex <- which(uniqueLatLon[, 1] == df$latitude[i])
  lonIndex <- which(uniqueLatLon[, 2] == df$longitude[i])
  df$stateCode[i] <- stateCodes[intersect(latIndex,lonIndex)]
}

# create a monitorID column as unique identifier 
df$monitorID <- with(df,paste(location,city,stateCode,sep=', '))

# ----- Optionally create openAQ "meta" dataframes --------------------------------

result <- try( createMetaDataframes(df) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating openAQ 'meta' dataframes: ", geterrmessage())
  logger.fatal(msg)
}


# ----- Always create openAQ "data" dataframes --------------------------------

result <- try( createLatestDataDataframes(df, opt$outputDir) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating openAQ 'data' dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}



