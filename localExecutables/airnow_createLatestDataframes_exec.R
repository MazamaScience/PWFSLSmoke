#!/usr/bin/env Rscript

# October, 2016 update:
#
# This script will download the most recent 10 days of hourly data from AirNow and generate a
# suite of "data" dataframes appropriate for use in "ws_monitor" objects defined by the
# PWFSLSmoke package. One dataframe will be created for each parameter availabe in the
# downloaded data.
# 
# These 10-day dataframes will be merged with LATEST dataframes containing 45 days
# of data.The LATEST dataframes will always contain the most recent 45 days worth of 
# data. Retaining 45 days of data in the LATEST dataframes gives AirNow enough time to
# complete updates to their archival data which can be processed with another script:
#
#   airnow_createMonthlyDataframes_exec.R
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

createDataDataframes <- function(opt) {
  
  # Download, separate and reshape the latest data for all parameters
  dfList <- airnow_createLatestDataDataframes(user=opt$user, pass=opt$pass)
  
  # Assign dataframes to their parameter name and merge them with the previous version
  for (parameter in names(dfList)) {
    
    filename <- paste0("AirNowTech_",parameter,"_LATEST.RData")
    filepath <- file.path(opt$outputDir,filename)
    
    # Get the latest dataframe for this parameter
    latestDF <- dfList[[parameter]]
    
    # Get the previous dataframe for this prameter from disk if it exists
    if ( file.exists(filepath) ) {
      previousDF <- get(load(filepath))
    }
    
    # NOTE:  Sadly, none of the dplyr::~_join() functions allow you to join-with-replacement
    # NOTE:  when values are found in both the same cell of x and y. This is precisely what
    # NOTE:  we need to do here so that newer data can replace older data. We'll have to
    # NOTE:  create our own merge-with-overwrite logic.
    
    # TODO:  merge-with-overwrite
    
    # create new timeAxis and emptyDF
    # join oldDF with emptyDF
    # join newDF with emptyDF
    # Now everything has the same time axis
    # Go through every column of newDF where !is.na and stick those variables in oldDF
    
    # Or, we could try to mask values in oldDF where values exist in newDF and then full_join would work
  }
  
  
  
  #############################################################################
  #############################################################################
  #############################################################################
  
  
  
  
  for (parameter in names(dfList)) {
    
    # NOTE:  Now the environment variable "parameter" is a character string, e.g. "PM2.5"
    # NOTE:  while the einvironment variable "PM2.5" is a dataframe.
    
    filename <- paste0("AirNowTech_",parameter,"_LATEST.RData")
    filepath <- file.path(opt$outputDir,filename)
    
    logger.debug('Writing %s...', filepath)
    
    result <- try( save(list=dfName, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing AirNow 'data' dataframes: ", geterrmessage())
      logger.error(msg)
    }
    
  }
  
  logger.info("Finished writing AirNow 'data' dataframes for %s", opt$yearMonth)
  
}

################################################################################

createMetaDataframes <- function(opt) {
  
  # Download, separate and reshape data for all parameters
  dfList <- airnow_createMetaDataframes(user=opt$user, pass=opt$pass)
  
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
  make_option(c("--user"), default='USER', help="User ID for AirNowTech"),
  make_option(c("--pass"), default='PASS', help="Password for AirNowTech"),
  make_option(c("--yearMonth"), default='201601', help="Year and month in YYYYMM format"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated .csv files [default=\"%default\"]"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("--spatialDataDir"), default="~/Data/Spatial", help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
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
  opt <- list(user='USER',
              pass='PASS',
              outputDir=paste0(getwd()),
              logDir=paste0(getwd()),
              spatialDataDir='~/Data/Spatial')
  
}

# Clean up arguments
if ( stringr::str_detect(opt$yearMonth, 'LAST_MONTH') ) {
  day <- lubridate::today()               # today
  lubridate::day(day) <- 1                # first day of this month
  day <- day - lubridate::ddays(1)        # last day of last month
  opt$yearMonth <- strftime(day, "%Y%m")  # just the YYYYMM part
}

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('airnow_createLATESTDataframes_',opt$yearMonth,'_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('airnow_createLATESTDataframes_',opt$yearMonth,'_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('airnow_createLATESTDataframes_',opt$yearMonth,'_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir)

logger.info('Running airnow_createLATESTDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)


# ----- Always create AirNow "meta" dataframes --------------------------------

result <- try( createMetaDataframes(opt) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating AirNow 'meta' dataframes: ", geterrmessage())
  logger.fatal(msg)
}


# ----- Always create AirNow "data" dataframes --------------------------------

result <- try( createDataDataframes(opt) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating AirNow 'data' dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}



