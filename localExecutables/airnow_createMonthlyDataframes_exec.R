#!/usr/bin/env Rscript

# October, 2016 update:
#
# This script will download one month of hourly data from AirNow archives and generate a
# suite of "data" dataframes appropriate for use in "ws_monitor" objects defined by the
# PWFSLSmoke package. One dataframe will be created for each parameter availabe in the
# downloaded data.
#
# This script can be onece a month in a run in a cron job as in the example below:
# 
# # m h  dom mon dow   command
# 
# # Update AirNow monitoring data
# 00 01  12   *   *    /home/bluesky/monitoring/bin/airnow_createMonthlyDataframes_exec.R --user=USER --pass=PASS --yearMonth=LAST_MONTH --outputDir=/home/web_data/monitoring --logDir=/home/web_logs/monitoring

VERSION = "0.1.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages( library(PWFSLSmoke) )
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

createMonthlyDataframes <- function(opt) {
  
  # TODO:  SpatialDataDir should be configurable.
  # MazamaSpatialUtils resources for metadata creation
  setSpatialDataDir('~/Data/Spatial')
  loadSpatialData('NaturalEarthAdm1')
  
  # Download, separate and reshape data for all parameters
  dfList <- airnow_createDataDataframes(user=opt$user, pass=opt$pass, yearMonth=opt$yearMonth)
  
  # Assign dataframes to their parameter name and save them
  for (parameter in names(dfList)) {
    
    # Assign the dataframe associated with "parameter" to an environment variable named after that parameter
    assign(parameter, dfList[[parameter]])
    
    # NOTE:  Now the environment variable "parameter" is a character string, e.g. "PM2.5"
    # NOTE:  while the einvironment variable "PM2.5" is a dataframe.
    
    filename <- paste0("AirNowTech_",parameter,"_",opt$yearMonth,".RData")
    filepath <- file.path(opt$outputDir,filename)
    
    logger.debug('Writing %s...', filepath)
    
    result <- try( save(list=parameter, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing AirNow dataframes: ", geterrmessage())
      logger.error(msg)
    }
    
  }
  
  logger.info('Finished writing AirNow dataframes for %s', opt$yearMonth)
  
  # TODO:  Should include optional creation of "meta" dataframe using latest sites file.

}

################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up OptionParser
option_list <- list(
  make_option(c("--user"), default='USER', help="User ID for AirNowTech"),
  make_option(c("--pass"), default='PASS', help="Password for AirNowTech"),
  make_option(c("--yearMonth"), default='201601', help="Year and month in YYYYMM format"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated .csv files [default\"%default\"]"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default\"%default\"]")
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
              yearMonth='201601',
              outputDir=paste0(getwd()),
              logDir=paste0(getwd()))
  
}

# Clean up arguments
if ( stringr::str_detect(opt$yearMonth, 'LAST_MONTH') ) {
  day <- lubridate::today()               # today
  lubridate::day(day) <- 1                # first day of this month
  day <- day - lubridate::ddays(1)        # last day of last month
  opt$yearMonth <- strftime(day, "%Y%m")  # just the YYYYMM part
}

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('airnow_createMonthlyDataframes_',opt$yearMonth,'_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('airnow_createMonthlyDataframes_',opt$yearMonth,'_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('airnow_createMonthlyDataframes_',opt$yearMonth,'_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

logger.info('Running airnow_createMonthlyDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ----- Create AirNow dataframes ----------------------------------------------

result <- try( createMonthlyDataframes(opt) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating AirNow dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}



