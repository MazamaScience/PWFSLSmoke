#!/usr/bin/env Rscript

# Updated Jan 3, 2018
# 
# This script will download data for all airsis monitors for the specified time frame
# 
# # Example
# airsis_getYearlyData_exec.R --startdate=20150101 --enddate=2016010223 --outputDir="~/Data/airsis" --fileName="airsis_2015"

VERSION <- "0.0.1"

suppressPackageStartupMessages({
    library(methods)
    library(optparse)
    library(PWFSLSmoke)
    library(MazamaSpatialUtils)
})

##########################################

saveAirsisData <- function(opt) {
  
  # Make a list of ws_monitor objects for all units
  monitors <- list()
  
  for ( provider in c("USFS", "APCD", "ARB2", "EPA")) {
    
    unitids <- airsis_availableUnits(opt$startdate, opt$enddate, provider = provider)
    logger.debug("----- retrieving monitro data from ", provider, " -----")
    for ( unitid in unitids ) {
      logger.debug(paste0("----- trying ", provider, " ", unitid, " -----"))
      result <- try (monitor <- airsis_createMonitorObject(startdate = opt$startdate, enddate = opt$enddate, provider = provider, unitID = unitid))
      if ("try-error" %in% class(result)) {
        print(paste0("error loading ", provider, " ", unitid, ": ", geterrmessage()))
      } else {
        monitors[[paste0(unitid, "_", provider)]] <- monitor
        logger.debug("successfully loaded monitor data")
      }
    }
    
  }
  
  all_monitors <- monitor_combine(monitors)
  
  # Set the name of all_monitors to fileName
  if (is.null(opt$fileName)) {
    fileName <- paste0('airsis_', opt$startdate, "_", opt$enddate)
  } else {
    fileName <- opt$fileName
  }
  
  assign(fileName, all_monitors)
  filePath <- paste0(opt$outputDir, '/', fileName, '.RData')
  
  # Save the RData file
  save(list = fileName, file = filePath)
  
}

################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up option parser
option_list <- list(
  make_option(c("--startdate"),default=NULL, help="starting date of the data to be downloaded"),
  make_option(c("--enddate"), default=NULL, help="ending date of the data to be downloaded"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated RData files [default=\"%default\"]"),
  make_option(c("--fileName"), default=NULL, help="name for the RData file"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createCSV_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( is.null(opt$startdate) ) stop(paste0("startdate is required"))
if ( is.null(opt$enddate) ) stop(paste0("enddate is required"))
if ( !file.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !file.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('airsis_getYearlyData_', '_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('airsis_getYearlyData_', '_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('airsis_getYearlyData_', '_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir) ##FOR bash

loadSpatialData("NaturalEarthAdm1")


# ----- Save airnow ws_monitor object as a RData file ------

result <- try( saveAirsisData(opt) )

if ( "try-error" %in% class(result) ) {
  msg <- paste("Error saving airsis data: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}




