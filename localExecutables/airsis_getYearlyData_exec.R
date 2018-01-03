#!/usr/bin/env Rscript

library(methods)
library(optparse)
suppressPackageStartupMessages(library(PWFSLSmoke))
suppressPackageStartupMessages(library(MazamaSpatialUtils))


option_list <- list(
  make_option(c("--startdate"),default='20160101', help="starting date of the data to be downloaded"),
  make_option(c("--enddate"), default='20170101', help="ending date of the data to be downloaded"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated RData files [default=\"%default\"]"),
  make_option(c("--fileName"), default=NULL, help="name for the RData file"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Sanity checks
if ( is.null(opt$fileName) ) stop(paste0("'filePath' must be specified"))
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

# Get all unitids 
usfs_unitids <- airsis_availableUnits(opt$startdate, opt$enddate, provider = 'USFS')
apcd_unitids <- airsis_availableUnits(opt$startdate, opt$enddate, provider = 'APCD')

# for usfs and apcd, make a list of ws_monitor objects for all units
monitors <- list()
for ( unitid in usfs_unitids ) {
  result <- try (monitor <- airsis_createMonitorObject(startdate = opt$startdate, enddate = opt$enddate, provider = 'USFS', unitID = unitid))
  if ("try-error" %in% class(result)) {
    msg <- geterrmessage()
    print(msg)
  } else {
  monitors[[paste0(unitid, "_usfs")]] <- monitor
  }
}
for ( unitid in apcd_unitids ) {
  result <- try(
    monitor <- airsis_createMonitorObject(startdate = opt$startdate, enddate = opt$enddate, provider = 'APCD', unitID = unitid)
  )
  if ("try-error" %in% class(result)) {
    msg <- geterrmessage()
    print(msg)
  } else {
    monitors[[paste0(unitid, "a_pcd")]] <- monitor
  }
}
all_monitors <- monitor_combine(monitors)

if opt$
save(all_monitors, file = opt$filePath)
