#!/usr/bin/env Rscript

# This script will combine airnow monthly data into a yearly data file and save it as an RData file
# 
# # Example
# airsis_getYearlyData_exec.R --startdate=20150101 --enddate=2016010223 --outputDir="~/Data/airsis" --fileName="airsis_2015"

VERSION <- "0.0.1"

suppressPackageStartupMessages({
  library(methods)
  library(optparse)
  library(PWFSLSmoke)
})

##########################################

saveAirnowYearlyData <- function(opt) {
  
  airnowYear <- load("path to January file goes here")
  
  for (month in 2:12){
    # Retrieve the correct ws_monitor object
    airnowMonth <- load("Path to correct file goes here")
    
    # add it to the yearly ws_monitor object
    monitor_combine(airnowYear, airnowMonth)
  }
  
  # Set the name of airnowYear to fileName
  if (is.null(opt$fileName)) {
    fileName <- paste0('airnow_', opt$year)
  } else {
    fileName <- opt$fileName
  }
  
  assign(fileName, airnowYear)
  filePath <- paste0(opt$outputDir, '/', fileName, '.RData')
  
  # Save the RData file
  save(list = fileName, file = filePath)
  
}

################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up option parser
option_list <- list(
  make_option(c("-n","--parameterName"),default='PM2.5', help="parameter name"),
  make_option(c("-y","--year"), default=2016, help="Specify a single year to download data for"),
  make_option(c("-d","--downloadDir"), default=getwd(), help="Output directory for downloaded EPA .zip files [default=\"%default\"]"),
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-f","--fileName"), default="airnow_[year]", help="Name for the RData file"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("-s","--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createCSV_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !dir.exists(opt$downloadDir) ) stop(paste0("downloadDir not found:  ",opt$downloadDir))
if ( !dir.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !dir.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Add year subdirectories
opt$downloadDir <- file.path(opt$downloadDir,opt$year)
opt$outputDir <- file.path(opt$outputDir,opt$year)
opt$logDir <- file.path(opt$logDir,opt$year)

# Make sure the year subdirectories exist
dir.create(opt$downloadDir, showWarnings=FALSE)
dir.create(opt$outputDir, showWarnings=FALSE)
dir.create(opt$logDir, showWarnings=FALSE)

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




