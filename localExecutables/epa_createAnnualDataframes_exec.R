#!/usr/local/bin/Rscript

# This Rscript will go to fetch air-quality data for a specified year and specified parameter

# This script is desgined to be run on demand as a cron job or 'at' job, see the example below

# 1 2 3 4 5 /Users/jonathan/Projects/PWFSL/PWFSLSmoke/localExecutables/epa_createAnnualDataframes_exec.R --parameterName=PM2.5 --parameterCode=88101 --year=2010 --downloadDir=/Users/jonathan/Data/EPA/raw --outputDir=/Users/jonathan/Data/EPA/RData --logDir=/Users/jonathan/Data/EPA/RData

VERSION = "1.0.1"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(PWFSLSmoke)
})

# Set up OptionParser
option_list <- list(
  make_option(c("-n","--parameterName"),default='PM2.5', help="parameter name"),
  make_option(c("-c","--parameterCode"), default='88101', help="a character string of parameter code"),
  make_option(c("-y","--year"), default=2016, help="Specify a single year to download data for"),
  make_option(c("-d","--downloadDir"), default=getwd(), help="Output directory for downloaded EPA .zip files [default=\"%default\"]"),
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("-s","--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# For debugging
if ( FALSE ) {
  
  opt <- list(parameterName = "PM2.5",
              parameterCode = "88101",
              year = "2010",
              downloadDir = "/Users/jonathan/Data/EPA/raw",
              outputDir = "/Users/jonathan/Data/EPA/RData",
              logDir = "/Users/jonathan/Data/EPA/RData",
              spatialDataDir = "~/Data/Spatial")  
  
}

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createGeoJSON_exec.R ',VERSION,'\n'))
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
debugLog <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$year,'_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$year,'_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$year,'_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir)
loadSpatialData("NaturalEarthAdm1")

logger.info('Running epa_createAnnualDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)


# ------Downloading and processing data --------------------------

filename <- paste0('hourly','_',opt$parameterCode,'_',opt$year,'.zip')
zipFile <- file.path(opt$downloadDir, filename)
if ( !file.exists(zipFile) ) {
  logger.info("Downloading EPA data...")
  zipFile <- epa_downloadData(opt$year, opt$parameterCode,
                              downloadDir=opt$downloadDir)
}

ws_monitor <- epa_createMonitorObject(zipFile, zeroMinimum=TRUE, addGoogleMeta=FALSE)

# Create appropriate data object and file name and write the data to disk
basename <- paste('epa', opt$parameterName, opt$parameterCode, opt$year, sep='_') 
assign(basename, ws_monitor)
filename <- paste0(basename, '.RData')
filepath <- file.path(opt$outputDir, filename)
save(list=basename, file=filepath)

logger.info('Completed successfully!')
