#!/opt/local/bin/Rscript

# November 2016 Note:
# This Rscript will go to fetch air-quality data for a specified year and specified parameter

# This script is desgined to be run on demand as a cron job or 'at' job, see the example below

# 1 2 3 4 5 /Users/aliceyang/Projects/PWFSLSmoke/localExecutables/epa_createAnnualDataframes_exec.R --parameterName='PM2.5' --parameterCode='88101' --year=2008 --outputDir=/Users/aliceyang/Data/Smoke --logDir=/Users/aliceyang/Data/Logs 

VERSION = "0.1.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
###suppressPackageStartupMessages( library(PWFSLSmoke) )
library(PWFSLSmoke)
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

# Set up OptionParser
option_list <- list(
  make_option(c("--parameterName"),default='PM2.5', help="parameter name"),
  make_option(c("--parameterCode"), default='88101', help="a character string of parameter code"),
  make_option(c("--year"), default=2016, help="Specify a single year to download data for"),
  make_option(c("--baseUrl"), default='https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/', help="url link where EPA data exists"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated .csv files [default=\"%default\"]"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("--smokeDataDir"), default='~/Data/Smoke', help="Directory to save EPA zip and csv files temoporarily"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createGeoJSON_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !file.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !file.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$startdate,'_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$startdate,'_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('epa_createAnnualDataframes_',opt$startdate,'_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
setSpatialDataDir(opt$spatialDataDir) ##FOR bash
setSmokeDataDir(opt$smokeDataDir)
loadSpatialData("NaturalEarthAdm1")

logger.info('Running epa_createAnnualDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)


# ------Downloading and processing data --------------------------

df <- epa_downloadData(opt$parameterName, opt$parameterCode, opt$year, opt$baseUrl)

dataSource <- 'EPA'

# Create a column with the unique monitorID using the method described above
df$monitorID <- paste(df$`State Code`,df$`County Code`,df$`Site Num`,df$`Parameter Code`,df$POC,sep='_')

# Create a column with the datetime
df$datetime <- lubridate::ymd_hms(paste0(df$`Date GMT`,' ',df$`Time GMT`,':00'))

# Create 'meta' dataframe
meta <- epa_createMetaDataframe(df)

#Create 'data' dataframe
data <- epa_createDataDataframe(df)

# Create the 'ws_monitor' data list
ws_monitor <- list(meta=meta,
                   data=data)

ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

# Create appropriate data object and file name and write the data to disk
dfName <- paste(dataSource, opt$parameterName, opt$parameterCode, opt$year,sep='_')  
assign(dfName, ws_monitor)
fileName <- paste0(opt$outputDir, '/', dfName, '.RData')
save(list=dfName, file=fileName)

logger.debug(paste0('   Finished creating ws_monitor object\n'))

