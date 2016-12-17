#!/opt/local/bin/Rscript

# December, 2016 update:
#
# This script uses regularly updated data found at:  /home/monitors
#
# AIRSIS raw dump files are transferred from DRI to PWFSL on an hourly basis and
# processed by this script into a single file:
#
# * airsis_pm25_latest.RData     -- 'ws_monitor' object that combines 'data' and 'meta' dataframes
#
# This script will be run hourly on vacuum in the 'bluesky' crontab as part of the 'Monitoring' block of instructions:
#
# # Update AIRSIS monitoring data
# 21 *  *   *   *    /home/bluesky/monitoring/bin/airsis_createLatestDataframes_exec.R --inputDir=/home/web_data/monitoring --outputDir=/home/web_data/monitoring --logDir=/home/web_logs/monitoring

VERSION <- "0.2.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages( library(PWFSLSmoke) )
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

################################################################################

createAIRSISDataframes <- function(opt) {
  
  # MazamaSpatialUtils resources for metadata creation
  setSpatialDataDir('~/Data/Spatial')
  loadSpatialData('NaturalEarthAdm1')
  
  # Loop through all dumpFiles
  monitorList <- list()
  for (file in opt$dumpFiles) {
    filepath <- file.path(opt$inputDir,file)
    logger.info('Ingesting %s', filepath)
    result <- try( ws_monitor <- airsisDump_createMonitorObject(filepath),
                   silent=TRUE)
    if ( class(result)[1] == "try-error") {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg, "No valid PM2.5 data") ) {
        logger.debug('No data -- skipping %s', filepath)
      } else {
        logger.error('Unable to parse %s', filepath)
      }
    } else {
      monitorList[[basename(filepath)]] <- ws_monitor 
    }
  }
  
  # Combine individual ws_monitor objects into one big one
  logger.info('Creating combined ws_monitor object...')
  airsisNew <- monitor_combine(monitorList)
  
  # Extract 'data' and 'meta' dataframes
  newData <- airsisNew$data
  newMeta <- airsisNew$meta
  
  # Add has_PM2.5 to match existing meta
  newMeta$has_PM2.5 <- TRUE
  
  # Create empty data dataframe with appropriate time axis
  # Allow for manual override for testing
  if ( is.null(opt$todayGMT) ) {
    todayGMT <- as.POSIXct( lubridate::today(), tz="GMT")
  } else {
    todayGMT <- lubridate::ymd_hms(opt$todayGMT, tz="GMT")
  }
  
  timeAxis <- seq(todayGMT - lubridate::ddays(9), todayGMT + lubridate::ddays(1), by='hours')
  data <- data.frame(datetime=timeAxis)
  
  # Read in existing AIRSIS dataframes meta, data
  result <- try( oldData <- get(load(file.path(opt$outputDir,'airsis_pm25_latestData.RData'))),
                 silent=TRUE)
  
  if ( class(result)[1] == "try-error") {
    logger.debug('Cannot load %s -- just using new data', file.path(opt$outputDir,'airsis_pm25_latestData.RData'))
  } else {
    # Join old data to new axis
    data <- dplyr::left_join(data, oldData, by='datetime')
  }
  
  # Join old data to new axis, join new data to new axis
  data <- dplyr::left_join(data, newData, by='datetime')
  
  # Subset to include only columns with valid data
  allMissingMask <- apply(data, 2, function(x) { all(is.na(x)) })
  data <- data[,!allMissingMask]
  
  result <- try( oldMeta <- get(load(file.path(opt$outputDir,'airsis_pm25_latestMeta.RData'))),
                 silent=TRUE)
  
  if ( class(result)[1] == "try-error") {
    logger.debug('Cannot load %s -- just using new meta', file.path(opt$outputDir,'airsis_pm25_latestMeta.RData'))
    meta <- newMeta
  } else {
    # Fix up older versions that are missing the 'monitorType' column
    # TODO:  Remove this fix after a couple of weeks of operation
    if ( !'monitorType' %in% names(oldMeta) ) {
      # Add 'monitortype' and 'has_pm2.5' columns in the proper order
      oldMeta <- oldMeta[1:20]
      oldMeta$monitorType <- 'EBAM'
      oldMeta$has_PM2.5 <- TRUE
    }
    # Join all meta
    meta <- dplyr::bind_rows(oldMeta, newMeta)
  }
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  # Subset to only include sites with valid data
  meta <- meta[colnames(data)[-1],]
  
  # Save the 'data' and 'meta' dataframes separately
  PM2.5 <- data
  save(PM2.5, file=file.path(opt$outputDir,'airsis_pm25_latestData.RData'))
  AIRSIS_SitesMetadata <- meta
  save(AIRSIS_SitesMetadata, file=file.path(opt$outputDir,'airsis_pm25_latestMeta.RData'))
  
  # Create and save the 'ws_monitor' object
  AIRSIS_Latest <- list(meta=as.data.frame(meta), data=as.data.frame(data))
  AIRSIS_Latest <- structure(AIRSIS_Latest, class = c("ws_monitor", "list"))
  save(AIRSIS_Latest, file=file.path(opt$outputDir,'airsis_pm25_latest.RData'))
  
}

################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up OptionParser
option_list <- list(
  make_option(c("--inputDir"), default=getwd(), help="Input directory containing .RData files [default\"%default\"]"),
  make_option(c("--outputDir"), default=getwd(), help="Output directory for generated .csv files [default\"%default\"]"),
  make_option(c("--logDir"), default=getwd(), help="Output directory for generated .log file [default\"%default\"]"),
  make_option(c("--spatialDataDir"), default='~/Data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('airsis_createLatestDataframes_exec.R ',VERSION,'\n'))
  quit()
}

# For interactive debugging:
if (FALSE) {
  
  VERSION = "0.2.0"
  opt <- list(inputDir="/Users/jonathancallahan/Data/monitors",
              outputDir="/Users/jonathancallahan/Data/monitors",
              logDir="/Users/jonathancallahan/Data/Logs",
              spatialDataDir='/Users/jonathancallahan/Data/Spatial',
              todayGMT="2016-09-27 22:00:00")
  
}

# Amend opt
opt$dumpFiles <- c('ARB2.csv',        # EBAM
                   'GBUAPCD.csv',     # EBAM
                   'ODEQ.csv',        # EBAM
                   'TCAPCD_ebam.csv', # EBAM
                   'USFS_ebam.csv',   # EBAM
                   'WASHOE.csv',      # EBAM
                   'mrpsa_ebam.csv',  # EBAM
                   'APCD_esam.csv',   # E-Sampler
                   'USFS_esam.csv')   # E-Sampler

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('airsis_createLatestDataframes_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('airsis_createLatestDataframes_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('airsis_createLatestDataframes_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

logger.info('Running airsis_createLatestDataframes_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ----- Create AIRSIS dataframes ----------------------------------------------

result <- try( createAIRSISDataframes(opt) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error AIRSIS dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}

