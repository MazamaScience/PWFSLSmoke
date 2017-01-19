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
# 00 01  12   *   * /Users/aliceyang/Projects/PWFSLSmoke/localExecutables/openaq_createMonthlyDataframes_exec.R --startdate=20161001 --days=1 --outputDir=/Users/alieyang/Data/openAQ --logDir=/Users/aliceyang/Data/Logs --spatialDataDir=/Users/aliceyang/Data/Spatial
VERSION = "0.1.0"

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages(library(PWFSLSmoke))
suppressPackageStartupMessages( library(MazamaSpatialUtils) )

################################################################################

createDataDataframes <- function(df, startdate, outputDir) {
  
  # Download, separate and reshape data for all parameters
  dfList <- suppressMessages(openaq_createDataDataframes(df))

  for (parameter in names(dfList)) {
    
    # Assign the dataframe associated with "parameter" to an environment variable named after that parameter
    dfName <- paste0(parameter)
    assign(dfName, dfList[[parameter]])
    
    filename <- paste0("openAQ_", parameter, "_", startdate, "_Datadata.RData")
    filepath <- file.path(outputDir,filename)
    
    logger.debug('Writing %s...', filepath)
    
    result <- try( save(list=dfName, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing openAQ 'data' dataframes: ", geterrmessage())
      logger.error(msg)
    }
  }
  logger.info("Finished writing openAQ 'data' dataframes for %s", startdate)
  
}

################################################################################

createMetaDataframes <- function(df, startdate, outputDir) {
  
  # Download, separate and reshape data for all parameters
  dfList <- suppressMessages(openaq_createMetaDataframes(df))
  
  for (parameter in names(dfList)) {
    
    # Assign the dataframe associated with "parameter" to an environment variable named after that parameter
    dfName <- paste0(parameter)
    assign(dfName, dfList[[parameter]])
    
    filename <- paste0("openAQ_", parameter, "_", startdate, "_Metadata.RData")
    filepath <- file.path(outputDir,filename)
    
    logger.debug('Writing %s...', filepath)
    
    result <- try( save(list=dfName, file=filepath),
                   silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      msg <- paste("Error writing openAQ 'meta' dataframes: ", geterrmessage())
      logger.error(msg)
    }
  }
  
  logger.info("Finished writing openAQ 'meta' dataframes")
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

# ----- Quality Control begin -----------------------------------------------

# Remove any records missing latitude or longitude
badLocationMask <- is.na(df$longitude) | is.na(df$latitude) | (df$latitude == 0 & df$longitude ==0)
badLocationCount <- sum(badLocationMask)
if ( badLocationCount > 0 ) {
  logger.info('Discarding %s rows with invalid location information', badLocationCount)
  badLocations <- paste('(',df$longitude[badLocationMask],',',df$latitude[badLocationMask],')',sep='')
  logger.debug('Bad locations: %s', paste0(badLocations, collapse=", "))
}
df <- df[!badLocationMask,]

# ----- Quality Control end -------------------------------------------------

# add additional columns to the dataframe
logger.info('Adding \'datetime\', \'countryCode\', \'stateCode\', \'monitorID\' columns...')

df$datetime <- lubridate::ymd_hms(df$utc)
df$countryCode <- df$country
df <- openaq_assignStateCode(df)

# The monitorID column is composed of four lication elements as the unique identifier 
df$monitorID <- make.names( with(df, paste(location,city,stateCode,countryCode) ) )

## NOTE: Get rid of latitudes and longitudes that are NAs instead of using Google
#
# if ( sum( is.na(df$latitude) ) > 0) {
#   # add missing latitudes and longitudes
#   logger.info('Fill in latitudes and longitudes where they are missing')
#   
#   missingIndex <- is.na(df$latitude)
#   
#   # create a new column that combines location, city and country to be used for google later
#   
#   
#   # pull out the unique locations where lat/lon doesn't exist
#   missingLatLon <- df$monitorID[missingIndex]
#   missingLatLonUnique <- unique(missingLatLon)
#   
#   # Use geocode from ggmap to find the latitudes and longitudes
#   findLatLon <- suppressMessages( ggmap::geocode(missingLatLonUnique) )
#   findLatLon <- cbind(missingLatLonUnique, findLatLon)
#   names(findLatLon)[1] <- "monitorID"
#   
#   # Append the newly found latitudes and longitudes to the dataframe
#   df <- dplyr::left_join(df, findLatLon, by="monitorID")
#   
#   # Replacing NA in original lat/lon by new lat/lon found by google
#   df$latitude[missingIndex] <- df$lat[missingIndex]
#   df$longitude[missingIndex] <- df$lon[missingIndex]
#   
#   # get rid of the extra lon and lat columns
#   df$lat <- NULL
#   df$lon <- NULL
# } 


# 201507 has three parts, so change the name to reflect it
startdate <- as.numeric(opt$startdate)
if ( startdate %/% 100 != 201507 ) {
  startdate <- startdate %/% 100
}


# ----- Always create openAQ "meta" dataframes ----------------------------


result <- try( createMetaDataframes(df, startdate, opt$outputDir) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating openAQ 'meta' dataframes: ", geterrmessage())
  logger.fatal(msg)
}



# ----- Always create openAQ "data" dataframes --------------------------------

result <- try( createDataDataframes(df, startdate, opt$outputDir) )

if ( class(result)[1] == "try-error" ) {
  msg <- paste("Error creating openAQ 'data' dataframes: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info('Completed successfully!')
}



