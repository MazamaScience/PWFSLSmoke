###############################################################################
# Create merged dataframe from AIRSIS dump file
#
# > names(EBAM_SitesMetadata)
# [1] "AQSID"          "siteCode"       "siteName"       "status"        
# [5] "agencyID"       "agencyName"     "EPARegion"      "latitude"      
# [9] "longitude"      "elevation"      "timezone"       "GMTOffsetHours"
#[13] "countryCode"    "FIPSMSACode"    "MSAName"        "FIPSStateCode" 
#[17] "stateCode"      "GNISCountyCode" "countyName"     "monitorID"     
#[21] "has_PM2.5"     
#
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(DEBUG)

# ebamFiles=c('APCD_ebam_bam.csv','ARB2.csv','mrpsa_ebam.csv','TCAPCD_ebam.csv','USFS_ebam_bam.csv'),
# # HACK -- temporarily remove ARB2.csv


# APCD_esam.csv
# ARB2.csv
### GBUAPCD.csv
### ODEQ.csv
# TCAPCD_ebam.csv
### USFS_bam1020.csv
# USFS_ebam.csv
# USFS_esam.csv
### WASHOE.csv
# mrpsa_ebam.csv

dumpFiles <- c('~/Data/monitors/ARB2.csv',        # EBAM
               '~/Data/monitors/GBUAPCD.csv',     # EBAM
               '~/Data/monitors/ODEQ.csv',        # EBAM
               '~/Data/monitors/TCAPCD_ebam.csv', # EBAM
               '~/Data/monitors/USFS_ebam.csv',   # EBAM
               '~/Data/monitors/WASHOE.csv',      # EBAM
               '~/Data/monitors/mrpsa_ebam.csv',  # EBAM
               '~/Data/monitors/APCD_esam.csv',   # E-Sampler
               '~/Data/monitors/USFS_esam.csv')   # E-Sampler

monitorList <- list()
for (filepath in dumpFiles) {
  logger.info('Ingesting %s', filepath)
  result <- try( ws_monitor <- airsisDump_createMonitorObject(filepath),
                 silent=TRUE)
  if ( class(result)[1] == "try-error") {
    err_msg <- geterrmessage()
    if (stringr::str_detect(err_msg, "No valid PM2.5 data")) {
      logger.debug('No data -- skipping %s', filepath)
    } else {
      logger.error('Unable to parse %s', filepath)
    }
  } else {
   monitorList[[basename(filepath)]] <- ws_monitor 
  }
}

airsisNew <- monitor_combine(monitorList)

newData <- airsisNew$data
newMeta <- airsisNew$meta

# Add has_PM2.5 to match existing meta
newMeta$has_PM2.5 <- TRUE

# TODO:  read in airsisOLD meta, data
oldData <- get(load('~/Data/EBAM_PM2.5_Latest.RData'))
oldMeta <- get(load('~/Data/EBAM_SitesMetadata.RData'))

if ( !'monitorType' %in% names(oldMeta) ) {
  # Add 'monitortype' and 'has_pm2.5' columns in the proper order
  oldMeta <- oldMeta[1:20]
  oldMeta$monitorType <- 'EBAM'
  oldMeta$has_PM2.5 <- TRUE
}

# TODO:  create empty data dataframe with appropriate time axis

todayGMT <- as.POSIXct( lubridate::today(), tz="GMT")
timeAxis <- seq(todayGMT - lubridate::ddays(9), todayGMT + lubridate::ddays(1), by='hours')
data <- data.frame(datetime=timeAxis)

# TODO:  join old data to new axis, join new data to new axis, subset any valid data
data <- dplyr::left_join(data, oldData, by='datetime')
data <- dplyr::left_join(data, newData, by='datetime')

allMissingMask <- apply(data, 2, function(x) { all(is.na(x)) })
data <- data[,!allMissingMask]

# TODO:  join all meta, subset to only include sites with valid data
meta <- dplyr::bind_rows(oldMeta, newMeta)
rownames(meta) <- meta$monitorID
meta <- meta[colnames(data)[-1],]

# TODO:  save both, and combined ws_monitor object




