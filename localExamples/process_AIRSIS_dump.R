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
logger.setLevel(INFO)

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
               '~/Data/monitors/TCAPCD_ebam.csv', # EBAM
               '~/Data/monitors/USFS_ebam.csv',   # EBAM
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


