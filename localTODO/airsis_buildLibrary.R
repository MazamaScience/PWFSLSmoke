#' @keywords AIRSIS
#' @export
#' @title Build a Local RData Library from AIRSIS
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitIDs vector of character or numeric unit identifiers
#' @param year eary for which to download data
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @description Smoke monitoring data from airsis.com is downloaded, quality controlled
#' and converted in to  a \code{ws_monitor} object.
#' @details When no unitIDs are supplied (the default), a range of unitIDs is generated that
#' covers all AIRSIS monitor known to exist for each provider as of May, 2016. The range will
#' contain some unitIDs for which no monitors exist so it is normal to see log messages
#' for unitIDs with no data.
#' @return \code{ws_monitor} object.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' apcd_2015 <- airsis_buildLibrary('APCD', year=2015)
#' }

if ( FALSE ) {
  
  # Test 
  library(PWFSLSmoke)
  
  setSpatialDataDir('~/Data/Spatial')
  loadSpatialData('NaturalEarthAdm1')
  logger.setup()
  logger.setLevel(DEBUG)
  
  # Silence other warning messages
  options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error
  
  usfs_2015 <- airsis_buildLibrary('USFS', unitIDs=1000:1040, year=2015)
  
}

airsis_buildLibrary <- function(provider='USFS', unitIDs=NULL, year=2015,
                                clusterDiameter=1000,
                                baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?") {
  
  
  # Silence other warning messages
  options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error
  
  # List where saved filenames are stored
  filenames <- list()
  
  if ( is.null(unitIDs) ) {
    
    if (toupper(provider) == 'APCD') {
      
      # Process AIRSIS files located at USFS.airsis.com -----------------------------
      
      dataram <- 1:2
      BAM1020 <- c(7, 1012)
      ebamNew <- 4:6
      IridiumEbams <- 1002:1028 # with other flavors mixed in
      IridiumEsams <- 1000:1001
      
      unitIDs <- c(1:9,1000:1030)
      
    } else if (toupper(provider) == 'ARB2') {
      
      unitIDs <- 1000:1040
      
    } else if (toupper(provider) == 'MARIPOSA') {
      
      unitIDs <- 1000
      
    } else if (toupper(provider) == 'USFS') {
      
      # Process AIRSIS files located at USFS -----------------------------
      
      # Datarams <- c(1:43) # with a few missing and a few extra larger numbers
      # 
      # BAM1020s <- c(49,50,84,1015) # These appear when clicking on "Bam 1020" at the URL above
      # 
      # eBams <- c(21,36,48,53,57,58,59,60,70,72,74)
      # 
      # eBamNews <- c(45:96) # with a few missing
      
      earlyEbams <- c(1:99) # with other flavors mixed in
      
      IridiumEbams <- c(1000:1055,2000) # with other flavors mixed in
      IridiumEbams <- c(1000:1005) # DELETEME
      
      unitIDs <- c(1:99,1000:1055,2000)
    }
    
  }
  
  # Begin data access ---------------------------------------------------------
  
  monitorList <- list()
  
  for (unitID in unitIDs) {
    
    unitID <- as.character(unitID)
    
    logger.info('Working on unitID %s ---------------------------------------------------------', unitID)
    
    # Download data
    
    logger.debug('Downloading %s data for unitID %s', year, unitID)
    
    startdate <- paste0(year,'0101')
    enddate <- paste0(year, '1231')

    result <- try ( mon <- airsis_createMonitorObject(startdate, enddate, provider, unitID, clusterDiameter, baseUrl),
                    silent=TRUE )
    
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.warn('Skipping unitID %s: %s', unitID, err_msg)
      next
    }
    
    monitorList[[unitID]] <- mon

  } # End of unitIDs loop
  
  # HACK:
  if ( length(monitorList) == 1 ) {
    ws_monitor <- monitorList[[1]]
  } else {
    ws_monitor <- monitor_combine(monitorList)
  }
  
  return(ws_monitor)

}

