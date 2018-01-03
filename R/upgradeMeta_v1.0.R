#' @export
#' @title Upgrade ws_monitor Metadata to Version 1.0
#' @param ws_monitor \emph{ws_monitor} object
#' @description Upgrade a \emph{ws_monitor} object to version 1.0 standards.
#' @return A \emph{ws_monitor} object with version 1.0 metadata.

upgradeMeta_v1.0 <- function(ws_monitor) {
  
  old_meta <- ws_monitor$meta
  old_data <- ws_monitor$data
  
  # Sanity check
  v0_columns <- c("AQSID","siteCode","siteName","status","agencyID",
                  "agencyName","EPARegion","latitude","longitude",
                  "elevation","GMTOffsetHours","countryCode","FIPSCMSACode",
                  "CMSAName","FIPSMSACode","MSAName","FIPSStateCode",
                  "stateCode","GNISCountyCode","countyName","GNISCityCode",
                  "cityName","timezone","monitorID")
  unknownColumns <- length(setdiff(names(old_meta), v0_columns))
  if (  length(unknownColumns)> 0 ) {
    logger.error("Invalid columns found in ws_monitor: %s", paste0(unknownColumns, collapse=","))
    logger.error("Has this ws_monitor object already been upgraded?")
    stop(paste0("Invalid columns found in ws_monitor: %s", paste0(unknownColumns, collapse=",")))
  }
  
  # Create v1.x style siteID
  aqsMask <- stringr::str_count(old_meta$monitorID) == 9
  siteID <- old_meta$AQSID
  old_pwfsl_location <- stringr::str_split_fixed(ws_monitor$meta$monitorID, "__",2)[,2]
  siteID[!aqsMask] <- old_pwfsl_location[!aqsMask]
  
  # Our v0.x 'meta' dataframe contains the following columns:
  #
  # > names(Northwest_Megafires$meta)
  #  [1] "AQSID"          "siteCode"       "siteName"       "status"         "agencyID"      
  #  [6] "agencyName"     "EPARegion"      "latitude"       "longitude"      "elevation"     
  # [11] "GMTOffsetHours" "countryCode"    "FIPSCMSACode"   "CMSAName"       "FIPSMSACode"   
  # [16] "MSAName"        "FIPSStateCode"  "stateCode"      "GNISCountyCode" "countyName"    
  # [21] "GNISCityCode"   "cityName"       "timezone"       "monitorID"     
  #
  # The PWFSLSmoke v1.0 data model contains the following parameters
  # 
  # > names(meta)
  #  [1] "monitorID"             "longitude"             "latitude"              "elevation"            
  #  [5] "timezone"              "countryCode"           "stateCode"             "siteName"             
  #  [9] "agencyName"            "countyName"            "msaName"               "monitorType"          
  # [13] "siteID"                "instrumentID"          "aqsID"                 "pwfslID"              
  # [17] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"      
  
  # NOTE:  'meta' must be a dataframe because it has rownames which are deprecated in tibbles
  # Create empty dataframe
  meta <- as.data.frame(matrix(nrow=nrow(old_meta),ncol=19), stringsAsFactors=FALSE)
  
  colNames <- c("monitorID", "longitude", "latitude",
                "elevation", "timezone", "countryCode",
                "stateCode", "siteName", "agencyName",
                "countyName", "msaName", "monitorType",
                "siteID", "instrumentID", "aqsID", "pwfslID",
                "pwfslDataIngestSource", "telemetryAggregator", "telemetryUnitID")
  
  names(meta) <- colNames
  
  # Assign data where we have it
  meta$longitude <- as.numeric(old_meta$longitude)
  meta$latitude <- as.numeric(old_meta$latitude)
  meta$elevation <- as.numeric(old_meta$elevation)
  meta$timezone <- as.character(old_meta$timezone)
  meta$countryCode <- as.character(old_meta$countryCode)
  meta$stateCode <- as.character(old_meta$stateCode)
  meta$siteName <- as.character(old_meta$siteName)
  meta$countyName <- as.character(old_meta$countyName)
  meta$msaName <- as.character(old_meta$MSAName)
  meta$agencyName <- as.character(old_meta$agencyName)
  meta$monitorType <- as.character(NA)
  meta$siteID <- as.character(siteID)
  meta$instrumentID <- "01"
  meta$aqsID <- as.character(old_meta$AQSID)
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- as.character(NA)
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)
  
  meta$monitorID <- paste(meta$siteID, meta$instrumentID, sep='_')
  
  # Assign rownames and colnames
  rownames(meta) <- meta$monitorID
  colnames(data) <- c('datetime',meta$monitorID)
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=old_data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}

