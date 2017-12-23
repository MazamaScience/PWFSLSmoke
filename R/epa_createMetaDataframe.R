#' @keywords EPA
#' @export
#' @importFrom magrittr '%>%'
#' @title Create Sites Metadata Dataframe
#' @param tbl an EPA raw tibble after metadata enhancement
#' @param pwfslDataIngestSource identifier for the source of monitoring data, e.g. \code{'EPA_hourly_88101_2016.zip'}
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @param addGoogleMeta logicial specifying wheter to use Google elevation and reverse geocoding services
#' @description After addtional columns(i.e. \code{datetime}, and \code{monitorID}) 
#' have been applied to an EPA dataframe, we are ready to 
#' pull out site information associated with unique monitorID.
#' 
#' These will be rearranged into a dataframe organized as deployment-by-property
#' with one row for each monitorID.
#'
#' This site information found in \code{tbl} is augmented so that we end up with a uniform
#' set of properties associated with each monitorID. The list of
#' columns in the returned \code{meta} dataframe is:
#' 
#' \preformatted{
#' > names(p$meta)
#'  [1] "monitorID"             "longitude"             "latitude"             
#'  [4] "elevation"             "timezone"              "countryCode"          
#'  [7] "stateCode"             "siteName"              "agencyName"           
#' [10] "countyName"            "msaName"               "monitorType"          
#' [13] "monitorInstrument"     "aqsID"                 "pwfslID"              
#' [16] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"      
#' }
#' @return A \code{meta} dataframe for use in a \emph{ws_monitor} object.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}

epa_createMetaDataframe <- function(tbl,
                                    pwfslDataIngestSource='EPA',
                                    existingMeta=NULL,
                                    addGoogleMeta=TRUE) {
  
  logger.debug(paste0('Creating meta dataframe ...'))
  
  # Subset tbl to contain only unique sites
  siteColumns <- c('State Code', 'County Code', 'Site Num','Parameter Code',
               'POC','Latitude','Longitude','State Name','County Name')
  tbl <- dplyr::select(tbl, siteColumns) %>% distinct()

  # Our tibble now contains the following columns:
  #
  # > names(tbl)
  #  [1] "State Code"          "County Code"         "Site Num"            "Parameter Code"     
  #  [5] "POC"                 "Latitude"            "Longitude"           "Datum"              
  #  [9] "Parameter Name"      "Date Local"          "Time Local"          "Date GMT"           
  # [13] "Time GMT"            "Sample Measurement"  "Units of Measure"    "MDL"                
  # [17] "Uncertainty"         "Qualifier"           "Method Type"         "Method Code"        
  # [21] "Method Name"         "State Name"          "County Name"         "Date of Last Change"
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
  meta <- as.data.frame(matrix(nrow=nrow(tbl),ncol=19), stringsAsFactors=FALSE)
  
  colNames <- c("monitorID", "longitude", "latitude",
                "elevation", "timezone", "countryCode",
                "stateCode", "siteName", "agencyName",
                "countyName", "msaName", "monitorType",
                "siteID", "instrumentID", "aqsID", "pwfslID",
                "pwfslDataIngestSource", "telemetryAggregator", "telemetryUnitID")
  
  names(meta) <- colNames
  
  # Assign data where we have it
  meta$longitude <- as.numeric(tbl$Longitude)
  meta$latitude <- as.numeric(tbl$Latitude)
  meta$elevation <- as.numeric(NA)
  meta$timezone <- as.character(NA)
  meta$countryCode <- as.character(NA)
  meta$stateCode <- as.character(NA)
  meta$siteName <- as.character(NA)
  meta$countyName <- as.character(NA)
  meta$msaName <- as.character(NA)
  meta$agencyName <- as.character(NA)
  meta$monitorType <- as.character(NA) # TODO:  Could figure this out by looking at 'Method Name' in original data
  meta$siteID <- paste0(tbl$`State Code`,tbl$`County Code`,tbl$`Site Num`)
  meta$instrumentID <- sprintf("%02d", as.numeric(tbl$POC))
  meta$aqsID <- paste0(tbl$`State Code`,tbl$`County Code`,tbl$`Site Num`)
  meta$pwfslID <- as.character(NA) # TODO:  This will be obtained from the "known_location" service
  meta$pwfslDataIngestSource <- as.character(pwfslDataIngestSource)
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)
  
  meta$monitorID <- paste(meta$siteID, meta$instrumentID, sep='_')
  
  # Add timezones, state and country codes
  meta <- addMazamaMetadata(meta, existingMeta=existingMeta)
  
  # TODO:  Could assign other spatial identifiers like EPARegion, etc.
  
  if ( addGoogleMeta ) {
    # Add elevation, siteName and countyName
    meta <- addGoogleElevation(meta, existingMeta=existingMeta)
    meta <- addGoogleAddress(meta, existingMeta=existingMeta)
  }

  # Restrict to North America
  CANAMEX <- c('CA','US','MX')
  meta <- dplyr::filter(meta, meta$countryCode %in% CANAMEX)
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  logger.info("Created 'meta' dataframe with %d rows and %d columns", nrow(meta), ncol(meta))
  
  return(meta)
  
}
