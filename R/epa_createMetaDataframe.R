#' @keywords EPA
#' @export
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

epa_createMetaDataframe <- function(tbl,
                                    pwfslDataIngestSource='EPA',
                                    existingMeta=NULL,
                                    addGoogleMeta=TRUE) {
  
  logger.debug(paste0('Creating meta dataframe ...'))
  
  # Subset tbl to include only site-specific columns
  columns <- c('Site Num','Parameter Code','Latitude','Longitude',
               'State Name','County Name','monitorID')
  tbl <- dplyr::select(tbl, columns) %>% distinct()

  # Our tibble now contains the following columns:
  #
  # > names(tbl)
  # [1] "Site Num"       "Parameter Code" "Latitude"       "Longitude"     
  # [5] "State Name"     "County Name"    "monitorID"         
  #
  # The PWFSLSmoke v1.0 data model contains the following parameters
  # 
  #  [1] "monitorID"             "longitude"             "latitude"             
  #  [4] "elevation"             "timezone"              "countryCode"          
  #  [7] "stateCode"             "siteName"              "agencyName"           
  # [10] "countyName"            "msaName"               "monitorType"          
  # [13] "monitorInstrument"     "aqsID"                 "pwfslID"              
  # [16] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"      
  
  # NOTE:  'meta' must be a dataframe because it has rownames which are deprecated in tibbles
  # Create empty dataframe
  meta <- as.data.frame(matrix(nrow=nrow(tbl),ncol=18))
  
  colNames <- c("monitorID", "longitude", "latitude",
                "elevation", "timezone", "countryCode",
                "stateCode", "siteName", "agencyName",
                "countyName", "msaName", "monitorType",
                "monitorInstrument", "aqsID", "pwfslID",
                "pwfslDataIngestSource", "telemetryAggregator", "telemetryUnitID")
  
  names(meta) <- colNames
  
  # Assign data where we have it
  meta$monitorID <- tbl$monitorID
  meta$longitude <- tbl$Longitude
  meta$latitude <- tbl$Latitude
  meta$elevation <- as.numeric(NA)
  meta$timezone <- as.character(NA)
  meta$countryCode <- as.character(NA)
  meta$stateCode <- as.character(NA)
  meta$siteName <- as.character(NA)
  meta$agencyName <- as.character(NA)
  meta$countyName <- tbl$`County Name`
  meta$msaName <- as.character(NA)
  meta$monitorType <- as.character(NA)
  meta$monitorInstrument <- as.character(NA)
  meta$aqsID <- tbl$monitorID
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- pwfslDataIngestSource
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  # Add timezones, state and country codes
  meta <- addMazamaMetadata(meta, existingMeta=existingMeta)
  
  # TODO:  Could assign other spatial identifiers like EPARegion, etc.
  
  # Add elevation, siteName and countyName
  # NOTE:  Check and loop to see if we need to use addGoogleMetadata multiple 
  # NOTE:  times due to google's per-query limit
  loopCount <- nrow(meta) %/% 300
  if ( nrow(meta) %% 300 > 0) {
    loopCount <- loopCount + 1
  }
  
  if ( addGoogleMeta ) {
    
    logger.debug("Adding Google metadata ...")
    metaList <- list()
    for (i in 1:loopCount) {
      startIndex <- (i-1) * 300 + 1
      if (i != loopCount) {
        endIndex <- i * 300
      } else {
        endIndex <- nrow(meta)
      }
      m <- addGoogleElevation(meta[startIndex:endIndex,], existingMeta=existingMeta)
      m <- addGoogleAddress(m, existingMeta=existingMeta)
      metaList[[i]] <- m
    }
    meta <- dplyr::bind_rows(metaList)
    
  }
  
  # Convert some columns to character even if they have all NA
  characterColumns <- c('siteName','agencyName','countyName','msaName','monitorType',
                        'monitorInstrument','aqsID')
  for (colName in characterColumns) {
    meta[[colName]] <- as.character(meta[[colName]])
  }
  
  logger.info("Created 'meta' dataframe with %d rows and %d columns", nrow(meta), ncol(meta))
  
  return(meta)
  
}
