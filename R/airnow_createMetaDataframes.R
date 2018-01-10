#' @keywords AirNow
#' @import dplyr
#' @import MazamaSpatialUtils
#' @export
#' @title Create Dataframes of AirNow Site Location Metadata
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param pwfslDataIngestSource identifier for the source of monitoring data, e.g. \code{'AIRNOW'}
#' @param addGoogleMeta logicial specifying wheter to use Google elevation and reverse geocoding services
#' @return List of 'meta' dataframes with site metadata for unique parameters (e.g: "PM2.5", "NOX").
#' @description The \code{airnow_createMetaDataframes()} function uses the \code{airnow_downloadSites()} function 
#' to download site metadata from AirNow and restructures that data into a format that is compatible
#' with the PWFSLSmoke package \emph{ws_monitor} data model.
#' 
#' The \code{meta} dataframe in the \emph{ws_monitor} data model has metadata associated with monitoring
#' site locations for a specific parameter and must contain at least the following columns:
#' \itemize{
#'   \item{monitorID -- per deployment unique ID}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{elevation -- height above sea level in meters}
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#' 
#' The \code{meta} dataframe will have rownames matching \code{monitorID}.
#' 
#' This function takes a dataframe obtained from AirNowTech's
#' \code{monitoring_site_locations.dat} file, splits it up into separate dataframes,
#' one for each parameter, and performs the following cleanup:
#' \itemize{
#'   \item{convert incorrect values to \code{NA} e.g. longitude=0 & latitude=0}
#'   \item{add timezone information}
#' }
#' 
#' Parameters included in AirNow data include at least the following list:
#' \enumerate{
#' \item{BARPR}
#' \item{BC}
#' \item{CO}
#' \item{NO}
#' \item{NO2}
#' \item{NO2Y}
#' \item{NO2X}
#' \item{NOX}
#' \item{NOOY}
#' \item{OC}
#' \item{OZONE}
#' \item{PM10}
#' \item{PM2.5}
#' \item{PRECIP}
#' \item{RHUM}
#' \item{SO2}
#' \item{SRAD}
#' \item{TEMP}
#' \item{UV-AETH}
#' \item{WD}
#' \item{WS}
#' }
#' 
#' Setting \code{parameters=NULL} will generate a separate dataframe for each of the above parameters.
#' @seealso \link{airnow_downloadSites}
#' @examples
#' \dontrun{
#' metaList <- airnow_createMetaDataframes(parameters="PM2.5")
#' }

airnow_createMetaDataframes <- function(parameters=NULL,
                                        pwfslDataIngestSource='AIRNOW',
                                        addGoogleMeta=TRUE) {
  
  # ----- Data Download -------------------------------------------------------
  
  logger.debug("Downloading AirNow sites metadata ...")
  
  # Create the tibble that holds a month worth of AirNow data
  result <- try( airnowTbl <- airnow_downloadSites(),
                 silent=TRUE)
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    logger.error("Unable to obtain sites tibble: %s",err_msg)
    stop(paste0("Unable to obtain sites tibble: ",err_msg))
  }
  
  logger.debug("Downloaded %d rows of AirNow sites metadata", nrow(airnowTbl))

  # > names(airnowTbl)
  #  [1] "AQSID"          "parameterName"  "siteCode"       "siteName"       "status"        
  #  [6] "agencyID"       "agencyName"     "EPARegion"      "latitude"       "longitude"     
  # [11] "elevation"      "GMTOffsetHours" "countryCode"    "FIPSCMSACode"   "CMSAName"      
  # [16] "FIPSMSACode"    "MSAName"        "FIPSStateCode"  "stateCode"      "GNISCountyCode"
  # [21] "countyName"     "GNISCityCode"   "cityName"      

  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(airnowTbl$parameterName))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(airnowTbl$parameterName))
    invalidParameters <- dplyr::setdiff(parameters, unique(airnowTbl$parameterName))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in AirNow sites metadata: %s", paste0(invalidParameters, collapse=", "))
    }
  }
  
  # Filter for parameters
  airnowTbl <- dplyr::filter(airnowTbl, airnowTbl$parameterName %in% parameters)
  if ( nrow(airnowTbl) == 0 ) {
    parametersString <- paste(parameters, collapse=", ")
    logger.error("No available sites for: %s",parametersString)
    stop(paste0("No available sites for: ",parametersString))
  }
  
  # ----- Data cleanup --------------------------------------------------------

  # Convert "O3" to "OZONE" as is used in all AirNow data files
  mask <- airnowTbl$parameterName == "O3"
  airnowTbl$parameterName[mask] <- "OZONE"
  
  # Remove ' ' from the end of MSAName
  airnowTbl$MSAName <- stringr::str_trim(airnowTbl$MSAName)
  
  # Remove bad stateCodes
  mask <- airnowTbl$stateCode %in% c('N/A')
  airnowTbl$stateCode[mask] <- as.character(NA)
  
  # Remove bad countyNames
  mask <- airnowTbl$countyName %in% c('N/A')
  airnowTbl$countyName[mask] <- as.character(NA)
  
  # Convert countyName from all caps to title case
  airnowTbl$countyName <- stringr::str_to_title(airnowTbl$countyName)
  
  # NOTE:  Don't stringr::str_to_title(siteName) because it might include all caps identifiers like "USFS"
  
  # Remove bad locations
  mask <- airnowTbl$longitude == 0 & airnowTbl$latitude == 0
  badLocationIDs <- paste(airnowTbl$AQSID[mask], collapse=", ")
  logger.debug("Replacing (0,0) locations with (NA,NA) for AQSIDs: %s", badLocationIDs)
  airnowTbl$longitude[mask] <- as.numeric(NA)
  airnowTbl$latitude[mask] <- as.numeric(NA)
  
  # Remove bad elevations (zero seems to be used as a missing value flag)
  mask <- airnowTbl$elevation <= 0.0
  airnowTbl$elevation[mask] <- as.numeric(NA)
  airnowTbl$elevation <- round(airnowTbl$elevation, 0) # round to whole meters
  
  # ----- Subset and add Mazama metadata and USGS elevation -------------------
  
  # Restrict to North America
  CANAMEX <- c('CA','US','MX')
  airnowTbl <- dplyr::filter(airnowTbl, airnowTbl$countryCode %in% CANAMEX)
  
  # For later testing
  old_airnowTbl <- airnowTbl
  
  # Do spatial searching only for unique locations to speed things up
  sitesUnique <- airnowTbl[!duplicated(airnowTbl$AQSID),]
  suppressWarnings({
    sitesUnique <- addMazamaMetadata(sitesUnique, 'longitude', 'latitude', countryCodes = CANAMEX)
    # TODO:  Handle addGoogleMeta
    # if ( addGoogleMeta ) {
    #   # Add elevation, siteName and countyName
    #   sitesUnique <- addGoogleElevation(sitesUnique, 'longitude', 'latitude')
    #   sitesUnique <- addGoogleAddress(sitesUnique, 'longitude', 'latitude')
    # }
  })
  # Now add the per-AQSID Mazama metadata to the larger dataframe
  # NOTE:  We need to remove the columns from airnowTbl that we replace with left_join()
  airnowTbl$countryCode <- NULL
  airnowTbl$stateCode <- NULL
  airnowTbl <- dplyr::left_join(airnowTbl, sitesUnique[,c('AQSID','countryCode','stateCode','timezone')], by='AQSID')
  
  # Sanity check
  if ( any(airnowTbl$countryCode != old_airnowTbl$countryCode) ) {
    indices <- which(airnowTbl$countryCode != old_airnowTbl$countryCode)
    AQSIDString <- paste0(airnowTbl$AQSID[indices], collapse=", ")
    logger.debug('addMazamaMetadata() changed the countryCode for AQSIDs: %s', AQSIDString)
    # Next line is for debugging
    mzm_airnowTbl <- airnowTbl
    # NOTE:  Neither the simpleCountriesEEZ nor the NaturalEarthAdm1 datasets used in MazamaSpatialUtils
    # NOTE:  is of high enough resolution to accurately assign states and countries for sites on
    # NOTE:  wiggly rivers near boundaries.
    # NOTE:  Although there are some obvious mismatches between locations and sitenames in the AirNow data,
    # NOTE:  we will use their countryCode and stateCode where they disagree with MazamaSpatialUtils.
    airnowTbl$countryCode[indices] <- old_airnowTbl$countryCode[indices]
    airnowTbl$stateCode[indices] <- old_airnowTbl$stateCode[indices]
  }
  
  # TODO:  FIX THIS HACK
  # NOTE:  Best guess is that at this point all monitors currently reporting frin US.MX are actually in US.TX
  # NOTE:  CA.CC monitors are tougher
  # NOTE:  The AirNow sites file has at least half a dozen mismatches between lat-lon and asociated location information
  mask <- airnowTbl$countryCode == 'US' & airnowTbl$stateCode == 'MX'
  airnowTbl$stateCode[mask] <- 'TX'
  
  # Set any remaining invalid stateCodes to NA
  mask <- airnowTbl$stateCode %in% c('CC')
  airnowTbl$stateCode[mask] <- as.character(NA)

  # ----- Data Reshaping ------------------------------------------------------
  
  logger.debug("Reshaping AirNow sites metadata ...")
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
  
  # Use dplyr to seprate the data by parameter
  for ( parameter in parameters ) {
    
    # Create a tbl with unique sites for this parameter
    tbl <- dplyr::filter(airnowTbl, airnowTbl$parameterName == parameter) %>%
      distinct()

    # Our tibble now contains the following columns:
    #
    # > names(airnowTbl)
    #  [1] "AQSID"          "parameterName"  "siteCode"       "siteName"       "status"        
    #  [6] "agencyID"       "agencyName"     "EPARegion"      "latitude"       "longitude"     
    # [11] "elevation"      "GMTOffsetHours" "FIPSCMSACode"   "CMSAName"       "FIPSMSACode"   
    # [16] "MSAName"        "FIPSStateCode"  "GNISCountyCode" "countyName"     "GNISCityCode"  
    # [21] "cityName"       "countryCode"    "stateCode"      "timezone"      
    #
    # The PWFSLSmoke v1.0 data model contains the following parameters
    # 
    # > names(meta)
    #  [1] "monitorID"             "longitude"             "latitude"              "elevation"            
    #  [5] "timezone"              "countryCode"           "stateCode"             "siteName"             
    #  [9] "agencyName"            "countyName"            "msaName"               "monitorType"          
    # [13] "siteID"                "instrumentID"          "aqsID"                 "pwfslID"              
    # [17] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"      
    
    meta <- createEmptyMetaDataframe(nrow(tbl))
    
    # Assign data where we have it
    meta$longitude <- as.numeric(tbl$longitude)
    meta$latitude <- as.numeric(tbl$latitude)
    meta$elevation <- as.numeric(tbl$elevation)
    meta$timezone <- as.character(tbl$timezone)
    meta$countryCode <- as.character(tbl$countryCode)
    meta$stateCode <- as.character(tbl$stateCode)
    meta$siteName <- as.character(tbl$siteName)
    meta$countyName <- as.character(tbl$countyName)
    meta$msaName <- as.character(tbl$MSAName)
    meta$agencyName <- as.character(tbl$agencyName)
    meta$monitorType <- as.character(NA)
    meta$siteID <- as.character(tbl$AQSID)
    meta$instrumentID <- "01" # Monitors are not identified and we should only have a single site-monitor
    meta$aqsID <- as.character(tbl$AQSID)
    meta$pwfslID <- as.character(NA)
    meta$pwfslDataIngestSource <- as.character(pwfslDataIngestSource)
    meta$telemetryAggregator <- as.character(NA)
    meta$telemetryUnitID <- as.character(NA)
    
    meta$monitorID <- paste(meta$siteID, meta$instrumentID, sep='_')
    
    # Assign rownames
    rownames(meta) <- meta$monitorID
    
    dfList[[parameter]] <- meta
    
  }
  
  return(dfList)
  
}

