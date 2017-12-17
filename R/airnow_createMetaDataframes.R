#' @keywords AirNow
#' @import dplyr
#' @import MazamaSpatialUtils
#' @export
#' @title Return Dataframes of AirNow Site Location Metadata
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @return List of dataframes where each dataframe contains all metadata for a unique parameter (e.g: "PM2.5", "NOX").
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

airnow_createMetaDataframes <- function(parameters=NULL) {
  
  # ----- Data Download -------------------------------------------------------
  
  logger.debug("Downloading AirNow sites metadata ...")
  
  # Create the tibble that holds a month worth of AirNow data
  result <- try( airnowRaw <- airnow_downloadSites(),
                 silent=TRUE)
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    logger.error("Unable to obtain sites tibble: %s",err_msg)
    stop(paste0("Unable to obtain sites tibble: ",err_msg))
  }
  
  logger.debug("Downloaded %d rows of AirNow sites metadata", nrow(airnowRaw))
  
  # ----- Data cleanup --------------------------------------------------------

  # Convert "O3" to "OZONE" as is used in all AirNow data files
  mask <- airnowRaw$parameterName == "O3"
  airnowRaw$parameterName[mask] <- "OZONE"
  
  # Add required column 'monitorID'
  airnowRaw$monitorID <- airnowRaw$AQSID
  
  # Remove ' ' from the end of MSAName and change the column name
  airnowRaw$msaName <- stringr::str_trim(airnowRaw$MSAName)
  
  # Remove bad stateCodes
  mask <- airnowRaw$stateCode %in% c('N/A')
  airnowRaw$stateCode[mask] <- as.character(NA)
  
  # Remove bad countyNames
  mask <- airnowRaw$countyName %in% c('N/A')
  airnowRaw$countyName[mask] <- as.character(NA)
  
  # Convert countyName from all caps to title case
  airnowRaw$countyName <- stringr::str_to_title(airnowRaw$countyName)
  
  # NOTE:  Don't stringr::str_to_title(siteName) because it might include all caps identifiers like "USFS"
  
  # Remove bad locations
  mask <- airnowRaw$longitude == 0 & airnowRaw$latitude == 0
  badLocationIDs <- paste(airnowRaw$AQSID[mask], collapse=", ")
  logger.debug("Replacing (0,0) locations with (NA,NA) for IDs: %s", badLocationIDs)
  airnowRaw$longitude[mask] <- as.numeric(NA)
  airnowRaw$latitude[mask] <- as.numeric(NA)
  
  # Remove bad elevations (zero seems to be used as a missing value flag)
  mask <- airnowRaw$elevation <= 0.0
  airnowRaw$elevation[mask] <- as.numeric(NA)
  
  # ----- Subset and add metadata ---------------------------------------------
  CANAMEX <- c('CA','US','MX')
  airnowRaw <- dplyr::filter(airnowRaw, countryCode %in% CANAMEX)
  
  # For later testing
  old_airnowRaw <- airnowRaw
  
  # Do spatial searching only for unique locations to speed things up
  sitesUnique <- airnowRaw[!duplicated(airnowRaw$monitorID),]
  suppressWarnings({
    sitesUnique <- addMazamaMetadata(sitesUnique, 'longitude', 'latitude', countryCodes = CANAMEX)
  })
  # Now add the per-AQSID Mazama metadata to the larger dataframe
  # NOTE:  We need to remove the columns from airnowRaw that we replace with left_join()
  airnowRaw$countryCode <- NULL
  airnowRaw$stateCode <- NULL
  airnowRaw <- dplyr::left_join(airnowRaw, sitesUnique[,c('AQSID','countryCode','stateCode','timezone')], by='AQSID')
  
  # Sanity check
  if ( any(airnowRaw$countryCode != old_airnowRaw$countryCode) ) {
    indices <- which(airnowRaw$countryCode != old_airnowRaw$countryCode)
    monitorIDString <- paste0(airnowRaw$monitorID[indices], collapse=", ")
    logger.debug('addMazamaMetadata() changed the countryCode for monitors: %s', monitorIDString)
    # Next line is for debugging
    mzm_airnowRaw <- airnowRaw
    # NOTE:  Neither the simpleCountriesEEZ nor the NaturalEarthAdm1 datasets used in MazamaSpatialUtils
    # NOTE:  is of high enough resolution to accurately assign states and countries for sites on
    # NOTE:  wiggly rivers near boundaries.
    # NOTE:  Although there are some obvious mismatches between locations and sitenames in the AirNow data,
    # NOTE:  we will use their countryCode and stateCode where they disagree with MazamaSpatialUtils.
    airnowRaw$countryCode[indices] <- old_airnowRaw$countryCode[indices]
    airnowRaw$stateCode[indices] <- old_airnowRaw$stateCode[indices]
  }
  
  # TODO:  FIX THIS HACK
  # NOTE:  Best guess is that at this point all monitors currently reporting frin US.MX are actually in US.TX
  # NOTE:  CA.CC monitors are tougher
  # NOTE:  The AirNow sites file has at least half a dozen mismatches between lat-lon and asociated location information
  mask <- airnowRaw$countryCode == 'US' & airnowRaw$stateCode == 'MX'
  airnowRaw$stateCode[mask] <- 'TX'
  
  # Set any remaining invalid stateCodes to NA
  mask <- airnowRaw$stateCode %in% c('CC')
  airnowRaw$stateCode[mask] <- as.character(NA)

  # > names(airnowRaw)
  #  [1] "AQSID"          "parameterName"  "siteCode"       "siteName"       "status"        
  #  [6] "agencyID"       "agencyName"     "EPARegion"      "latitude"       "longitude"     
  # [11] "elevation"      "GMTOffsetHours" "countryCode"    "FIPSCMSACode"   "CMSAName"      
  # [16] "FIPSMSACode"    "MSAName"        "FIPSStateCode"  "stateCode"      "GNISCountyCode"
  # [21] "countyName"     "GNISCityCode"   "cityName"       "monitorID"      "timezone"
  # [22] "msaName"
  
  retainedColumns <- c('monitorID', 'longitude', 'latitude', 'elevation', 
                       'timezone', 'countryCode', 'stateCode',
                       'siteName', 'agencyName', 'countyName', 'msaName')
  

  # TODO:  Could add HUC12 or any other MazamaSpatialUtils datasets
  
  # ----- Data Reshaping ------------------------------------------------------
  
  logger.debug("Reshaping AirNow sites metadata ...")
  
  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(airnowRaw$parameterName))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(airnowRaw$parameterName))
    invalidParameters <- dplyr::setdiff(parameters, unique(airnowRaw$parameterName))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in AirNow sites metadata: %s", paste0(invalidParameters, collapse=", "))
    }
  }
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
  
  # Use dplyr to seprate the data by parameter
  for (parameter in parameters) {
    
    # Create a tbl with unique sites for this parameter
    tbl <- dplyr::filter(airnowRaw, airnowRaw$parameterName == parameter) %>%
      dplyr::select(retainedColumns) %>%
      dplyr::distinct()
    
    # Add extra metadata columns
    # * `monitorType` -- broad instrument categories for E-Sampler, EBAM or BAM-1020
    # * `monitorInstrument` -- specific instrument identifiers
    # * `aqsID` -- AQS site identifier (often used as the `monitorID`)
    # * `pwfslID` -- PWFSL site identifier (used as the `monitorID` for temporary monitors)
    # * `telemetryAggregator` -- data provider for temporary monitors (e.g. 'wrcc' or 'usfs.airsis')
    # * `telemetryUnitID` -- unique ID for each monitoring site used within the `telemetryAggregator`
    tbl$monitorType <- as.character(NA)
    tbl$monitorInstrument <- as.character(NA)
    tbl$aqsID <- tbl$monitorID
    tbl$pwfslID <- as.character(NA)
    tbl$pwfslDataIngestSource <- 'AIRNOW'
    tbl$telemetryAggregator <- as.character(NA)
    tbl$telemetryUnitID <- as.character(NA)

    # Convert from tibble to standard data.frame so we can add rownames as per ws_monitor convention
    df <- as.data.frame(tbl)
    rownames(df) <- df$monitorID
    dfList[[parameter]] <- df
    
  }
  
  return(dfList)
  
}

