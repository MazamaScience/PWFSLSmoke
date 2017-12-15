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
#' AirNow data parameters include at least the following list:
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
  
  # Create the data frame that holds a month worth of AirNow data
  result <- try( airnowRaw <- airnow_downloadSites(),
                 silent=TRUE)
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    logger.error("Unable to obtain sites dataframe: %s",err_msg)
    stop(paste0("Unable to obtain sites dataframe: ",err_msg))
  }
  
  logger.debug("Downloaded %d rows of AirNow sites metadata", nrow(airnowRaw))
  
  
  # ----- Adding Required Columns ---------------------------------------------
  
  airnowRaw$monitorID <- airnowRaw$AQSID
  
  # latitude, longitude and elevation alreay exist
  
  # Fix bad locations
  badLocationMask <- airnowRaw$longitude == 0 & airnowRaw$latitude == 0
  badLocationIDs <- paste(airnowRaw$AQSID[badLocationMask], collapse=", ")
  
  logger.debug("Replacing (0,0) locations with (NA,NA) for IDs: %s", badLocationIDs)
  
  airnowRaw$longitude[badLocationMask] <- NA
  airnowRaw$latitude[badLocationMask] <- NA
  
  # NOTE:  Instead of calling addMazamaMetadata, we just add timezone "by hand"
  # Do spatial searching only for unique locations to speed things up
  sitesUnique <- airnowRaw[!duplicated(airnowRaw$AQSID),]
  sitesUnique$timezone <- suppressWarnings({
    MazamaSpatialUtils::getTimezone(sitesUnique$longitude, sitesUnique$latitude,
                                    countryCodes=unique(sitesUnique$countryCode), useBuffering=TRUE)
  })
  # Now add the per-AQSID timezones to the larger dataframe
  airnowRaw <- dplyr::left_join(airnowRaw, sitesUnique[,c('AQSID','timezone')], by='AQSID')

  # ----- Data cleanup --------------------------------------------------------
  
  # remove ' ' from the end of MSAName and chante the column name
  airnowRaw$msaName <- stringr::str_trim(airnowRaw$MSAName)

  # convert countyName from all caps to title case
  airnowRaw$countyName <- stringr::str_to_title(airnowRaw$countyName)
  
  # TODO:  Use MazamaSpatialUtils to get stateCode when countryCode %in% c('CA','MX')
  # TODO:  handle 'N/A' in stateCode
  # TODO:  check for other atrocities
  # TODO:  
  
  # NOTE:  Don't convert siteName because it might include all caps identifiers like "USFS"
  
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
    tbl$telemetryAggregator <- as.character(NA)
    tbl$telemetryUnitID <- as.character(NA)

    # Convert from tibble to standard data.frame so we can add rownames as per ws_monitor convention
    df <- as.data.frame(tbl)
    rownames(df) <- df$monitorID
    dfList[[parameter]] <- df
    
  }
  
  return(dfList)
  
}

