#' @keywords OpenAQ
#' @export
#' @title Create OpenAQ Site Location Metadata Dataframe
#' @param df a OpenAQ dataframe after metadata enhancement
#' @param parameters pollutant name
#' @return A \code{meta} dataframe for use in a \emph{ws_monitor} object.
#' @description After an OpenAQ dataframe has been enhanced with 
#' additional columns including \code{'datetime'}, \code{'stateCode'}, \code{'monitorID'} we are ready to 
#' pull out site information associated with unique monitorID.
#' 
#' These will be rearranged into a dataframe organized as deployment-by-property
#' with one row for each monitorID.
#'
#' This site information found in \code{df} is augmented so that we end up with a uniform
#' set of properties associated with each monitorID. The list of
#' columns in the returned \code{meta} dataframe is:
#' 
#' \preformatted{
#' > names(meta)
#' [1] "siteName"    "latitude"    "longitude"   "timezone"    "countryCode" "stateCode"   "monitorID"
#' }


openaq_createMetaDataframes <- function(df, parameters=NULL){
  
  # Sanity check -- df must have a monitorID
  if ( !'monitorID' %in% names(df) ) {
    logger.error("No 'monitorID' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'monitorID' column found in 'df' dataframe."))
  }
  
  # Fix bad locations
  badLocationMask <- df$longitude == 0 & df$latitude == 0
  badLocationIDs <- paste( unique( df$monitorID[badLocationMask] ), collapse=", ")
  logger.debug("Replacing 0,0 locations with NA,NA for IDs: %s", badLocationIDs)
  df$longitude[badLocationMask] <- NA
  df$latitude[badLocationMask] <- NA
  
  # Pull out unique monitors
  dfUnique <- df[!duplicated(df$monitorID),]
  logger.debug("Dataframe contains %d unique monitorID(s)", nrow(dfUnique))
  
  dfUnique$timezone <- suppressMessages( MazamaSpatialUtils::getTimezone(dfUnique$longitude, dfUnique$latitude, useBuffering = T) )
  df <- dplyr::left_join(df, dfUnique[,c("monitorID", "timezone")], by="monitorID")
    
  metaDF <- data.frame(matrix(nrow = nrow(df), ncol = 8))

  names(metaDF) <- c('siteName', 'latitude', 'longitude', 'timezone',
                   'countryCode', 'stateCode', 'monitorID', 'parameter')

  for (name in names(metaDF)[-1]) {
    metaDF[[name]] <- df[[name]]
  }
  
  metaDF$siteName <- df$location
  
  logger.info("Created 'meta' dataframe with %d rows and %d columns", nrow(metaDF), ncol(metaDF))
  
  # ----- Data Reshaping ------------------------------------------------------
  
  logger.debug("Reshaping OpenAQ sites metadata ...")
  
  # Get a list of parameters
  if ( is.null(parameters) ) {
    parameters <- sort(unique(df$parameter))
  } else {
    # Guarantee that passed in parameters actually exist
    parameters <- dplyr::intersect(parameters, unique(df$parameter))
    invalidParameters <- dplyr::setdiff(parameters, unique(df$parameter))
    if ( length(invalidParameters) > 0 ) {
      logger.warn("Requested parameters not found in OpenAQ sites metadata: %s", paste0(invalidParameters, collapse=", "))
    }
  }
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
  
  # Use dplyr to seprate the data by parameter
  for (parameter in parameters) {
    
    # Create datetime variable
    indexes <- which(metaDF$parameter == parameter)
    subDF <- metaDF[indexes,]
    
    # Guarantee unique rows
    subDF <- dplyr::distinct(subDF)
    
    # check if there exist two or more identical monitors that have disagreed lat/lon
    # if exist, take the average of lat/lon
    repIDs <- unique( subDF$monitorID[ duplicated(subDF$monitorID) ] )
    
    if ( length(repIDs) >0 ) {
      
      logger.debug("There are %i monitor(s)' lat/lon will be averaged", length(repIDs))
      
      for (id in repIDs ) {
        
        indexes <- which(subDF$monitorID == id)
        
        lat <- mean( subDF$latitude[indexes] )
        lon <- mean( subDF$longitude[indexes] )
        subDF$longitude[indexes] <- lat
        subDF$longitude[indexes] <- lon
        
        subDF <- subDF[ - unique( c(indexes[2], indexes[ length(indexes) ] ) ), ]
        
      }
      
    }

    # Add rownames as per ws_monitor convention
    rownames(subDF) <- subDF$monitorID
    subDF$parameter <- NULL
    dfList[[parameter]] <- subDF
    
  }
  
  return(dfList)
}
