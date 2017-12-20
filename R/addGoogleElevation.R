#' @keywords internal
#' @export
#' @importFrom utils installed.packages
#' @title Add Elevation Data to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} those created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @description Google APIs are used to determine elevation associated with the locations specified by the
#' \code{lonVar} and \code{latVar} columns of the incoming dataframe.
#' 
#' Address information is obtained by using the \pkg{ggmap} package.
#' @return Input dataframe with (possibly) additional column: \code{elevation}.
#' @references \url{https://developers.google.com/maps/documentation/elevation/intro}

addGoogleElevation <- function(df, lonVar="longitude", latVar="latitude", existingMeta=NULL) {
  
  logger.debug(" ----- addGoogleElevation() ----- ")
  
  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }
  
  # Initialize the elevation column if it doesn't exist
  if ( is.null(df$elevation) ) df$elevation <- as.numeric(NA)
  
  # ----- Add elevation data (meters) from Google API ---------------------------
  
  if ( !is.null(existingMeta) ) {
    
    # NOTE:  If existingMeta is passed in, assume we are in an operational environment where we want to minimize web service calls.
    
    for (i in 1:nrow(df)) {
      monitorID <- df[i,'monitorID']
      if ( monitorID %in% existingMeta$monitorID ) {
        df$elevation[i] <- round(existingMeta[monitorID,'elevation'], 0) # round to whole meters
      } else {
        df$elevation[i] <- as.numeric(NA)
      }
    }
    
  } else {
    
    # NOTE:  No existingMeta so go ahead and query the google elevation service
    
    logger.debug("Getting Google elevation data for %s location(s)", nrow(df))
    
    # NOTE:  Check and loop to see if we need to use addGoogleMetadata multiple 
    # NOTE:  times due to google's per-query limit
    loopCount <- nrow(df) %/% 300
    if ( nrow(df) %% 300 > 0) {
      loopCount <- loopCount + 1
    }
    
    dfList <- list()
    for (i in 1:loopCount) {
      
      startIndex <- (i-1) * 300 + 1
      if (i != loopCount) {
        endIndex <- i * 300
      } else {
        endIndex <- nrow(df)
      }
      
      dfSub <- df[startIndex:endIndex,]
      
      # Create url
      urlBase <- 'https://maps.googleapis.com/maps/api/elevation/json?locations='
      locations <- paste(dfSub[[latVar]], dfSub[[lonVar]], sep=',', collapse='|')
      url <- paste0(urlBase, locations)
      
      # NOTE:  For now (2017-08-15) we aren't hitting Google limits because this service
      # NOTE:  accepts a vector of locations in a single web service call.
      
      # Get and parse the return
      r <- httr::GET(url)
      if ( httr::http_error(r) ) {
        logger.error("Google elevation service failed with: %s", httr::content(r))
        logger.error("Google elevation service failed for URL: %s", url)
        stop(paste0("Google elevation service failed with: ",httr::content(r)))
      }
      
      returnObj <- httr::content(r)
      
      # Check results
      if ( returnObj$status != 'OK' ) {
        
        logger.warn("Google status was %s", returnObj$status)
        df$elevation <- as.numeric(NA)
        
      } else {
        
        # Convert list of lists to list of dataframes
        tempList <- lapply(returnObj$results, as.data.frame, stringsAsFactors=FALSE)
        # Combine individual dataframes
        elevationDF <- dplyr::bind_rows(tempList)
        
        # Sanity check that things came back in the same order
        if ( !all(dfSub[[latVar]] == elevationDF$location.lat) || !all(dfSub[[lonVar]] == elevationDF$location.lon) ) {
          logger.warn("Something is wrong with station elevation ordering")
          dfSub$elevation <- as.numeric(NA)
        } else {
          dfSub$elevation <- round(elevationDF$elevation, 0) # Round to whole meters
        }
        
      }
      
      dfList[[i]] <- dfSub
      
    }
    
    amendedDF <- dplyr::bind_rows(dfList)
    
  } # end of !is.null(existingMetadata)
  
  return(amendedDF)
  
}
