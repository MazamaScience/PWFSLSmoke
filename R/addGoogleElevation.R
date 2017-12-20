#' @keywords internal
#' @export
#' @importFrom utils installed.packages
#' @title Add Elevation Data to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
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
  df <- as.data.frame(df)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }
  
  # Initialize the elevation column if it doesn't exist
  if ( is.null(df$elevation) ) df$elevation <- as.numeric(NA)
  
  lons = df[[lonVar]]
  lats = df[[latVar]]
  
  # ----- Add elevation data (meters) from Google API ---------------------------
  
  if ( !is.null(existingMeta) ) {
    
    # NOTE:  If existingMeta is passed in, assume we are in an operational environment where we want to minimize web service calls.
    
    # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
    df <- as.data.frame(df)
    
    for (i in 1:nrow(df)) {
      monitorID <- df[i,'monitorID']
      if ( monitorID %in% existingMeta$monitorID ) {
        df$elevation[i] <- existingMeta[monitorID,'elevation']
      } else {
        df$elevation[i] <- as.numeric(NA)
      }
    }
    
  } else {
    
    # NOTE:  No existingMeta so go ahead and query the google elevation service
    
    logger.debug("Getting Google elevation data for %s location(s)", nrow(df))
    
    # Create url
    urlBase <- 'https://maps.googleapis.com/maps/api/elevation/json?locations='
    locations <- paste(lats, lons, sep=',', collapse='|')
    url <- paste0(urlBase, locations)
    
    # NOTE:  For now (2017-08-15) we aren't hitting Google limits because this service
    # NOTE:  accepts a vector of locations in a single web service call.
    
    # Get and parse the return
    r <- httr::GET(url)
    if ( httr::http_error(r) ) {
      stop(paste0("Google elevation service failed with: ",httr::content(r)))
    }
    
    returnObj <- httr::content(r)
    
    # Check results
    if ( returnObj$status != 'OK' ) {
      
      logger.warn("Google status was %s for URL %s", returnObj$status, url)
      df$elevation <- as.numeric(NA)
      
    } else {
      
      # Convert list of lists to list of dataframes
      tempList <- lapply(returnObj$results,as.data.frame)
      # Combine individual dataframes
      elevationDF <- dplyr::bind_rows(tempList)
      
      # Sanity check that things came back in the same order
      if ( !all(df[[latVar]] == elevationDF$location.lat) || !all(df[[lonVar]] == elevationDF$location.lon) ) {
        logger.error("Something is wrong with station elevation ordering")
        df$elevation <- as.numeric(NA)
      } else {
        df$elevation <- elevationDF$elevation
      }
      
    }
    
  } # end of !is.null(existingMetadata)
  
  return(df)
  
}
