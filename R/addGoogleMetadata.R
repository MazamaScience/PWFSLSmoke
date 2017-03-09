#' @keywords internal
#' @export
#' @importFrom utils installed.packages
#' @title Add Elevation and Address Information to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @description Google APIs are used to determine elevation and
#' address information associated with the locations specified by the
#' \code{longitude} and \code{latitude} columns of the incoming dataframe.
#' 
#' Address information is obtained by using the \pkg{ggmap} package.
#' @return Input dataframe with additional columns: \code{elevation, siteName, countyName}.
#' @references \url{https://developers.google.com/maps/documentation/elevation/intro}

addGoogleMetadata <- function(df, lonVar="longitude", latVar="latitude") {
  
  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }
  
  lons = df[[lonVar]]
  lats = df[[latVar]]
  
  # ----- Add elevation data (meters) from Google API ---------------------------
  
  # Create url
  urlBase <- 'https://maps.googleapis.com/maps/api/elevation/json?locations='
  locations <- paste(lats, lons, sep=',', collapse='|')
  url <- paste0(urlBase, locations)
  
  logger.debug("Getting Google elevation data for %s location(s)", nrow(df))
  
  # Get and parse the return which has elements 'results' and 'status'
  googleReturn <- httr::content(httr::GET(url))
  
  # Check results
  if ( googleReturn$status != 'OK' ) {
    
    logger.warn("Google status was %s for URL %s", googleReturn$status, url)
    df$elevation <- as.numeric(NA)
    
  } else {
    
    # Convert list of lists to list of dataframes
    tempList <- lapply(googleReturn$results,as.data.frame)
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
  
  # ----- Add siteName from Google API ---------------------------
  
  logger.debug("Getting Google address data for %s location(s)", nrow(df))
  
  # When siteName is missing, create one similar to AirNow with "locality-route"
  
  if ( !('siteName' %in% names(df)) ) df$siteName <- as.character(NA)
  if ( !('coutyName' %in% names(df)) ) df$countyName <- as.character(NA)
  
  # Use ggmap::revgeocode to return a dataframe with (address, street number, route, locality , ...)
  # (2500 queries allowed per day in August, 2015)
  if ('ggmap' %in% installed.packages()[,1]) {
    
    for (i in 1:nrow(df)) {
      
      if ( is.na(df[i,'siteName']) ) {
        
        location <- c(df[i,lonVar],df[i,latVar])
        logger.trace("\tgoogle address request for location = %s, %s", location[1], location[2])
        if (!anyNA(location)) {
          address <- suppressMessages( ggmap::revgeocode(location, output='more') )
          # NOTE:  revgeocode can fail if you have too may Google requests in a day
          result <- try( df$siteName[i] <- paste(address$locality,address$route,sep='-'),
                         silent=TRUE ) # don't show errors
          if ( "try-error" %in% class(result) ) {
            logger.warn("Google geocoding may have failed. Have you goin over the 2,500/day limit? (2017)")
          }
          # NOTE:  administrative_area_level_2 is not always present
          try( df$countyName[i] <- stringr::str_replace(address$administrative_area_level_2,' County',''),
               silent=TRUE ) # don't show errors
        }
        
      }
      
    }
    
  }
  
  return(df)
  
}
