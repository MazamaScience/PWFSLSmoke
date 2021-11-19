#' @keywords internal
#' @export
#' @import MazamaCoreUtils
#'
#' @title Add address information to a dataframe
#'
#' @param df dataframe with geolocation information (\emph{e.g.} those created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @description Google APIs are used to determine
#' address information associated with the locations specified by the
#' \code{longitude} and \code{latitude} columns of the incoming dataframe.
#'
#' Address information is obtained by using the \pkg{ggmap} package.
#' @return Input dataframe with additional columns: \code{siteName, countyName}.
#' @references \url{https://developers.google.com/maps/documentation/geocoding/overview}

addGoogleAddress <- function(
  df,
  lonVar = "longitude",
  latVar = "latitude",
  existingMeta = NULL
) {

  ###logger.debug(" ----- addGoogleAddress() ----- ")

  message("addGoogleAddress() is currently disabled after discontinuation of free querys in July, 2018")

  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }

  # Initialize siteName and countyName columns
  if ( is.null(df$siteName) ) df$siteName <- as.character(NA)
  if ( is.null(df$countyName) ) df$countyName <- as.character(NA)

  lons <- df[[lonVar]]
  lats <- df[[latVar]]

  # ----- Add siteName from Google API -----------------------------------------

  logger.trace("Getting site names for %s location(s)", nrow(df))

  # When siteName is missing, create one similar to AirNow with "locality-route"

  # Use ggmap::revgeocode to return a dataframe with (address, street number, route, locality , ...)
  # (2500 queries allowed per day in August, 2015)
  if ( length(find.package("ggmap", quiet = TRUE)) > 0 ) {

    for ( i in seq_len(nrow(df)) ) {

      # NOTE:  monitorID for AIRSIS and WRCC contains location information and will always
      # NOTE:  be associated with a unique siteName. Reusing metadata will dramatically
      # NOTE:  decrease the number of Google API requests we make and will prevent null
      # NOTE:  responses when we are over our 2500 free requests.

      # Check for existing metadata for this monitorID
      metadataExists <- FALSE
      monitorID <- df[i,'monitorID']
      if ( !is.null(existingMeta) ) {
        if ( monitorID %in% existingMeta$monitorID ) {
          if ( !is.na(existingMeta[monitorID,'siteName']) && existingMeta[monitorID,'siteName'] != "" ) {
            metadataExists <- TRUE
          }
        }
      }

      if ( metadataExists ) {

        # Use existing siteName and countyName if the already exist
        logger.trace("\tusing existing metadata for %s", monitorID)
        df$siteName[i] <- existingMeta[monitorID,'siteName']
        df$countyName[i] <- existingMeta[monitorID, 'countyName']

      } else {

        # Query Google for siteName and countyName
        location <- c(df[i,lonVar],df[i,latVar])
        logger.trace("\tgoogle address request for location = %s, %s", location[1], location[2])
        if ( !anyNA(location) ) {
          address <- suppressMessages( ggmap::revgeocode(location, output = 'more') )
          # NOTE:  revgeocode can fail if you have too may Google requests in a day
          result <- try( df$siteName[i] <- paste(address$locality,address$route,sep = '-'),
                         silent = TRUE ) # don't show errors
          if ( "try-error" %in% class(result) ) {
            logger.warn("Google geocoding may have failed. Have you gone over the 2.5K/day limit? (2017)")
          }
          # NOTE:  administrative_area_level_2 is not always present
          try( df$countyName[i] <- stringr::str_replace(address$administrative_area_level_2,' County',''),
               silent = TRUE ) # don't show errors
        }

      }

    }

  }

  return(df)

}
