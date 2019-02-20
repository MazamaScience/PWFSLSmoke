#' @keywords internal
#' @export
#' @import MazamaCoreUtils
#' @importFrom utils installed.packages
#' @title Add Elevation Data to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @description USGS APIs are used to determine elevation associated with the locations specified by the
#' \code{lonVar} and \code{latVar} columns of the incoming dataframe.
#' @return Input dataframe with (possibly) additional column: \code{elevation}.
#' @references \url{https://nationalmap.gov/epqs/}

addUSGSElevation <- function(df, lonVar="longitude", latVar="latitude", existingMeta=NULL) {

  logger.debug(" ----- addUSGSElevation() ----- ")

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

  lons <- df[[lonVar]]
  lats <- df[[latVar]]

  # ----- Add elevation data (meters) from USGS API ---------------------------

  if ( !is.null(existingMeta) ) {

    # NOTE:  If existingMeta is passed in, assume we are in an operational environment where we want to minimize web service calls.

    # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
    df <- as.data.frame(df, stringsAsFactors=FALSE)

    for (i in 1:nrow(df)) {
      monitorID <- df[i,'monitorID']
      if ( monitorID %in% existingMeta$monitorID ) {
        df$elevation[i] <- existingMeta[monitorID,'elevation']
      } else {
        df$elevation[i] <- as.numeric(NA)
      }
    }

  } else {

    # NOTE:  No existingMeta so go ahead and query the USGS elevation service

    # https://nationalmap.gov/epqs/pqs.php?x=-123.4&y=47.24&units=Meters&output=json

    logger.debug("Getting USGS elevation data for %s location(s)", nrow(df))

    # Create url
    url <- httr::parse_url("https://nationalmap.gov/epqs/pqs.php")

    for ( i in 1:nrow(df) ) {

      lon <- lons[i]
      lat <- lats[i]

      url$query <- list(x=lon,
                        y=lat,
                        units='Meters',
                        output='json')

      # Get and parse the return
      r <- httr::GET(httr::build_url(url))
      if ( httr::http_error(r) ) {

        logger.warn("USGS elevation service failed for URL %s", httr::build_url(url))
        df$elevation[i] <- as.numeric(NA)

      } else {

        returnObj <- httr::content(r)
        if ( !is.null(returnObj$USGS_Elevation_Point_Query_Service$Elevation_Query) ) {
          eq <- returnObj$USGS_Elevation_Point_Query_Service$Elevation_Query
          df$elevation[i] <- ifelse(eq$Elevation < -999999, 0, eq$Elevation) # See https://nationalmap.gov/epqs/
          # TODO:  If we were being careful we would check the returned x,y
          # TODO:  to see how much they differ from the requested lon,lat
          # TODO:  Initial tests show the results to be pretty good.
        } else {}

      }

    }

  } # end of !is.null(existingMetadata)

  return(df)

}
