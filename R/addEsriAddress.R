#' @keywords internal
#' @export
#' @import MazamaCoreUtils
#' @importFrom utils installed.packages
#' @title Add Address Information to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} those created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @description ESRI APIs are used to determine
#' address information associated with the locations specified by the
#' \code{longitude} and \code{latitude} columns of the incoming dataframe.
#' @return Input dataframe with additional columns: \code{siteName, countyName}.
#' @references \url{https://developers.arcgis.com/rest/geocode/api-reference/geocoding-reverse-geocode.htm}
#' @examples
#' \dontrun{
#' df <- data.frame(longitude=c(-121,-122,-123),latitude=c(42,43,44))
#' addEsriAddress(df)
#' }

addEsriAddress <- function(df, lonVar="longitude", latVar="latitude", existingMeta=NULL) {

  logger.trace(" ----- addEsriAddress() ----- ")

  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df, stringsAsFactors=FALSE)

  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }

  # Initialize siteName and countyName columns
  if ( is.null(df$siteName) ) df$siteName <- as.character(NA)
  if ( is.null(df$countyName) ) df$countyName <- as.character(NA)

  # ----- Add siteName from ESRI API ---------------------------

  # NOTE:  URL looks like:
  # NOTE:
  # NOTE:    https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?outSR=4326&returnIntersection=false&location=-121.06092453002931%2C47.255990221115475&f=json
  # NOTE:
  # NOTE:  Successful return looks like:
  # {
  #   "address": {
  #     "AddNum": "90",
  #     "Addr_type": "PointAddress",
  #     "Address": "90 Guzzie Ln",
  #     "Block": "",
  #     "City": "Ronald",
  #     "CountryCode": "USA",
  #     "District": "",
  #     "LongLabel": "90 Guzzie Ln, Ronald, WA, 98940, USA",
  #     "Match_addr": "90 Guzzie Ln, Ronald, Washington, 98940",
  #     "MetroArea": "",
  #     "Neighborhood": "",
  #     "PlaceName": "",
  #     "Postal": "98940",
  #     "PostalExt": "",
  #     "Region": "Washington",
  #     "Sector": "",
  #     "ShortLabel": "90 Guzzie Ln",
  #     "Subregion": "Kittitas County",
  #     "Territory": "",
  #     "Type": ""
  #   },
  #   "location": {
  #     "spatialReference": {
  #       "latestWkid": 4326,
  #       "wkid": 4326
  #     },
  #     "x": -121.06043450738626,
  #     "y": 47.2559559888915
  #   }
  # }

  logger.trace("Getting site names for %s location(s)", nrow(df))

  # When siteName is missing, create one similar to AirNow with "locality-route"

  for (i in 1:nrow(df)) {

    # NOTE:  monitorID for AIRSIS and WRCC contains location information and will always
    # NOTE:  be associated with a unique siteName. Reusing metadata will dramatically
    # NOTE:  decrease the number of API requests we make.

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

      # Use existing siteName and countyName if they already exist
      logger.trace("\tusing existing metadata for %s", monitorID)
      df$siteName[i] <- existingMeta[monitorID,'siteName']
      df$countyName[i] <- existingMeta[monitorID, 'countyName']

    } else {

      # Query ESRI for siteName and countyName
      location <- c(df[i,lonVar],df[i,latVar])
      logger.trace("\tESRI address request for location = %s, %s", location[1], location[2])
      if ( !anyNA(location) ) {
        # Always wrap any webservice reqeust
        result <- try({

          urlBase <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?"
          location <- paste0("location=",paste0(location[1],",",location[2]),"&")
          format <- "f=json"
          tokenString <- NULL
          if ( !is.null(getEsriToken()) ) {
            tokenString <- paste0("&token=", getEsriToken())
          }
          url <- paste0(urlBase, location, format, tokenString)


          # Get and parse the return
          r <- httr::GET(url)
          if ( httr::http_error(r) ) {
            logger.error("ESRI address service failed with: %s", httr::content(r))
            logger.error("ESRI address service failed for URL: %s", url)
            return(df)
          }

          returnObj <- httr::content(r)

          elements <- names(returnObj$address)

          # Create siteName similar to AirNow with "City-Address"
          if ( "City" %in% elements && "Address" %in% elements) {
            if ( returnObj$address$Address == "" ) {
              df$siteName[i] <- paste0(returnObj$address$City)
            } else {
              df$siteName[i] <- paste0(returnObj$address$City,"-",returnObj$address$Address)
            }
          } else if ( "City" %in% elements ) {
            df$siteName[i] <- paste0(returnObj$address$City)
          } else if ( "Address" %in% elements ) {
            df$siteName[i] <- paste0(returnObj$address$Address)
          }

          # Create county
          if ( "Subregion" %in% elements ) {
            df$countyName[i] <- stringr::str_replace(returnObj$address$Subregion,' County','')
          }

        }, silent=TRUE)
        if ( "try-error" %in% class(result) ) {
          logger.trace("\t%s", url)
          logger.warn("Unable to add ESRI address: %s", geterrmessage())
        }

      }

    }

  }





  return(df)

}
