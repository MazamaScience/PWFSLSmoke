#' @keywords internal
#' @export
#' @title Add Elevation and Address Information to a Dataframe
#' @param df dataframe with geolocation information (e.g. created by wrcc_qualityControl() or airsis_qualityControl)
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param countryCodes vector of countryCodes
#' @description The \pkg{MazamaSpatialUtils} package used to determine elevation and
#' address information associated with the locations specified by the
#' \code{longitude} and \code{latitude} columns of the incoming dataframe.
#' 
#' This function requires previous setup of the \pkg{MazamaSpatialUtils} package:
#' 
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir('~/Data/Spatial')
#' loadSpatialData('NaturalEarthAdm1')
#' }
#' 
#' If these lines have not be previously run, the function will return the original,
#' unmodified dataframe.
#' @return Input dataframe with additional columns: timezone, countryCode, stateCode.
#' @references \url{https://github.com/MazamaScience/MazamaSpatialUtils}

addMazamaMetadata <- function(df, lonVar="longitude", latVar="latitude", countryCodes=NULL) {
  
  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }
  
  lons = df[[lonVar]]
  lats = df[[latVar]]
  
  if ( exists('NaturalEarthAdm1') ) {
    
    logger.debug("Getting Mazama spatial data for %s location(s)", nrow(df))
    df$timezone <- MazamaSpatialUtils::getTimezone(lons, lats, countryCodes=countryCodes, useBuffering=TRUE)
    df$countryCode <- MazamaSpatialUtils::getCountryCode(lons, lats, countryCodes=countryCodes, useBuffering=TRUE)
    df$stateCode <- MazamaSpatialUtils::getStateCode(lons, lats, countryCodes=countryCodes, useBuffering=TRUE)
    
  } else {
    
    # NOTE:  Timezone, countryCode and stateCode information is mandatory for ws_monitor objects.
    logger.error("MazamaSpatialUtils package was not properly initialized -- no Mazama metadata added")
    stop(paste0("MazamaSpatialUtils package was not properly initialized. Please run:\n",
                "  library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1')"))
    
  }
  
  return(df)
  
}
