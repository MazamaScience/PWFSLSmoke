#' @keywords internal
#' @export
#' @title Add State, Country and Timezone to a Dataframe
#' @param df dataframe or tibble with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @param countryCodes vector of countryCodes (use \code{NULL} for global searches)
#' @return Input dataframe with additional columns: \code{timezone, countryCode, stateCode}.
#' @description The \pkg{MazamaSpatialUtils} package used to determine the ISO state and country code,
#' and the Olson timezone associated with the locations specified by the
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
#' An error will be generated if these lines have not be previously run.
#' @references \url{https://github.com/MazamaScience/MazamaSpatialUtils}

addMazamaMetadata <- function(df, lonVar="longitude", latVar="latitude", existingMeta=NULL, countryCodes=c('CA','US','MX')) {
  
  # NOTE:  existingMeta is not currently used but is retained as an argument to mimic the signature of addGoogleElevation()
  
  logger.debug(" ----- addMazamaMetadata() ----- ")
  
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
