#' @keywords OpenAQ
#' @export
#' @title Create Sites Metadata Dataframe
#' @param df a OpenAQ dataframe after metadata enhancement
#' @description After an OpenAQ dataframe has been enhanced with 
#' additional columns including \code{'datetime'}, \code{'stateCode'}, \code{'monitorID'} we are ready to 
#' pull out site information associated with unique monitorID.
#' 
#' These will be rearranged into a dataframe organized as site-by-property
#' with one row for each monitorID.
#'
#' This site information found in \code{df} is augmented so that we end up with a uniform
#' set of properties associated with each monitorID. The list of
#' columns in the returned \code{'meta'} dataframe is:
#' 
#' \preformatted{
#' > names(meta)
#' [1] "siteName"    "latitude"    "longitude"   "timezone"    "countryCode" "stateCode"   "monitorID"
#' }
#' 
#' @return A \code{'meta'} dataframe for use in a \code{ws_monitor} object.


openaq_createMetaDataframe <- function(df){
  
  # Sanity check -- df must have a monitorID
  if ( !'monitorID' %in% names(df) ) {
    logger.error("No 'monitorID' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'monitorID' column found in 'df' dataframe."))
  }
  
  df <- df[!duplicated(df$monitorID),]
  
  logger.debug('Dataframe contains %d unique monitorID(s)', nrow(df))
  
  meta <- data.frame(matrix(nrow = nrow(df), ncol = 7), row.names = df$monitorID)
  
  names(meta) <- c('siteName', 'latitude', 'longitude', 'timezone',
                'countryCode', 'stateCode', 'monitorID')
  
  for (name in names(meta)[-c(1,4,5)]) {
    meta[[name]] <- df[[name]]
  }
  meta$siteName <- df$location
  meta$countryCode <- 'US'
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude,meta$latitude,useBuffering = T)

  logger.debug("'meta' dataframe has %d rows and %d columns", nrow(meta), ncol(meta))
  
  return(meta)
}
