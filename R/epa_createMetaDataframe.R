#' @keywords ws_monitor
#' @export
#' @title Create Sites Metadata Dataframe
#' @param df an EPA dataframe after metadata enhancement
#' @description After an EPA dataframe has been enhanced with 
#' additional columns including \code{'datetime'},\code{'monitorID'} we are ready to 
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
#' [1] "monitorID"        "siteName"         "latitude"         "longitude"        "elevation"        "timezone"         "stateCode"       
#' [8] "Site.Num"         "Parameter.Code"   "POC"              "Units.of.Measure" "MDL"              "Method.Type"      "Method.Name"     
#' [15] "State.Name"       "County.Name" 
#' 
#' @return A \code{'meta'} dataframe for use in a \code{ws_monitor} object.

epa_createMetaDataframe <- function(df, verbose){
  
  if (verbose) cat(paste0('   Creating meta dataframe ...\n'))
  
  # The metadata file will strictly involve the data that doesn't involve the parameterNames.
  
  # Create a vector of column names. Create vector from df by inputing default x values and columns as the y values.
  columns <- c('Site Num','Parameter Code','POC','Latitude','Longitude','Units of Measure','MDL',
               'Method Type','Method Name','State Name','County Name','monitorID')
  dfSub <- df[,columns]
  
  # Create a new dataframe containing a single row for each monitorID
  # Create a mask that limits multiple readings from a single instruments with the !dupplicated() command.
  uniqueMonitorIDMask <- !duplicated(dfSub$monitorID)
  meta <- dfSub[uniqueMonitorIDMask,]
  
  # Add common metadata columns shared among all PM2.5 datasets
  # latitude, longitude, elevation, monitorID, timezone, has_pm25
  meta$latitude <- meta$Latitude
  meta$longitude <- meta$Longitude
  
  meta <- MazamaSpatialUtils::addGoogleMetadata(meta)
  meta <- MazamaSpatialUtils::addMazamaMetadata(meta)
  meta$countyName <- NULL

  # Add timezones only if the MazamaSpatialUtils package exists. This way we can compile and load this
  # package even if the MazamaSpatialUtils package is not present.
  
  # if ( requireNamespace('MazamaSpatialUtils', quietly = TRUE) ) {
  #   
  #   dummy <- getSpatialDataDir()
  #   if (verbose) cat(paste0('   Determining timezones and stateCodes...\n')) 
  #   
  #   meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude, useBuffering=TRUE)
  #   
  #   meta$stateCode <- MazamaSpatialUtils::getStateCode(meta$longitude, meta$latitude, countryCodes=c('CA','US','MX'), useBuffering=TRUE)
  #   
  # }
  
  
  # Create a vector of column names to be included in meta in the order of their inclusion
  columns <- c('monitorID','siteName','latitude','longitude','elevation','timezone','stateCode','Site Num','Parameter Code',
               'POC','Units of Measure','MDL','Method Type','Method Name','State Name','County Name')
  meta <- meta[,columns]
  rownames(meta) <- meta$monitorID
  
  # Make end users lives easier by using normal R names for the columns. (No more "meta$`Parameter Code`")
  names(meta) <- make.names(names(meta))
  
  # Remove the "tbl_df" class
  meta <- as.data.frame(meta)
  
  return(meta)
}