#' @keywords ws_monitor
#' @export
#' @title Create Sites Metadata Dataframe
#' @param df an EPA dataframe after metadata enhancement
#' @description After an EPA dataframe has been enhanced with 
#' additional columns including \code{datetime},\code{monitorID} we are ready to 
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
#' }
#' @return A \code{'meta'} dataframe for use in a \code{ws_monitor} object.

epa_createMetaDataframe <- function(df){
  
  logger.debug(paste0('   Creating meta dataframe ...\n'))
  
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
  
  # check and loop to see if we need to use addGoogleMetadata multiple times due to google's limit
  loopNum <- nrow(meta) %/% 300
  if ( nrow(meta) %% 300 > 0) {
    loopNum <- loopNum + 1
  }
  
  # logger.debug("Using addGoogleMetaData to get elevation, siteName, and countyName for 'meta'.")
  for (i in 1:loopNum) {
    startIndex <- (i-1) * 300 + 1
    if (i != loopNum) {
      endIndex <- i * 300
    } else {
      endIndex <- nrow(meta)
    }
    metaSub <- addGoogleMetadata(meta[startIndex:endIndex,])
    metaName <- paste("meta",i, sep="")
    assign(metaName, metaSub)
  }
  # logger.debug("Finished appending elevation, siteName and countyName to 'meta'.")
  
  meta <- get(paste("meta", 1, sep=""))
  
  # combine all the sub-meta's together if any
  if (loopNum > 1) {
    for (i in 2:loopNum) {
      meta <- rbind(meta, get(paste("meta", i, sep="")))
    }
  }
  
  logger.debug("Using addMazamaMetaData to get timezone, countryCode and stateCode.")
  meta <- addMazamaMetadata(meta, countryCodes = 'US')
  logger.debug("Finished appending timezone, countryCode and stateCode columns to the dataframe")

  # Create a vector of column names to be included in meta in the order of their inclusion
  columns <- c('monitorID','siteName','latitude','longitude','elevation','timezone','stateCode','Site Num','Parameter Code',
               'POC','Units of Measure','MDL','Method Type','Method Name','State Name','County Name')
  meta <- meta[,columns]
  # rownames(meta) <- meta$monitorID
  
  # Make end users lives easier by using normal R names for the columns. (No more "meta$`Parameter Code`")
  names(meta) <- make.names(names(meta))
  
  # Remove the "tbl_df" class
  meta <- as.data.frame(meta)
  
  return(meta)
}