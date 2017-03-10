#' @keywords EPA
#' @export
#' @title Create Sites Metadata Dataframe
#' @param df an EPA raw dataframe after metadata enhancement
#' @description After addtional columns(i.e. \code{datetime}, and \code{monitorID}) 
#' have been applied to an EPA dataframe, we are ready to 
#' pull out site information associated with unique monitorID.
#' 
#' These will be rearranged into a dataframe organized as deployment-by-property
#' with one row for each monitorID.
#'
#' This site information found in \code{df} is augmented so that we end up with a uniform
#' set of properties associated with each monitorID. The list of
#' columns in the returned \code{meta} dataframe is:
#' 
#' \preformatted{
#' > names(meta)
#' [1] "monitorID"        "siteName"         "latitude"         "longitude"        "elevation"        "timezone"         "stateCode"       
#' [8] "Site.Num"         "Parameter.Code"   "POC"              "Units.of.Measure" "MDL"              "Method.Type"      "Method.Name"     
#' [15] "State.Name"       "County.Name" 
#' }
#' @return A \code{meta} dataframe for use in a \emph{ws_monitor} object.

epa_createMetaDataframe <- function(df){
  
  logger.debug(paste0('Creating meta dataframe ...'))
  
  # Subset df to include only site-specific columns
  columns <- c('Site Num','Parameter Code','POC','Latitude','Longitude','Units of Measure','MDL',
               'Method Type','Method Name','State Name','County Name','monitorID')
  dfSub <- df[,columns]
  
  # Create a new dataframe containing a single row for each monitorID
  uniqueMonitorIDMask <- !duplicated(dfSub$monitorID)
  meta <- dfSub[uniqueMonitorIDMask,]
  
  # Add required columns:  latitude, longitude, elevation, monitorID, timezone
  meta$latitude <- meta$Latitude
  meta$longitude <- meta$Longitude
  
  # check and loop to see if we need to use addGoogleMetadata multiple times due to google's limit
  loopCount <- nrow(meta) %/% 300
  if ( nrow(meta) %% 300 > 0) {
    loopCount <- loopCount + 1
  }
  
  logger.debug("Adding Google metadata ...")
  metaList <- list()
  for (i in 1:loopCount) {
    startIndex <- (i-1) * 300 + 1
    if (i != loopCount) {
      endIndex <- i * 300
    } else {
      endIndex <- nrow(meta)
    }
    metaList[[i]] <- addGoogleMetadata(meta[startIndex:endIndex,])
  }
  meta <- dplyr::bind_rows(metaList)
  
  logger.debug("Adding Mazama metadata ...")
  meta <- addMazamaMetadata(meta, countryCodes = 'US')

  # Create a vector of column names to be included in meta in the order of their inclusion
  columns <- c('monitorID','siteName','latitude','longitude','elevation','timezone','stateCode','Site Num','Parameter Code',
               'POC','Units of Measure','MDL','Method Type','Method Name','State Name','County Name')
  meta <- meta[,columns]

  # Make end users lives easier by using normal R names for the columns. (No more "meta$`Parameter Code`")
  names(meta) <- make.names(names(meta))
  
  # Remove the "tbl_df" class
  meta <- as.data.frame(meta)
  
  return(meta)
}
