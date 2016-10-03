#' @keywords WRCC
#' @export
#' @importFrom stats median
#' @title Create Data Dataframe
#' @param df single site WRCC dataframe created by wrcc_clustering()
#' @param meta WRCC meta dataframe created by wrcc_createMetaDataframe()
#' @param verbose logical flag to generate verbose output
#' @description Explain this function
#' 
#' @return ws_monitor object with a unique `monitorID` for each unique location


wrcc_createDataDataframe <- function(df, meta, verbose=FALSE) {
  
  # Sanity check -- df must have deploymentID
  if ( !'deploymentID' %in% names(df) ) stop(paste0("The 'df' dataframe does not have a 'deploymentID' column.  Have you run wrcc_clustering()?"))

  # Add monitorID to the dataframe
  df$monitorID <- meta$monitorID[df$deploymentID]
  
  # Create minimal subset with the the variables we need for rows, columns and data
  subDF <- df[,c('datetime','monitorID','Conc.RT')]
  melted <- reshape2::melt(subDF, id.vars=c('datetime','monitorID'), measure.vars='Conc.RT')
  
  # Use median if multiple values are found
  # NOTE:  The resulting dataframe is [datetime,monitorID] with an extra first column containing datetime
  pm25DF <- reshape2::dcast(melted, datetime ~ monitorID, median)
  colnames(pm25DF) <- c('datetime',meta$monitorID)
  rownames(pm25DF) <- format(pm25DF$datetime,"%Y%m%d%H",tz="GMT")
  
  # Sanity check -- only one pm25DF measure per hour
  valueCountPerCell <- reshape2::dcast(melted, datetime ~ monitorID, length)
  maxCount <- max(valueCountPerCell[,-1])
  if (maxCount > 1) stop(paste0('Up to ',maxCount,' pm25DF measurements per hour.'))
 
  # Create an empty hourlyDF dataframe with a full time axis (no missing hours)
  datetime <- seq(min(df$datetime), max(df$datetime), by="hours")
  hourlyDF <- data.frame(datetime=datetime)

  # Merge pm25DF into the houlyDF dataframe, inserting NA's where necessary
  data <- dplyr::left_join(hourlyDF, pm25DF, by='datetime')
  rownames(data) <- format(data$datetime,"%Y%m%d%H",tz="GMT")
  
  return(data)
  
}
