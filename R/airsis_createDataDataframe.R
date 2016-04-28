#' @keywords AIRSIS
#' @export
#' @title Create Data Dataframe
#' @param df single site AIRSIS dataframe created by airsis_clustering()
#' @param meta AIRSIS meta dataframe created by airsis_createMetaDataframe()
#' @param pm25Var name of PM 2.5 variable in the incoming dataframe
#' @param verbose logical flag to generate verbose output
#' @description Explain this function
#' 
#' @return ws_monitor object with a unique `monitorID` for each unique location


airsis_createDataDataframe <- function(df, meta, pm25Var='ConcHr', verbose=FALSE) {
  
  # Sanity check -- df must have deploymentID
  if ( !'deploymentID' %in% names(df) ) stop(paste0("The 'df' dataframe does not have a 'deploymentID' column.  Have you run airsis_clustering()?"))

  # Add monitorID to the dataframe
  df$monitorID <- meta$monitorID[df$deploymentID]
  
  # Create minimal subset with the the variables we need for rows, columns and data
  subDF <- df[,c('datetime','monitorID',pm25Var)]
  melted <- reshape2::melt(subDF, id.vars=c('datetime','monitorID'), measure.vars=pm25Var)
  
  # Convert to UG/M3
  melted$value <- melted$value * 1000
  
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
  # NOTE:  dplyr returns objects of class "tbl_df" which can be confusing. We undo that.
  data <- as.data.frame( dplyr::left_join(hourlyDF, pm25DF, by='datetime') )
  rownames(data) <- format(data$datetime,"%Y%m%d%H",tz="GMT")
  
  return(as.data.frame(data))
  
}
