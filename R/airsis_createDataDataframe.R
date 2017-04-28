#' @keywords AIRSIS
#' @export
#' @title Create AIRSIS Data Dataframe
#' @param df single site AIRSIS dataframe created by \code{airsis_clustering()}
#' @param meta AIRSIS meta dataframe created by \code{airsis_createMetaDataframe()}
#' @description After quality control has been applied to an AIRSIS dataframe,
#' we can extract the PM2.5 values and store them in a \code{data} dataframe
#' organized as time-by-deployment (aka time-by-site).
#' 
#' The first column of the returned dataframe is named \code{'datetime'} and
#' contains a \code{POSIXct} time in UTC. Additional columns contain data
#' for each separate deployment of a monitor. 
#' 
#' @return A \code{data} dataframe for use in a emph{ws_monitor} object.


airsis_createDataDataframe <- function(df, meta) {
  
  # Sanity check -- df must have deploymentID
  if ( !'deploymentID' %in% names(df) ) {
    logger.error("No 'deploymentID' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'deploymentID' column found in 'df' dataframe.  Have you run addClustering()?"))
  }

  # Sanity check -- df must have datetime
  if ( !'datetime' %in% names(df) ) {
    logger.error("No 'datetime' column found in 'df' dataframe with columns: %s", paste0(names(df), collapse=", "))
    stop(paste0("No 'datetime' column found in 'df' dataframe."))
  }
  
  # Sanity check -- meta must have a monitorType
  if ( !'monitorType' %in% names(meta) ) {
    logger.error("No 'monitorType' column found in 'meta' dataframe with columns: %s", paste0(names(meta), collapse=", "))
    stop(paste0("No 'monitorType' column found in 'meta' dataframe."))
  }
  
  monitorType <- unique(meta$monitorType)
  
  # Sanity check -- only a single monitorType is allowed
  if ( length(monitorType) > 1 ) {
    logger.error("Multiple monitor types found in 'meta' dataframe: %s", paste0(monitorType, collapse=", "))
    stop(paste0("Multiple monitor types found in 'meta' dataframe."))
  }
  
  # Add monitorID to the dataframe
  df$monitorID <- meta[df$deploymentID, 'monitorID']
  
  if ( monitorType == 'EBAM' ) {
    pm25Var <- 'ConcHr'
  } else if ( monitorType == 'ESAM' ) {
    pm25Var <- 'Conc.mg.m3.'
  } else {
    logger.error("Dataframe creation is not supported for %s", monitorType)
    stop(paste0("Dataframe creation is not supported for ", monitorType))
  }
  
  # Create minimal subset with the the variables we need for rows, columns and data
  subDF <- df[,c('datetime','monitorID',pm25Var)]
  melted <- reshape2::melt(subDF, id.vars=c('datetime','monitorID'), measure.vars=pm25Var)

  # Unit conversion as needed (mg/m3 ==> ug/m3)
  if ( monitorType == 'EBAM' ) melted$value <- melted$value * 1000
  if ( monitorType == 'ESAM' ) melted$value <- melted$value * 1000
  
  # Use median if multiple values are found
  
  # Sanity check -- only one pm25DF measure per hour
  valueCountPerCell <- reshape2::dcast(melted, datetime ~ monitorID, length)
  maxCount <- max(valueCountPerCell[,-1])
  if (maxCount > 1) logger.warn("Up to %s measurements per hour -- median used",maxCount)
  
  # NOTE:  The resulting dataframe is [datetime,monitorID] with an extra first column containing datetime
  pm25DF <- reshape2::dcast(melted, datetime ~ monitorID, stats::median)
  colnames(pm25DF) <- c('datetime',meta$monitorID)

  # Create an empty hourlyDF dataframe with a full time axis (no missing hours)
  datetime <- seq(min(df$datetime), max(df$datetime), by="hours")
  hourlyDF <- data.frame(datetime=datetime)

  # Merge pm25DF into the houlyDF dataframe, inserting NA's where necessary
  # NOTE:  dplyr returns objects of class "tbl_df" which can be confusing. We undo that.
  data <- as.data.frame( dplyr::left_join(hourlyDF, pm25DF, by='datetime') )

  logger.info("Created 'data' dataframe with %d rows and %d columns", nrow(data), ncol(data))
  
  return(as.data.frame(data))
  
}
