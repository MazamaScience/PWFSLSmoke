#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Create AIRSIS data dataframe
#'
#' @param tbl single site AIRSIS tibble created by \code{airsis_clustering()}
#' @param meta AIRSIS meta dataframe created by \code{airsis_createMetaDataframe()}
#' @description After quality control has been applied to an AIRSIS tibble,
#' we can extract the PM2.5 values and store them in a \code{data} dataframe
#' organized as time-by-deployment (aka time-by-site).
#'
#' The first column of the returned dataframe is named \code{'datetime'} and
#' contains a \code{POSIXct} time in UTC. Additional columns contain data
#' for each separate deployment of a monitor.
#'
#' @return A \code{data} dataframe for use in a \emph{ws_monitor} object.


airsis_createDataDataframe <- function(
  tbl,
  meta
) {

  logger.debug(" ----- airsis_createDataDataframe() ----- ")

  # ----- Validate parameters --------------------------------------------------

  # Sanity check -- tbl must have deploymentID
  if ( !'deploymentID' %in% names(tbl) ) {
    logger.error("No 'deploymentID' column found in 'tbl' tibble with columns: %s", paste0(names(tbl), collapse = ", "))
    stop(paste0("No 'deploymentID' column found in 'tbl' tibble.  Have you run addClustering()?"))
  }

  # Sanity check -- tbl must have datetime
  if ( !'datetime' %in% names(tbl) ) {
    logger.error("No 'datetime' column found in 'tbl' tibble with columns: %s", paste0(names(tbl), collapse = ", "))
    stop(paste0("No 'datetime' column found in 'tbl' tibble."))
  }

  # Sanity check -- meta must have a monitorType
  if ( !'monitorType' %in% names(meta) ) {
    logger.error("No 'monitorType' column found in 'meta' dataframe with columns: %s", paste0(names(meta), collapse = ", "))
    stop(paste0("No 'monitorType' column found in 'meta' dataframe."))
  }

  monitorType <- unique(meta$monitorType)
  monitorSubtype <- unique(tbl$monitorSubtype)

  # Sanity check -- only a single monitorType is allowed
  if ( length(monitorType) > 1 ) {
    logger.error("Multiple monitor types found in 'meta' dataframe: %s", paste0(monitorType, collapse = ", "))
    stop(paste0("Multiple monitor types found in 'meta' dataframe."))
  }

  # Sanity check -- only a single monitorSubtype is allowed
  if ( length(monitorSubtype) > 1 ) {
    logger.error("Multiple monitor subtypes found in 'tbl' dataframe: %s", paste0(monitorSubtype, collapse = ", "))
    stop(paste0("Multiple monitor subtypes found in 'tbl' dataframe."))
  }

  # ----- Create 'data' dataframe ----------------------------------------------

  # Create monitorID the same way we did in airsis_createMetaDataframe()
  # Should only have a single instrumentID
  instrumentIDs <- sort(unique(meta$instrumentID))
  if ( length(instrumentIDs) > 1 ) {
    logger.warn('Multiple instrumentIDs encountered: %s', paste0(instrumentIDs, collapse = ", "))
  }
  instrumentID <- instrumentIDs[1]
  tbl$monitorID <- paste(as.character(tbl$deploymentID), instrumentID, sep = '_')

  if ( monitorType == 'EBAM' ) {
    pm25Var <- 'ConcHr'
  } else if ( monitorType == 'ESAM' ) {
    if ( monitorSubtype == 'MULTI2022' ) {
      pm25Var <- 'ConcHr'
    } else {
      pm25Var <- 'Conc.mg.m3.'
    }
  } else if ( monitorType == 'BAM1020' ) {
    pm25Var <- 'Conc..\u00B5g.m3.'
  } else {
    logger.error("Dataframe creation is not supported for %s", monitorType)
    stop(paste0("Dataframe creation is not supported for ", monitorType))
  }

  # Create minimal subset with the the variables we need for rows, columns and data
  subTbl <- tbl[,c('datetime', 'monitorID', pm25Var)]
  melted <- reshape2::melt(subTbl, id.vars = c('datetime','monitorID'), measure.vars = pm25Var)

  # Unit conversion as needed (mg/m3 ==> ug/m3)

  if ( monitorType == 'EBAM' ) {
    melted$value <- melted$value * 1000
  }

  if ( monitorType == 'ESAM' ) {
    melted$value <- melted$value * 1000
  }

  # Use median if multiple values are found

  # Sanity check -- only one pm25DF measure per hour
  valueCountPerCell <- reshape2::dcast(melted, datetime ~ monitorID, length)
  maxCount <- max(valueCountPerCell[,-1])
  if (maxCount > 1) logger.warn("Up to %s measurements per hour -- median used",maxCount)

  # NOTE:  The resulting dataframe is [datetime,monitorIDs] with monitorIDs in alphabetical order
  pm25DF <- reshape2::dcast(melted, datetime ~ monitorID, stats::median)
  # Reorder data columns to match the order of monitorIDs in 'meta'
  pm25DF <- pm25DF[,c('datetime',meta$monitorID)]

  # Create an empty hourlyDF dataframe with a full time axis (no missing hours)
  datetime <- seq(min(tbl$datetime), max(tbl$datetime), by = "hours")
  hourlyDF <- data.frame(datetime = datetime)

  # Merge pm25DF into the houlyDF dataframe, inserting NA's where necessary
  # NOTE:  dplyr returns objects of class "tbl_df" which can be confusing. We undo that.
  data <- as.data.frame( dplyr::left_join(hourlyDF, pm25DF, by = 'datetime'), stringsAsFactors = FALSE )

  logger.trace("Created 'data' dataframe with %d rows and %d columns", nrow(data), ncol(data))

  return(data)

}
