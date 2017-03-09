#' @keywords EPA
#' @export
#' @title Create EPA Data Dataframe
#' @param df an EPA raw dataframe after metadata enhancement
#' @description After addtional columns(i.e. \code{datetime}, and \code{monitorID}) 
#' have been applied to an EPA dataframe, we are ready to
#' extract the PM2.5 values and store them in a \code{data} dataframe
#' organized as time-by-monitor.
#' 
#' The first column of the returned dataframe is named \code{datetime} and
#' contains a \code{POSIXct} time in UTC. Additional columns contain data
#' for each separate monitorID. 
#' 
#' @return A \code{data} dataframe for use in a \emph{ws_monitor} object.

epa_createDataDataframe <- function(df) {

  subDF <- df[,c("monitorID","datetime","Sample Measurement")]
  # The data file will refer to any of the original instrument's readings, the bulk of the information.
  
  # "melt" the data frame into long-format data
  # The "melt" function will turn the column names into their own column and the rest of the data into a second.
  melted <- reshape2::melt(data=subDF, id.vars = c("datetime","monitorID"), measure.vars="Sample Measurement")
  
  # Sanity check -- only one pm25DF measure per hour
  valueCountPerCell <- reshape2::dcast(melted, datetime ~ monitorID, length)
  maxCount <- max(valueCountPerCell[,-1])
  if (maxCount > 1) logger.warn("Up to %s measurements per hour -- median used",maxCount)
  
  # create a dataframe for values
  pm25DF <- reshape2::dcast(melted, datetime ~ monitorID, stats::median)
  
  # create a dataframe for hours
  hourlyDF <- data.frame(seq(min(melted$datetime),max(melted$datetime),by="hours"))
  names(hourlyDF) <- "datetime"
  
  # combine the two dataframes together by doing a left join
  data <- dplyr::left_join(hourlyDF, pm25DF, by="datetime")

  logger.debug("'data' dataframe has %d rows and %d columns", nrow(data), ncol(data))
  
  return(data)
}
