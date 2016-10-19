#' @keywords OpenAQ
#' @export
#' @importFrom stats median
#' @title Create Data Dataframe
#' @param df a OpenAQ dataframe after metadata enhancement
#' @description After addtional columns(i.e. \code{'datetime'}, \code{'stateCode'}, and \code{'monitorID'}) 
#' have been applied to an OpenAQ dataframe,
#' we can extract the PM2.5 values and store them in a \code{'data'} dataframe
#' organized as hour-by-monitor.
#' 
#' The first column of the returned dataframe is named \code{'datetime'} and
#' contains a \code{POSIXct} time in UTC. Additional columns contain data
#' for each separate monitorID. 
#' 
#' @return A \code{'data'} dataframe for use in a \code{ws_monitor} object.


openaq_createDataDataframe <- function(df){
  subDF <- df[,c("datetime","value","monitorID")]
  melted <- reshape2::melt(subDF,id.vars=c("datetime","monitorID"))
  
  # check if it's exactly one measurement per hour at each location
  countValuePerHr <- reshape2::dcast(melted,datetime~monitorID,length)
  maxCount <- max(countValuePerHr[,-1])
  if (maxCount>1) logger.warn('Up to %s measurements per hour -- median used',maxCount)
  
  # create a dataframe for values
  pm25DF <- reshape2::dcast(melted,datetime~monitorID,median)
  
  # create a dataframe for hours
  hourlyDF <- data.frame(seq(min(melted$datetime),max(melted$datetime),by="hours"))
  names(hourlyDF) <- "datetime"
  
  # combine the two dataframes together by doing a left join
  data <- dplyr::left_join(hourlyDF,pm25DF,by="datetime")
  rownames(data) <- format(data$datetime,"%Y%m%d%H",tz="GMT")
  
  return(data)
}

