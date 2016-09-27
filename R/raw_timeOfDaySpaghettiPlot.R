#' @export
#' @title Time of Day Spaghetti Plot
#' @param df dataframe with \code{datetime} column in GMT
#' @param dataVar variable to be plotted
#' @param tzone timezone where data were collected
#' @param col color used for each daily line
#' @param meanCol color used for the mean line (use NA to omit the mean)
#' @param meanLwd line width used for th mean line
#' @param highlightDates dates to be highlighted in YYYYMMDD format
#' @param highlightCol color used for highlighted days
#' @param ... additional graphical parameters are passed to the lines() function
#' @description Spaghetti Plot that shows data by hour-of-day. 
#' @examples
#' \dontrun{
#' }

raw_timeOfDaySpaghettiPlot <- function(df, dataVar, tzone=NULL,
                                       col='salmon',
                                       meanCol='black', meanLwd=4,
                                       highlightDates=c(),
                                       highlightCol='dodgerblue',
                                       ...) {

  # Sanity check -- 'datetime' must exist
  if ( !'datetime' %in% names(df) ) {
    stop(paste0("Dataframe 'df' has no 'datetime' column."))
  }
  
  # Data Preparation ----------------------------------------------------------
  
  df$localTime <- lubridate::with_tz(df$datetime,tzone)
  df$datestamp <- format(df$localTime,"%Y%m%d")
  df$hour <- lubridate::hour(df$localTime)
  
  # Create a subset dataframe for local use
  df <- df[,c('localTime',dataVar,'datestamp','hour')]
  names(df) <- c('localTime','data','datestamp','hour')
  
  # Style  --------------------------------------------------------------------
  

  # Plotting ------------------------------------------------------------------
  
  # Blank plot to set up limits
  plot(df$data ~ df$hour, col='transparent',
       xlab='', ylab='',
       axes=FALSE)
  axis(1,at=seq(0,24,3))
  axis(2,las=1)
  mtext(dataVar,2,line=3)
  mtext(paste0('Hour'),1,line=3)

  # Simple line plot for each day
  for ( singleDay in unique(df$datestamp) ) {
    
    dayDF <- dplyr::filter(df, datestamp == singleDay)
    
    if ( singleDay %in% highlightDates ) {
      lines(dayDF$data ~ dayDF$hour, col=highlightCol, ...)
    } else {
      lines(dayDF$data ~ dayDF$hour, col=col, ...)
    }
    
  }
  
  # Add mean line
  df %>% group_by(as.factor(hour)) %>%
    summarize(data=mean(data,na.rm=TRUE)) ->
    hourMeanDF
  
  lines(hourMeanDF$data ~ seq(0,23,1), col=meanCol, lwd=meanLwd)

}

