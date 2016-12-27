#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Daily Bar Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the given ws_monitor object (optional
#' if only one monitor in the ws_monitor object)
#' @param tlim time limit for barplot
#' @param title plot title
#' @param cex size
#' @param aqiColors color code bars based on AQI
#' @param ... additional arguments to be passed to barplot()
#' @description Creates a bar plot showing daily average PM 2.5 values for a specific monitor in a ws_monitor object.
#' Each bar is colored according to its AQI category.
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150820)
#' # Pull out first monitorID from 'meta' dataframe
#' monitorID <- ws_monitor$meta$monitorID[1]
#' monitor_dailyBarPlot(ws_monitor, monitorID)
#' }

monitorPlot_dailyBarPlot <- function(ws_monitor,
                                     monitorID=NULL,
                                     tlim=NULL,
                                     title="Title",
                                     cex=2,
                                     aqiColors=TRUE,
                                     ...) {
  
  # Plot style ----------------------------------------------------------------
  
  
  
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # NOTE:  Incomting insituTime is GMT
  
  # Insitu data requested 
  pm25 <- ws_monitor$data[[monitorID]]
  utc <- ws_monitor$data$datetime
  index <- which(ws_monitor$meta$monitorID %in% monitorID)
  stationMetadata <- ws_monitor$meta
  
  # TODO:  When AirNow timezones are correct and always available we can do this
  
  # NOTE:  We will functions from the lubridate package to assign an integer
  # NOTE:  dayNum to each date. These need to be aligned so that each day begins
  # NOTE:  at midnight in the local timezone.  We can then use functionality
  # NOTE:  from the dplyr package to group data by dayNum and then summarize
  # NOTE:  to get the average time and average value for each day.
  
  # Convert to local time
  localTime <- lubridate::with_tz(utc, stationMetadata$timezone[index])
  
  # Create a vector of Julian day numbers
  day <- lubridate::date(localTime)
  
  # Put all required elements into a new dataframe
  df <- data.frame(localTime, pm25, day)
  
  # Time limit application
  if ( !is.null(tlim) ) {
    # TODO: add logic to check for tlim format
    timeMask <- localTime >= lubridate::ymd(tlim[1]) & localTime < lubridate::ymd(tlim[2])+lubridate::days(1)
    if (sum(timeMask)==0) {
      PWFSLSmoke::monitorPlot_noData(ws_monitor)
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }
  
  # Create a new dataframe with daily means
  df %>% 
    group_by(day) %>%
    summarize(localTime=mean(localTime,na.rm=TRUE), pm25=mean(pm25,na.rm=TRUE)) ->
    dfMean
  
  # Full days will have a local time mean at 11:30:00 so we can find full days like this
  fullDayMask <- lubridate::hour(dfMean$localTime) == 11
  
  # Subset to only have full days
  dfMean <- dfMean[fullDayMask,]
  
  if (nrow(dfMean) == 0) {
    futile.logger::flog.warn('There are no full days to plot in monitor_dailyBarPlot. Try a ws_monitor that covers more days.')
  }
  
  
  # Plotting ------------------------------------------------------------------

  # Colors come from pm25Daily means
  aqiColors <- adjustcolor(AQI$colors, 0.5)
  cols <- aqiColors[ .bincode(dfMean$pm25, AQI$breaks_24, include.lowest=TRUE) ]
  
  # Create time labels we can use for the X axis
  dayLabels <- strftime(dfMean$localTime, "%b %d")
  
  # Plot
  barplot(dfMean$pm25, las=1,
          col=cols,
          names.arg=dayLabels,
          ylab="PM 2.5")
  
  grid(nx=NA, ny=NULL, col='white', lwd=2)
  
  # Annotations
  title('Daily Average PM2.5')
  
}
