#' @keywords ws_monitor
#' @export
#' @title Create Interactive Time Series Plot
#' @param ws_monitor ws_monitor object
#' @param title title text
#' @param ylab title for the y axis
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param rollPeriod rolling mean to be applied to the data
#' @param showLegend logical to toggle display of the legend
#' @description This function creates interactive graphs that will be displayed in RStudio's 'Viewer' tab.  
#' @return Initiates the interactive dygraph plot in RStudio's 'Viewer' tab.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' King_Fire <- monitor_subsetByDistance(airnow, lon=-120.604, lat=38.782, radius=50)
#' monitorDygraph(King_Fire, title='KingFire/California/2014', rollPeriod=3)
#' } 

monitorDygraph <- function(ws_monitor, title='title', ylab='PM2.5 Concentration', 
                           tlim=NULL, rollPeriod=1, showLegend=TRUE) {
  
  # Sanity check
  tzCount <- length(unique(ws_monitor$meta$timezone))
  if (tzCount > 1) {
    stop(paste0('Dygraphs cannot be made for data with multiple timezones: ',tzCount,' were found'))
  }
  
  # Convert tlim to POSIXct
  if ( !is.null(tlim) ) {
    dateWindow <- parseDatetime(tlim)
  } else {
    dateWindow <- NULL
  }
  
  # Simplify access to variables
  datetime <- ws_monitor$data$datetime
  tzone <- ws_monitor$meta[1,'timezone']
  
  # Create an xts from all data columns except the first which is 'datetime'
  timeseriesData <- xts::xts(ws_monitor$data[,-1],datetime,tzone=tzone)
  
  # Add siteNames
  # Sanity check for existence of siteName column 
  # NOTE:  ws_monitor objects derived from ws_grid objects only have the monitorID column
  if ( is.null(ws_monitor$meta$siteName) ) {
    ws_monitor$meta$siteName <- ws_monitor$meta$monitorID
  }
  # Sanity check for existence of names
  siteNames <- ifelse(is.na(ws_monitor$meta$siteName), names(ws_monitor$data)[-1], ws_monitor$meta$siteName)
  names(timeseriesData) <- siteNames
  
  show <- ifelse(showLegend,'always','never')
  
  # Create dygraph
  dygraphs::dygraph(timeseriesData, main=title, ylab=ylab) %>%
    dygraphs::dyOptions(useDataTimezone=TRUE) %>%                       # Always show local time
    dygraphs::dyLegend(show=show, width=500, labelsSeparateLines=TRUE) %>%
    dygraphs::dyRangeSelector(dateWindow=dateWindow) %>%
    dygraphs::dyRoller(rollPeriod=rollPeriod)
  
}
