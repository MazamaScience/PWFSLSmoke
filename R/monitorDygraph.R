#' @keywords ws_monitor
#' @export
#' @title Create Interactive Time Series Plot
#' @param ws_monitor \emph{ws_monitor} object
#' @param title title text
#' @param ylab title for the y axis
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param rollPeriod rolling mean to be applied to the data
#' @param showLegend logical to toggle display of the legend
#' @description This function creates interactive graphs that will be displayed in RStudio's 'Viewer' tab.  
#' @return Initiates the interactive dygraph plot in RStudio's 'Viewer' tab.
#' @examples
#' \dontrun{
#' ca <- airnow_load(2017) %>%
#'   monitor_subset(tlim=c(20171001,20171101), stateCodes='CA')
#' Vallejo <- monitor_subset(ca, monitorIDs='060950004_01')
#' Napa_Fires <- monitor_subsetByDistance(ca,
#'                                        longitude = Vallejo$meta$longitude,
#'                                        latitude = Vallejo$meta$latitude,
#'                                        radius = 50)
#' monitorDygraph(Napa_Fires, title='Napa Fires in California, Oct. 2017')
#' } 

monitorDygraph <- function(ws_monitor,
                           title = 'title',
                           ylab = 'PM2.5 Concentration', 
                           tlim = NULL,
                           rollPeriod = 1,
                           showLegend = TRUE) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }
  
  # Convert tlim to POSIXct
  if ( !is.null(tlim) ) {
    dateWindow <- parseDatetime(tlim)
  } else {
    dateWindow <- NULL
  }
  
  # Set timezone
  tzCount <- length(unique(ws_monitor$meta$timezone))
  if (tzCount > 1) {
    warning(paste0(tzCount, ' timezones found. Using UTC time.'))
    tzone <- 'UTC'
  } else {
    tzone <- unique(ws_monitor$meta$timezone)
  }
  
  # Simplify access to variables
  datetime <- ws_monitor$data$datetime
  
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
    dygraphs::dyLegend(show=show, width=250, labelsSeparateLines=TRUE) %>%
    dygraphs::dyRangeSelector(dateWindow=dateWindow) %>%
    dygraphs::dyRoller(rollPeriod=rollPeriod)
  
}
