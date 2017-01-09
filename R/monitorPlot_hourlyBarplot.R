#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Hourly Barplot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the given ws_monitor object (optional
#' if only one monitor in the ws_monitor object)
#' @param tlim time limit for barplot
#' @param localTime logical specifying whether \code{tlim} is in UTC or local time
#' @param style named style specification ('AirFire')
#' @param title plot title
#' @param shadedNight add nighttime shading
#' @param ... additional arguments to be passed to barplot()
#' @description Creates a bar plot showing hourly PM 2.5 values for a specific monitor in a ws_monitor object.
#' Colors are assigned to one of the following styles:
#' \itemize{
#' \item{\code{AirFire} -- hourly values colored by AQI 24-hour breaks}
#' }
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150701, 20150710)
#' title <- "Hourly PM2.5 for Colleville, WA"
#' monitorPlot_hourlyBarplot(airnow, monitorID="530650004", title=title)
#' }

monitorPlot_hourlyBarplot <- function(ws_monitor,
                                      monitorID=NULL,
                                      tlim=NULL,
                                      localTime=TRUE,
                                      style='AirFire',
                                      title=NULL,
                                      shadedNight=FALSE,
                                      ...) {
  
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",
                  paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # TODO:  handle localTime=TRUE, may require additional localTime argument to monitor_subset
  
  
  
  # Subset to a single monitor
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim)
  
  # TODO: assign local or UTC datetime depending on localTime flag
  
  if ( localTime ) {
    datetime <- mon$data$datetime
  } else {
    datetime <- mon$data$datetime
  }
  
  pm25 <- as.numeric(mon$data[,monitorID])
  
  # Plot command default arguments ---------------------------------------------
  
  argsList <- list(...)
  
  # TODO:  We will eventually need a cople of different choices for assigning colors based on
  # TODO:  hourly, nowcast, AQI, etc.
  
  # 'AirFire' colors use hourly values with 24 hour colors
  if ( style == 'AirFire' ) {
    argsList$height <- pm25
    aqiColors <- adjustcolor(AQI$colors, 0.5)
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  } else if ( style == 'NOT_YET_IMPLEMENTED' ) {
    # modify pm25 with either nowcast, rolling mean or aqi
    # modify color breaks and levels as needed
  }
  
  
  # X axis labeling
  argsList$xlab <- ifelse('xlab' %in% names(argsList), argsList$xlab, "Date")
  if ( !('names.arg' %in% names(argsList)) ) {
    argsList$names.arg <- strftime(datetime, "%b %d")
  }
  
  # NOTE:  For mathematical notation in R see:
  # NOTE:    http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
  
  # Y axis labeling
  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
  }
  
  # Additional small tweaks
  argsList$las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Plotting ------------------------------------------------------------------
  
  do.call(barplot, argsList)
  
  # Shaded Night; breaks if >1 time zone
  if ( shadedNight ) {
    if ( length(unique(mon$eta$timezone)) >1) {
      stop("Can't do shaded night for more than one time zone!!")
    } else {
      lon <- mon$meta$longitude
      lat <- mon$meta$latitude
      timezone <- mon$meta$timezone
      if ( localTime ) {
        timeInfo <- PWFSLSmoke::timeInfo(datetime, lon, lat, timezone)
        PWFSLSmoke::addShadedNights(timeInfo)
      } else {
        timeInfo <- PWFSLSmoke::timeInfo(mon$data$datetime, lon, lat, timezone)
        PWFSLSmoke::addShadedNights(timeInfo)
      }
    }
  }
  
  
  
  # Add horizontal bars
  grid(nx=NA, ny=NULL, col='white', lwd=2)
  
  # Add a title
  if ( is.null(title) ) {
    title <- expression(paste("Hourly Average PM"[2.5]))
  }
  title(title)
  
}
