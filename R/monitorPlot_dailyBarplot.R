#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Daily Bar Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the given ws_monitor object (optional
#' if only one monitor in the ws_monitor object)
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param minHours minimum number of valid data hours required to calculate each daily average
#' @param grid add grid lines either 'over', 'under' or '' for no grid lines
#' @param gridCol color of grid lines (see graphical parameter 'col')
#' @param gridLwd line width of grid lines (see graphical parameter 'lwd')
#' @param gridLty type of grid lines (see graphical parameter 'lty')
#' @param ... additional arguments to be passed to barplot()
#' @description Creates a bar plot showing daily average PM 2.5 values for a specific monitor
#' in a ws_monitor object. Each bar is colored according to its AQI category.
#' 
#' This function is a wrapper around \code{base::barplot} and any arguments to that 
#' function may be used.
#' 
#' Each 'day' is the midnight-to-midnight period in the monitor local timezone.
#' When \code{tlim} is used, it is converted to the monitor local timezone.
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150701, 20150831)
#' main <- "Daily Average PM2.5 for Colleville, WA"
#' monitorPlot_dailyBarplot(airnow, monitorID="530650004", main=main)
#' }

monitorPlot_dailyBarplot <- function(ws_monitor,
                                     monitorID=NULL,
                                     tlim=NULL,
                                     minHours=20,
                                     grid='over',
                                     gridCol='white',
                                     gridLwd=2,
                                     gridLty='dotted',
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
  
  # When tlim is specified in whole days we should add hours to get the requsted full days
  if ( !is.null(tlim) ) {
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
  }
  
  # Subset to a single monitor
  timezone <- as.character(ws_monitor$meta[monitorID,'timezone'])
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, timezone=timezone)
  
  # Calculate the daily mean
  mon_dailyMean <- monitor_dailyStatistic(mon, FUN=get('mean'), dayStart='midnight',
                                          na.rm=TRUE, minHours=minHours)
  
  localTime <- mon_dailyMean$data$datetime
  pm25 <- as.numeric(mon_dailyMean$data[,monitorID])
  
  # Plot command default arguments ---------------------------------------------
  
  argsList <- list(...)
  
  # Remove non-standard arguments from argsList
  argsList$grid <- NULL
  argsList$gridCol <- NULL
  argsList$gridLwd <- NULL
  argsList$gridLty <- NULL
  
  argsList$height <- pm25
  
  # Default colors come from pm25Daily means
  if ( !('col' %in% names(argsList)) ) {
    aqiColors <- AQI$colors
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  }
  
  # X axis labeling
  argsList$xlab <- ifelse('xlab' %in% names(argsList), argsList$xlab, "Date")
  if ( !('names.arg' %in% names(argsList)) ) {
    argsList$names.arg <- strftime(localTime, "%b %d")
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

  if ( grid == 'under' ) {
    do.call(barplot, argsList)
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
    argsList$add <- TRUE    
  }
  
  do.call(barplot, argsList)

  # Add horizontal bars
  if ( grid == 'over' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
  # Add a title if none was specified
  if ( !('main' %in% names(argsList)) ) {
    main <- expression(paste("Daily Average PM"[2.5]))
    title(main)
  }
  
}
