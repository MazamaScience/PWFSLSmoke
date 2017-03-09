#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Daily Barplot
#' @param ws_monitor emph{ws_monitor} object
#' @param monitorID monitor ID for a specific monitor in \code{ws_monitor} (optional
#' if \code{ws_monitor} only has one monitor)
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param minHours minimum number of valid data hours required to calculate each daily average
#' @param gridPos position of grid lines either 'over', 'under' ('' for no grid lines)
#' @param gridCol color of grid lines (see graphical parameter 'col')
#' @param gridLwd line width of grid lines (see graphical parameter 'lwd')
#' @param gridLty type of grid lines (see graphical parameter 'lty')
#' @param labels_x_nudge nudge x labels to the left
#' @param labels_y_nudge nudge y labels down
#' @param ... additional arguments to be passed to \code{barplot()}
#' @description Creates a bar plot showing daily average PM 2.5 values for a specific monitor
#' in a emph{ws_monitor} object. Each bar is colored according to its AQI category.
#' 
#' This function is a wrapper around \code{base::barplot} and any arguments to that 
#' function may be used.
#' 
#' Each 'day' is the midnight-to-midnight period in the monitor local timezone.
#' When \code{tlim} is used, it is converted to the monitor local timezone.
#' @details The \code{labels_x_nudge} and \code{labels_y_nudge} can be used to 
#' tweak the date labeling. Units used are the same as those in the plot.
#' @examples
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150715,20150930))
#' main <- "Daily Average PM2.5 for Omak, WA"
#' monitorPlot_dailyBarplot(N_M, monitorID="530470013", main=main,
#'                          labels_x_nudge=1)
#' addAQILegend(fill=rev(AQI$colors), pch=NULL)

monitorPlot_dailyBarplot <- function(ws_monitor,
                                     monitorID=NULL,
                                     tlim=NULL,
                                     minHours=20,
                                     gridPos='',
                                     gridCol='black',
                                     gridLwd=0.5,
                                     gridLty='solid',
                                     labels_x_nudge=0,
                                     labels_y_nudge=0,
                                     ...) {
 
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for > 1 monitor. Please specify a monitorID from: '",
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
  
  argsList$height <- pm25
  
  # Default colors come from pm25Daily means
  if ( !('col' %in% names(argsList)) ) {
    aqiColors <- AQI$colors
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  }
  
  # X axis labeling is handled after the plot
  
  # NOTE:  For mathematical notation in R see:
  # NOTE:    http://vis.supstat.com/2013/04/mathematical-annotation-in-r/

  # Y axis labeling
  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
  }
  
  # Additional small tweaks
  argsList$las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Title
  if ( !('main' %in% names(argsList)) ) {
    argsList$main <- expression(paste("Daily Average PM"[2.5]))
  }
  # Explicitly declare defaults for use in creating the x axis
  argsList$axes <- ifelse('axes' %in% names(argsList), argsList$axes, TRUE)
  argsList$space <- ifelse('space' %in% names(argsList), argsList$space, 0.2)
  argsList$cex.names <- ifelse('cex.names' %in% names(argsList), argsList$cex.names, par("cex.axis"))
  
  # Plotting ------------------------------------------------------------------

  if ( gridPos == 'under' ) {
    do.call(barplot, argsList)
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
    argsList$add <- TRUE    
  }
  
  do.call(barplot, argsList)

  # Add default X axis
  if ( argsList$axes && !('names.arg' %in% names(argsList)) ) {
    barCount <- length(argsList$height)
    allIndices <- 1:barCount
    allLabels <- strftime(localTime, "%b %d")
    maxLabelCount <- 16
    stride <- round(barCount/maxLabelCount)
    if ( stride == 0 ) {
      indices <- allIndices
      labels <- allLabels
    } else {
      indices <- allIndices[seq(1,barCount,by=stride)]
      labels <- allLabels[seq(1,barCount,by=stride)]
    }
    labels_x <- (indices - 0.5) + (indices * argsList$space)
    labels_y <- -0.06 * (par('usr')[4] - par('usr')[3])
    # Add tilted dates
    text(labels_x - labels_x_nudge, labels_y - labels_y_nudge, labels, srt=45, cex=argsList$cex.names, xpd=NA)
    # Now add tick marks
    axis(1, at=labels_x, labels=FALSE, lwd=0, lwd.ticks=1)
  }
  
  # Add horizontal bars
  if ( gridPos == 'over' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
}
