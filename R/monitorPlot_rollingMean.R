#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Individual Value and Rolling Mean Plot
#' @param ws_monitor emph{ws_monitor} object
#' @param monitorID monitor ID for a specific monitor in the ws_monitor object (optional if only one monitor
#' in the ws_monitor object)
#' @param width number of periods to average (e.g. for hourly data, \code{width = 24} plots 24-hour rolling means)
#' @param align alignment of averaging window relative to point being calculated; one of \code{"left|center|right"}
#' @param data.thresh minimum number of valid observations required as a percent of \code{width};
#' NA is returned if insufficicnet valid data to calculate mean
#' @param ylim y limits for the plot
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime logical specifying whether \code{tlim} is in local time or UTC
#' @param shadedNight add nighttime shading
#' @param aqiLines horizontal lines indicating AQI levels
#' @param gridHorizontal add dashed horizontal grid lines
#' @param grid24hr add dashed grid lines at day boundaries
#' @param grid3hr add dashed grid lines every 3 hours
#' @param showLegend include legend in top left
#' @description Creates a plot of individual (e.g. hourly) and rolling mean PM2.5 values for a specific monitor.
#' @details \itemize{
#' \item{\code{align = 'left'}: Forward roll, using hour of interest and the (\code{width}-1) subsequent hours 
#' (e.g. 3-hr left-aligned roll for Hr 5 will consist of average of Hrs 5, 6 and 7)}
#' \item{\code{align = 'right'}: Backwards roll, using hour of interest and the (\code{width}-1) prior hours
#' (e.g. 3-hr right-aligned roll for Hr 5 will consist of average of Hrs 3, 4 and 5)}
#' \item{\code{align = 'center'} for odd \code{width}: Average of hour of interest and (\code{width}-1)/2 on either side
#' (e.g. 3-hr center-aligned roll for Hr 5 will consist of average of Hrs 4, 5 and 6)}
#' \item{\code{align = 'center'} for even \code{width}: Average of hour of interest and (\code{width}/2)-1 hours prior and 
#' \code{width}/2 hours after (e.g. 4-hr center-aligned roll for Hr 5 will consist of average of Hrs 4, 5, 6 and 7)}
#' }
#' @note This function attempts to provide a 'publication ready' rolling mean plot.
#' @examples
#' N_M <- Northwest_Megafires
#' Roseburg <- monitor_subset(N_M, tlim=c(20150821, 20150831),
#'                            monitorIDs=c('410190002'))
#' monitorPlot_rollingMean(Roseburg, shadedNight=TRUE)

monitorPlot_rollingMean <- function(ws_monitor,
                                    monitorID=NULL,
                                    width=3,
                                    align="center",
                                    data.thresh=75,
                                    tlim=NULL,
                                    ylim=NULL,
                                    localTime=TRUE,
                                    shadedNight=FALSE,
                                    aqiLines=TRUE,
                                    gridHorizontal=FALSE,
                                    grid24hr=FALSE,
                                    grid3hr=FALSE,
                                    showLegend=TRUE) {
  
  # ----- Style ---------------------------------------------------------------
  
  # points
  col_points <- adjustcolor('black',0.2)
  pch_points <- 16
  cex_points <- .8
  
  # rolling mean
  col_mean <- 'black'
  lwd_mean <- 1
  lty_mean <- 'solid'
  
  # Grid lines
  lty_grid <- 'dotted'
  col_grid <- 'gray80'
  lwd_grid <- 1.6
  lwd_gridLight <- 0.8 # for 3 hour intervals
  
  # AQI lines
  lwd_aqi <- 6
  col_aqi <- adjustcolor(AQI$colors[2:6], 0.6)
  
  # ----- Data Preparaion ------------------------
  
  # Roll direction for legend
  if ( align=='left' ) {
    direction <- 'Forward-'
  } else if ( align=='right' ) {
    direction <- 'Backward-'
  } else {
    direction <- 'Centered '
  }
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",
                  paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # Subset ws_monitor object to monitor of interest and pull out meta & data
  ws_monitor <- PWFSLSmoke::monitor_subset(ws_monitor,monitorIDs = monitorID)
  meta <- ws_monitor$meta
  data <- ws_monitor$data
  
  # Pull out hour averages and create rolling means for dataframe prior to subsetting by tlim
  hourAvgs <- data[[monitorID]]
  rollingMeans <- PWFSLSmoke::monitor_rollingMean(ws_monitor, width=width, data.thresh=data.thresh, align=align)$data[[monitorID]]
  
  # Assign timeStamp based on localTime setting
  if ( localTime ) {
    timeStamp <- lubridate::with_tz(data$datetime,meta$timezone)
    tzLabel <- '(local)'
  } else {
    timeStamp <- data$datetime
    tzLabel <- '(UTC)'
  }
  
  # put the pieces of the new dataframe together
  df <- data.frame(timeStamp,hourAvgs,rollingMeans)
  
  # Time limit application
  if ( !is.null(tlim) ) {
    # TODO: Warn if no data for any dates within tlim?
    # TODO: add logic to check for tlim format
    timeMask <- timeStamp >= lubridate::ymd(tlim[1]) & timeStamp < lubridate::ymd(tlim[2])+lubridate::days(1)
    if ( sum(timeMask)==0 ) {
      monitorPlot_noData(ws_monitor)
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }
  
  # Set y Limits
  if ( is.null(ylim) ) {
    #ylim <- c(min(0,min(df$hourAvgs,na.rm=TRUE)),max(df$hourAvgs,na.rm=TRUE))
    ylim <- c(0,max(c(df$hourAvgs,275),na.rm=TRUE))
  }
  
  # Date ranges for grid line locatioins and chart title
  minTime <- df$timeStamp[1]
  maxTime <- utils::tail(df$timeStamp, 1)
  
  # Find first full day for grid lines and time axis labels
  min24Hour <- timeStamp[which(lubridate::hour(timeStamp)==0)[1]]
  
  # Find start of first 3-hour period (e.g. Hr 0, 3, 6...)
  threeHrRemainder <- lubridate::hour(minTime)/3-floor(lubridate::hour(minTime)/3)
  if ( threeHrRemainder == 0 ) { # first time stamp is right on a 3-hr marker
    min3Hour <- df$timeStamp[1]
  } else if ( threeHrRemainder < .5 ) { # first time stamp is one hour after a 3-hr marker
    min3Hour <- df$timeStamp[1]-lubridate::hours(1)
  } else {
    min3Hour <- df$timeStamp[1]-lubridate::hours(2)
  }
  
  # Define grid line locations
  xGrid24Hour <- seq(min24Hour,maxTime+lubridate::hours(1),"day")
  xGrid3Hour <- seq(min3Hour,maxTime+lubridate::hours(1),"3 hour")
  if ( grid24hr==TRUE) {
    xGrid3Hour <- xGrid3Hour[!(xGrid3Hour %in% xGrid24Hour)]
  }
  
  # Assign time axis label
  if ( nrow(df) <=24 ) {
    xlab <- 'Hour'
  } else {
    xlab <- 'Date'
  }
  xlab <- paste(xlab,tzLabel)
  
  # ----- Plotting --------------------------------
  
  # Create blank plot
  plot(df$timeStamp, df$hourAvgs,
       type = 'n',
       axes=FALSE,
       xlab = xlab,
       ylab = '',
       ylim = ylim
  )
  
  # Shaded Night; breaks if >1 time zone
  if ( shadedNight ) {
    if ( length(unique(meta$timezone)) >1) {
      stop("Can't do shaded night for more than one time zone!!")
    } else {
      lon <- meta$longitude
      lat <- meta$latitude
      timezone <- meta$timezone
      if ( localTime ) {
        timeInfo <- PWFSLSmoke::timeInfo(timeStamp, lon, lat, timezone)
        PWFSLSmoke::addShadedNight(timeInfo)
      } else {
        timeInfo <- PWFSLSmoke::timeInfo(data$datetime, lon, lat, timezone)
        PWFSLSmoke::addShadedNight(timeInfo)
      }
    }
  }
  
  # AQI Lines
  if ( aqiLines ) {
    abline(h=AQI$breaks_24[2:6], col=col_aqi, lwd=lwd_aqi)
  }
  
  # Add grid lines
  if ( gridHorizontal ) {  # Horizontal
    grid(nx=NA,ny=NULL, col=col_grid, lwd=lwd_grid, lty=lty_grid) #horizontal lines
  }
  if ( grid3hr ) {  # 3-hour interval
    abline(v=xGrid3Hour, col=col_grid, lwd=lwd_gridLight, lty=lty_grid) # Vertical lines - 3 Hr intervals
  }
  if ( grid24hr ) {   # 24-hour interval
    abline(v=xGrid24Hour, col=col_grid, lwd=lwd_grid, lty=lty_grid)
  }
  
  # Box the plot area
  box()
  
  # Plot light grey circles for actual PM2.5 data
  points(df$timeStamp, df$hourAvgs,
         col=col_points,
         pch=pch_points,
         cex=cex_points)
  
  # Add rolling means
  lines(df$timeStamp, df$rollingMeans, col='black', lwd=lwd_mean, lty=lty_mean)
  
  # ----- Annotations ---------------------
  
  # Horizontal axis (see "Assign timeStamp based on localTime setting" above for horizontal axis label)
  if ( nrow(df) <=24 ) {
    axis.POSIXct(1, at=seq(minTime, maxTime+lubridate::hours(1), by="3 hour"), format="%H")
  } else {
    axis.POSIXct(1, at=seq(min24Hour, maxTime+lubridate::hours(1), by="1 day"), format="%b %d")
  }
  
  # Vertical axis
  axis(2,las=1)
  mtext(expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")")),2,line=2.5)
  
  # Title
  mtext(expression(paste("Hourly and Rolling Mean PM"[2.5])),line=2,cex = 1.5)
  if ( lubridate::date(minTime)==lubridate::date(maxTime) ) {
    mtext(format(minTime, '%b. %d %Y'),line=.7,cex=1.5)
  } else {
    mtext(paste(format(minTime, '%b. %d - '),format(maxTime, '%b. %d %Y')),line=.7,cex=1.5)
  }
  
  # Legend
  if ( showLegend ) {
    legend("topleft", 
           legend=c('Hourly Averages',paste0(width, "-hour ",direction,"Rolling Mean")),
           cex=par('cex.lab'),
           col=c(col_points,col_mean),
           lwd=c(NA,lwd_mean),
           lty=c(NA,lty_mean),
           pch=c(pch_points,NA))
  }
  
}
