#' @keywords raw
#' @export
#' @import graphics
#' @title Create Time of Day Spaghetti Plot from a Raw Dataframe
#' @param df enhanced, raw dataframe as created by the raw_enhance() function
#' @param parameter variable to be plotted
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param shadedNight add nighttime shading
#' @param meanCol color used for the mean line (use NA to omit the mean)
#' @param meanLwd line width used for the mean line
#' @param meanLty line type used for the mean line
#' @param highlightDates dates to be highlighted in YYYYMMDD format
#' @param highlightCol color used for highlighted days
#' @param ... additional graphical parameters are passed to the lines() function for day lines
#' @description Spaghetti Plot that shows data by hour-of-day. 
#' @examples
#' \dontrun{
#' raw <- airsis_createRawDataframe(startdate = 20160901, enddate=20161015, provider = 'USFS', unitID = 1012)
#' raw <- raw_enhance(raw, rawSource='AIRSIS')
#' rawPlot_timeOfDaySpaghetti(raw,parameter="temperature")
#' }

rawPlot_timeOfDaySpaghetti <- function(df,
                                       parameter='pm25',
                                       tlim=NULL,
                                       shadedNight=TRUE,
                                       meanCol='black',
                                       meanLwd=4,
                                       meanLty=1,
                                       highlightDates=c(),
                                       highlightCol='dodgerblue',
                                       ...) {

  # Initial Style
  
  dayCol <- 'salmon' #default; can be overwritten with col, e.g. col='blue'
  col_shadedNight <- 'gray90'
  
  # Data Preparation ----------------------------------------------------------
  
  # Sanity check -- 'datetime' must exist
  if ( !'datetime' %in% names(df) ) {
    stop(paste0("Dataframe has no 'datetime' column."))
  }
  
  # Pull out timezone and lat/lon  
  if( length(unique(df$timezone)) > 1 ) {
    stop("More than one timezone in the data -- please subset as necessary and try again.")
  } else {
    tzone <- df$timezone[1]
    lat <- df$latitude[1]
    lon <- df$longitude[1]
  }
  
  # Create new dataframe
  df$localTime <- lubridate::with_tz(df$datetime,tzone)
  df$datestamp <- format(df$localTime,"%Y%m%d")
  df$hour <- lubridate::hour(df$localTime)
  
  # Create a subset dataframe for local use
  df <- df[,c('localTime',parameter,'datestamp','hour')]
  names(df) <- c('localTime','data','datestamp','hour')
  
  # Subset dataframe by tlim
  if ( !is.null(tlim) ) {
    
    # Chop tlim to full days unless hours included in argument
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
    
    tlim <- as.POSIXct(tlim,format='%Y%m%d%H')
    df <- df[(df$localTime>=tlim[1] & df$localTime <= tlim[2]),]
    
  }
  
  # Style  --------------------------------------------------------------------
  
  argsList <- list(...)
  
  if ( !('col' %in% names(argsList)) ) {
    argsList$col <- dayCol
  }
  
  if ( !('ylim' %in% names(argsList)) ) {
    argsList$ylim <- c(min(df$data,na.rm = TRUE),max(df$data,na.rm = TRUE))
  }
  
  if ( !('ylab' %in% names(argsList)) ) {
    if ( parameter=='pm25' ) {
      argsList$ylab <- "PM2.5"
    } else {
      argsList$ylab <- parameter
    }
  }
  
  if ( !('xlab' %in% names(argsList)) ) {
    argsList$xlab <- "Hour"
  }
  
  # Plotting ------------------------------------------------------------------
  
  # Define data for plot bounds
  argsList$x <- df$hour
  argsList$y <- df$data
  
  # Set up duplicate argsList for blank plot
  argsListBlank <- argsList
  
  # Assign blankness to duplicate argsList
  argsListBlank$col <- 'transparent'
  argsListBlank$axes <- FALSE
  
  # Plot blank plot
  do.call(plot,argsListBlank)
  
  # Add Shaded Night
  if ( shadedNight ) {
    
    # Get the sunrise/sunset information
    ti <- timeInfo(df$localTime, lon=lon, lat=lat, timezone=tzone)
    
    # Extract the middle row
    ti <- ti[round(nrow(ti)/2),]
    
    # Get sunrise and sunset in units of hours
    sunrise <- lubridate::hour(ti$sunrise) + lubridate::minute(ti$sunrise)/60
    sunset <- lubridate::hour(ti$sunset) + lubridate::minute(ti$sunset)/60
    
    # Left edge to sunrise
    rect(par('usr')[1], ybottom=par('usr')[3],
         xright=sunrise, ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
    # Sunset to right edge
    rect(xleft=sunset, ybottom=par('usr')[3],
         xright=par('usr')[2], ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
  }
  
  # Annotations
  axis(1,at=seq(0,23,3))
  axis(2,las=1)

  # Simple line plot for each day
  for ( singleDay in unique(df$datestamp) ) {
    
    dayDF <- dplyr::filter(df, df$datestamp == singleDay)
    
    argsList$x <- dayDF$hour
    argsList$y <- dayDF$data
    
    if ( singleDay %in% highlightDates ) {
      argsList$col <- highlightCol
      do.call(lines,argsList)
      argsList$col <- dayCol
    } else {
      do.call(lines,argsList)
    }
    
  }
  
  # Add mean line
  hourMeanDF <- data.frame(hour=seq(0,23))
  for (hr in 0:23) {
    hourMeanDF$data[hr+1] <- mean(df$data[which(df$hour==hr)],na.rm = TRUE)
  }
  
  lines(hourMeanDF$data ~ hourMeanDF$hour, col=meanCol, lwd=meanLwd, lty=meanLty)

}

