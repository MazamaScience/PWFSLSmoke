#' @keywords raw
#' @export
#' @import graphics
#' @title Create Timeseries Plot from a Raw Dataframe
#' @param df enhanced, raw dataframe as created by the raw_enhance() function
#' @param parameter raw parameter to plot. Default = "pm25". Other options include
#' c("temperature","humidity","windSpeed","windDir", "pressure"), or any of the other raw
#' parameters (do "names(df)" to see list of options)
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime use local times; defaults to GMT if >1 time zone in data.
#' @param shadedNight Shade background based on approximate sunrise/sunset times. Unavailable if >1 time zone in data.
#' Also note that for multiple deployments, this defaults to use the lat/lon for the first deployment, which in theory
#' could be somewhat unrepresentative, such as if deployments have a large range in latitude.
#' @param shadedBackground Add vertical lines corresponding to a second parameter; currently defaults to wind speed binned into quartiles. Future iterations
#' may include options to choose which parameter to plot, which color to use, which intervals, etc.
#' @param sbLwd shaded background line width
#' @param add A logical specifying whether you want to add the data points on top of an existing time series plot
#' @param gridPos position of grid lines either 'over', 'under' ('' for no grid lines)
#' @param gridCol grid line color
#' @param gridLwd grid line width
#' @param gridLty grid line type
#' @param dayLwd day marker line width
#' @param hourLwd hour marker line width
#' @param hourInterval interval for grid (max=12)
#' @param ... additional arguments to pass to lines() function
#' @description Creates a plot of raw monitoring data as generated using raw_enhance().

###############################################################################
#
# This function creates a plot of raw monitoring data, with various bells and
# whistles per user input.
#
###############################################################################

rawPlot_timeseries <- function(df,
                               parameter="pm25",
                               tlim=NULL,
                               localTime=TRUE,
                               shadedNight=TRUE,
                               shadedBackground=NULL, #specify parameter to shade, e.g. 'windSpeed'
                               sbLwd=1,
                               add=FALSE,
                               gridPos='',
                               gridCol='black',
                               gridLwd=1,
                               gridLty='solid',
                               dayLwd=0,
                               hourLwd=0,
                               hourInterval=6,
                               ...) {

  # ----- Initial coherency checks -------------
  
  # Verify plot parameter exists
  if ( !(parameter %in% names(df)) ) {
    stop(paste0("'",parameter,"' does not exist in names(",deparse(substitute(df)),")",sep=""))
  }
  
  # Verify shadedBackground parameter exists
  if ( !(is.null(shadedBackground)) ) {
    if ( !(shadedBackground %in% names(df)) ) {
      warning(paste0("Shaded background parameter '",shadedBackground,"' does not exist in names(",deparse(substitute(df)),")",sep=""))
      shadedBackground <- NULL
    }
  }

  # ----- Data Preparation ----------------------------------------------------
  
  # Identify timezone(s)
  timezone <- unique(df$timezone)
  
  # Force timezone to UTC and disable shadedNight if >1 timezone in metadata for monitorIDs
  if ( length(timezone)>1 ) { # note that we will only enter this condition if localTime==TRUE
    if ( localTime ) {
      warning(">1 timezone in data: Timezone (including tlim, if specified) forced to UTC")
      timezone <- "UTC"
    }
    if ( shadedNight ) {
      warning(">1 timezone in metadata for selected monitorIDs: Shaded Night disabled")
      shadedNight <- FALSE
    }    
  }
  
  # Set timezone to UTC if localTime==FALSE
  if ( !localTime ) {
    timezone <- "UTC"
  }
  
  # Set time axis data
  df$datetime <- lubridate::with_tz(df$datetime, tzone=timezone)

  # Time limit application
  # TODO: add logic to check for tlim format
  # TODO: warn if tlim is outside range of datetime data
  if ( !is.null(tlim) ) {
    
    # When tlim is specified in whole days we add hours to get the requsted full days
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
    tlim <- PWFSLSmoke::parseDatetime(tlim, timezone=timezone)
    
    # Create time mask and subset data
    timeMask <- df$datetime >= tlim[1] & df$datetime <= tlim[2]
    if ( sum(timeMask)==0 ) {
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }
  
  # Pull out key data
  times <- df$datetime
  data <- df[[parameter]]
  
  # Cap hour interval
  if ( hourInterval>12 ) {
    warning("Hour interval capped at 12 hours")
    hourInterval <- min(hourInterval,12)
  }
  
  # ----- Style / Argument List -----------------------------------------------
  
  argsList <- list(...) #TODO: add ... in parens when finished...

  # Prep the data to plot, based on parameter selection by user (default = "pm25")
  argsList$x <- times
  argsList$y <- data
  
  # xlab
  if ( !('xlab' %in% names(argsList)) ) {  
    if ( timezone=="UTC" ) {
      argsList$xlab <- "Date and Time (UTC)"
    } else {
      argsList$xlab <- "Date and Time (local)"
    }
  }
  
  # ylab
  if ( !('ylab' %in% names(argsList)) ) {
    if (parameter == "temperature") {
      argsList$ylab <- "Air Temperature (Deg C)"
    } else if (parameter == "humidity") {
      argsList$ylab <- "Relative Humidity (%)"
    } else if (parameter == "windSpeed") {
      argsList$ylab <- "Wind Speed (m/s)"
    } else if (parameter == "windDir") {
      argsList$ylab <- "Wind Direction (degrees)"
    } else if (parameter == "pm25") {
      argsList$ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
    } else if (parameter == "pressure") {
      argsList$ylab <- "Barometric Pressure (hPa)"
    } else {
      argsList$ylab <- parameter
    }
  }
    
  # main (Title)
  if ( !('main' %in% names(argsList)) ) {
    if (parameter == "temperature") {
      argsList$main <- "Air Temperature"
    } else if (parameter == "humidity") {
      argsList$main <- "Relative Humidity"
    } else if (parameter == "windSpeed") {
      argsList$main <- "Wind Speed"
    } else if (parameter == "windDir") {
      argsList$main <- "Wind Direction"
    } else if (parameter == "pm25") {
      argsList$main <- expression("PM"[2.5]*" Concentration")
    } else if (parameter == "pressure") {
      argsList$main <- "Atmospheric Pressure"
    } else {
      argsList$main <- parameter
    }
  }
  
  # type
  if ( !('type' %in% names(argsList)) ) {
    if ( parameter== "windDir" ) {
      argsList$type <- "p"
    } else {
      argsList$type <- "l"
    }
  }
  
  # ylim TODO: better ylim smarts
  if ( !('ylim' %in% names(argsList)) ) {
    argsList$ylim <- max(c(data,0), na.rm=TRUE)
    argsList$ylim <- c(0, argsList$ylim*1.1)
  }
  
  # ----- Plotting ------------------------------------------------------------
  
  if ( !add ) {
  
    # Set up argsList for blank plot...review to ensure everything captured
    argsListBlank <- argsList

    argsListBlank$col <- 'transparent'
    argsListBlank$axes <- FALSE

    # Create blank plot canvas
    do.call(plot,argsListBlank)
    
    # Shaded Night
    # Based on first deployment lat/lon if >1 deployment; breaks if >1 time zone
    if ( shadedNight ) {
      if ( is.null(shadedBackground) ) {
        if ( length(unique(df$timezone))>1 ) {
          stop("Can't do shaded night for more than one time zone!!")
        } else {
          # Lat/lon for shadedNight
          lat <- df$latitude[1]
          lon <- df$longitude[1]
          timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
          addShadedNights(timeInfo)
        }
      } else {
        warning("Can't do shaded night and shaded background")
      }
    }
    
    # Add vertical lines to denote days and/or hour breaks
    hour_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% hourInterval == 0)
    day_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% 24 == 0)
    abline(v=times[hour_indices], lwd=hourLwd) # at beginning of hour
    abline(v=times[day_indices], lwd=dayLwd) # at beginning of day
    
    # Add horizontal grid lines (before points if grid=='under')
    if ( gridPos == 'under' ) {
      abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
    }
    
    # Put a box around the plot area
    box()
    
    # Add axes if we are not adding points on top of an existing plot
    axis(2, las=1)
    axis.POSIXct(1, times) # TODO: better x axis smarts, e.g. keep from saying "Monday, Tuesday" etc...
    
    # Shaded background
    if ( !is.null(shadedBackground) ) {
      dataSB <- df[[shadedBackground]]
      PWFSLSmoke::addShadedBackground(param=dataSB, timeAxis=times, lwd=sbLwd)
    }
  
  }
  
  # Add lines
  do.call(lines,argsList)

  if ( gridPos == 'over' ) {
    
    # Horizontal lines
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
    
  }

}
