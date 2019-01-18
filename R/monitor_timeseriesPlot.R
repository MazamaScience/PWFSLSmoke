#' @title Create Timeseries Plot
#'
#' @description
#' Creates a time series plot of PM2.5 data from a \emph{ws_monitor} object (see
#' note below). Optional arguments color code by AQI index, add shading to
#' indicate nighttime, and adjust the time display (local vs. UTC).
#'
#' When a named \code{style} is used, some graphical parameters will be
#' overridden. Available styles include:
#'
#' \itemize{
#'   \item{\code{aqidots}}{-- hourly values are individually colored by 24-hr
#'     AQI levels}
#'   \item{\code{gnats}}{-- semi-transparent dots like a cloud of gnats}
#' }
#'
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID for one or more monitor in the ws_monitor object.
#' @param tlim Optional vector with start and end times (integer or character
#'   representing YYYYMMDD[HH]).
#' @param localTime Logical specifying whether \code{tlim} is in local time or
#'   UTC.
#' @param shadedNight Add nighttime shading.
#' @param style Custom styling, one of \code{"aqidots"}.
#' @param add Logical specifying whether to add to the current plot.
#' @param gridPos Position of grid lines either "over", "under" ("" for no grid
#'   lines).
#' @param gridCol Grid line color.
#' @param gridLwd Grid line width.
#' @param gridLty Grid line type.
#' @param dayLwd Day marker line width.
#' @param hourLwd Hour marker line width.
#' @param hourInterval Interval for grid (max = 12).
#' @param ... Additional arguments to be passed to \code{points()}.
#'
#' @note
#' Remember that a \emph{ws_monitor} object can contain data from more than one
#' monitor, and thus, this function may produce a time series of data from
#' multiple monitors. To plot a time series of an individual monitor's data,
#' specify a single \code{monitorID}.
#'
#' @import graphics
#' @export
#' @keywords ws_monitor
#'
#' @examples
#' N_M <- Northwest_Megafires
#' # monitor_leaflet(N_M) # to identify Spokane monitorIDs
#' Spokane <- monitor_subsetBy(
#'   N_M,
#'   stringr::str_detect(N_M$meta$monitorID, "^53063")
#' )
#'
#' monitor_timeseriesPlot(Spokane, style = "gnats")
#' title("Spokane PM2.5 values, 2015")
#' monitor_timeseriesPlot(
#'   Spokane,
#'   tlim = c(20150801, 20150831),
#'   style = "aqidots",
#'   pch = 16
#' )
#' addAQILegend()
#' title("Spokane PM2.5 values, August 2015")
#' monitor_timeseriesPlot(
#'   Spokane,
#'   tlim = c(20150821, 20150828),
#'   shadedNight = TRUE,
#'   style = "gnats"
#' )
#' abline(h = AQI$breaks_24, col = AQI$colors, lwd = 2)
#' addAQILegend()
#' title("Spokane PM2.5 values, August 2015")
monitor_timeseriesPlot <- function(ws_monitor,
                                   monitorID = NULL,
                                   tlim = NULL,
                                   localTime = TRUE,
                                   style = NULL,
                                   shadedNight = FALSE,
                                   add = FALSE,
                                   gridPos = "",
                                   gridCol = "black",
                                   gridLwd = 1,
                                   gridLty = "solid",
                                   dayLwd = 0,
                                   hourLwd = 0,
                                   hourInterval = 6,
                                   ...) {

  # For testing
  if ( FALSE ) {
    monitorID <- NULL
    tlim <- NULL
    localTime <- TRUE
    style <- NULL
    shadedNight <- FALSE
    add <- FALSE
    gridPos <- ""
    gridCol <- "black"
    gridLwd <- 1
    gridLty <- "solid"
    dayLwd <- 0
    hourLwd <- 0
    hourInterval <- 6
  }

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")

  # ----- Data Preparation ------------

  # Set monitorID if not passed in as argument
  if ( is.null(monitorID) ) {
    monitorID <- ws_monitor$meta$monitorID
  }

  # When tlim is specified in whole days we add hours to get the requsted full days
  if ( !is.null(tlim) ) {
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1], "00")
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2], "23")
    }
  }

  # Identify timezone(s)
  timezone <- unique(ws_monitor$meta[monitorID, "timezone"])

  # Force timezone to UTC and disable shadedNight if >1 timezone in metadata for monitorIDs
  if ( length(timezone) > 1 ) { # note that we will only enter this condition if localTime == TRUE
    if ( localTime ) {
      warning(">1 timezone in metadata for selected monitorIDs:  timezone forced to UTC")
      timezone <- "UTC"
    }
    if ( shadedNight ) {
      warning(">1 timezone in metadata for selected monitorIDs:  shadedNight disabled")
      shadedNight <- FALSE
    }
  }

  # Set timezone to UTC if localTime == FALSE
  if ( !localTime ) {
    timezone <- "UTC"
  }

  # Subset ws_monitor object by monitorID(s) and tlim, using timezone from above
  mon <- monitor_subset(ws_monitor, monitorIDs = monitorID, tlim = tlim, timezone = timezone)

  ## TODO: Consider fixing bug here where a monitor that has no valid data in
  #  the tlim (and thus will not be included in subset) will still force
  #  timezone = "UTC" and shadedNight = FALSE

  # Pull out meta and data
  meta <- mon$meta
  data <- mon$data

  # Pull out time data
  times <- lubridate::with_tz(data$datetime, tzone = timezone)

  # Cap hour interval
  if ( hourInterval > 12 ) {
    warning("Hour interval capped at 12 hours")
    hourInterval <- min(hourInterval, 12)
  }

  # ----- Args List ----------------------

  argsList <- list(...)

  argsList$x <- times
  argsList$y <- data[, 2] # TODO:  This is a problem when the monitor in this position has all NAs

  # set range for plotting
  if ( !("ylim" %in% names(argsList)) ) {
    ymin <- min(data[, -1], na.rm = TRUE)
    ymax <- max(data[, -1], na.rm = TRUE)
    buffer <- 0.04 * (ymax - ymin) # Standard R buffer around min/max
    argsList$ylim <- c(ymin - buffer, ymax + buffer)
  }

  if ( !("xlab" %in% names(argsList)) ) {
    if ( timezone == "UTC" ) {
      argsList$xlab <- "UTC"
    } else {
      argsList$xlab <- "Local Time"
    }
  }

  if ( !("ylab" %in% names(argsList)) ) {
    argsList$ylab <- "PM2.5"
  }

  if ( !("main" %in% names(argsList)) ) {
    argsList$main <- "Hourly PM2.5"
  }

  # ----- Args List for Blank ------------
  argsListBlank <- argsList

  argsListBlank$col <- "transparent"
  argsListBlank$axes <- FALSE
  argsListBlank$main <- NULL

  # ----- Plotting -----------------------

  # Base plot for background
  if ( !add ) {

    # Create blank plot
    do.call(plot, argsListBlank)

    # Shaded Night
    if ( shadedNight ) {
      lat <- mean(mon$meta$latitude)
      lon <- mean(mon$meta$longitude)
      timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
      addShadedNight(timeInfo)
    }

    # Add vertical lines to denote days and/or hour breaks
    hour_indices <-
      which(as.numeric(strftime(times, format = "%H", tz = timezone)) %% hourInterval == 0)
    day_indices <-
      which(as.numeric(strftime(times, format = "%H", tz = timezone)) %% 24 == 0)
    # NOTE:  Windows doesn't support lwd = 0
    if ( hourLwd > 0 ) {
      abline(v = times[hour_indices], lwd = hourLwd) # at beginning of hour
    }
    if ( dayLwd > 0 ) {
      abline(v = times[day_indices], lwd = dayLwd) # at beginning of day
    }

    # Add horizontal grid lines (before points if grid == "under")
    if ( gridPos == "under" && gridLwd > 0 ) {
      abline(h = axTicks(2)[-1], col = gridCol, lwd = gridLwd, lty = gridLty)
    }

    # Put a box around the plot area
    box()

    # Add axes
    axis(2, las = 1)

    # TODO: better x axis smarts, e.g. keep from saying "Monday, Tuesday" etc...
    axis.POSIXct(1, times)

  }

  # TODO: Add more style options

  # choose AQI breaks
  if ( !is.null(style) ) {

    if ( style == "aqidots" ) {

      userCex <- "cex" %in% names(argsList)

      # # Set opacity based on number of points
      # dims <- dim(as.matrix(data[, -1]))
      # naCount <- length(which(is.na(data[, -1])))
      # size <- dims[1]*dims[2] - naCount
      # opacity <- min(8/log(size), 1)
      opacity <- 1

      breaks <- AQI$breaks_24
      for (id in meta$monitorID) {
        argsList$y <- data[[id]] # same as data[, id]
        levels <- .bincode(argsList$y, breaks)
        argsList$col <- AQI$colors[levels]
        argsList$col <- adjustcolor(argsList$col, alpha.f = opacity)
        if ( !userCex ) {
          argsList$cex <- argsList$y / 200 + .3
          argsList$cex <- pmin(argsList$cex, 2)
        }
        # Add the points
        do.call(points, argsList)
      }

    } else if ( style == "gnats" ) {

      # Set opacity based on total number of valid measurements
      dims <- dim(as.matrix(data[, -1]))
      naCount <- length(which(is.na(data[, -1])))
      size <- dims[1] * dims[2] - naCount
      opacity <- min(1 / log2(size), 0.9)

      if ( !"col" %in% names(argsList) ) {
        baseColor <- "black"
      } else {
        baseColor <- argsList$col
      }

      for (id in meta$monitorID) {
        argsList$y <- data[[id]] # same as data[, id]
        argsList$col <- adjustcolor(baseColor, alpha.f = opacity)
        argsList$pch <- 15 # squares draw faster than circles
        # Add the points
        do.call(points, argsList)
      }

    }

  } else { # No 'style' specified

    for (id in meta$monitorID) {
      argsList$y <- data[[id]] # same as data[, id]
      do.call(points, argsList)
    }

  }

  # Add horizontal grid lines (on top of points if grid == "over")
  if ( gridPos == "over" && gridLwd > 0 ) {
    abline(h = axTicks(2)[-1], col = gridCol, lwd = gridLwd, lty = gridLty)
  }

}
