#' @keywords ws_monitor
#' @export
#' @import maps mapproj
#'
#' @title Create map of monitor prediction performance
#'
#' @param predicted ws_monitor object with predicted values
#' @param observed ws_monitor object with observed  values
#' @param threshold value used to classify \code{predicted} and \code{observed}
#'   measurements
#' @param cex the amount that the points will be magnified on the map
#' @param sizeBy name of the metric used to create relative sizing
#' @param colorBy name of the metric used to create relative colors
#' @param breaks set of breaks used to assign colors or a single integer used to
#'   provide quantile based breaks - Must also specify the colorBy paramater
#' @param paletteFunc a palette generating function as returned by
#'   \code{colorRampPalette}
#' @param showLegend logical specifying whether to add a legend (default:
#'   \code{TRUE})
#' @param legendPos legend position passed to \code{legend()}
#' @param stateCol color for state outlines on the map
#' @param stateLwd width for state outlines
#' @param countyCol color for county outline on the map
#' @param countyLwd width for county outlines
#' @param add logical specifying whether to add to the current plot
#' @param ... additional arguments to be passed to the \code{maps::map()}
#'   funciton such as graphical parameters (see code{?par})
#'
#' @description
#' This function uses \emph{confusion matrix} analysis to calculate different
#' measures of predictive performance for every timeseries found in
#' \code{predicted} with respect to the observed values found in the single
#' timeseries found in \code{observed}.
#'
#' Using a single number for the \code{breaks} argument will cause the algorithm
#' to use quantiles to determine breaks.
#'
#' @details
#' Setting either \code{sizeBy} or \code{colorBy} to \code{NULL} will cause the
#' size/colors to remain constant.
#'
#' @seealso \link{monitor_performance}
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(PWFSLSmoke)
#'
#' # Napa Fires -- October, 2017
#' ca <- airnow_load(2017) %>%
#'   monitor_subset(tlim=c(20171001,20171101), stateCodes='CA')
#' Vallejo <- monitor_subset(ca, monitorIDs='060950004_01')
#' Napa_Fires <- monitor_subsetByDistance(ca,
#'                                        longitude = Vallejo$meta$longitude,
#'                                        latitude = Vallejo$meta$latitude,
#'                                        radius = 50)
#' monitor_performanceMap(ca, Vallejo, cex = 2)
#' title('Heidke Skill of monitors predicting another monitor.')
#'
#' }, silent = FALSE)
#' }

monitor_performanceMap <- function(
  predicted,
  observed,
  threshold = AQI$breaks_24[3],
  cex = par("cex"),
  sizeBy = NULL,
  colorBy = "heidikeSkill",
  breaks = c(-Inf, .5, .6, .7, .8, Inf),
  paletteFunc = grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(
      length(breaks), "Purples"
    )[-1]
  ),
  showLegend = TRUE,
  legendPos = "topright",
  stateCol = "grey60",
  stateLwd = 2,
  countyCol = "grey70",
  countyLwd = 1,
  add = FALSE,
  ...
) {


  # below copies from monitor_performanceMap -----------------------------------

  # Get the performance dataframe
  performanceDF <- monitor_performance(predicted, observed, threshold, threshold)

  # Create the basemap
  if ( !add ) {

    stateCodes <- unique(predicted$meta$stateCode)

    if ( is.null(stateCodes) || all(stateCodes == "") ) {

      # No stateCodes found. Use xlim and ylim.
      xlim <- range(predicted$meta$longitude)
      ylim <- range(predicted$meta$latitude)
      # add a 1 degree buffer
      xlim[1] <- xlim[1] - 1.0
      xlim[2] <- xlim[2] + 1.0
      ylim[1] <- ylim[1] - 1.0
      ylim[2] <- ylim[2] + 1.0
      # Plot the base map: state first, counties on top
      maps::map(
        "state",
        xlim = xlim, ylim = ylim,
        col = stateCol, lwd = stateLwd,
        ...
      )
      maps::map(
        "county",
        xlim = xlim, ylim = ylim,
        col = countyCol, lwd = countyLwd,
        add = TRUE, ...
      )

    } else {

      # Plot only the states containing the monitors
      # Need to get the maps package state names to be plotted in base map
      # We need to deal with things like "washington:whidbey island"
      stateCodes <- as.character(stateCodes)
      df <- maps::state.fips
      df$polyname <- stringr::str_replace(df$polyname, ":.*", "")
      df <- df %>% dplyr::filter(df$abb %in% stateCodes) %>% dplyr::distinct()
      stateNames <- df$polyname
      # Plot the base map: state first, counties on top
      maps::map("state", stateNames, col = stateCol, lwd = stateLwd, ...)
      maps::map("county", stateNames, col = countyCol, lwd = countyLwd, add = TRUE, ...)

    }

  }

  # Sizing
  if ( !is.null(sizeBy) && sizeBy %in% names(performanceDF) ) {
    cex <- cex * performanceDF[[sizeBy]] / max(performanceDF[[sizeBy]], na.rm = TRUE)
  }

  # This chunk  of code figures out colors for the map.
  if ( !is.null(colorBy) && colorBy %in% names(performanceDF) ) {
    if ( length(breaks) == 1 ) {
      probs <- seq(0, 1, length.out = (breaks + 1))
      breaks <- stats::quantile(performanceDF[[colorBy]], probs = probs, na.rm = TRUE)
    }
    # TODO:  Use aqiColors() to do this?
    indices <- .bincode(performanceDF[[colorBy]], breaks = breaks, include.lowest = TRUE)
    colors <- paletteFunc((length(breaks) - 1))
    cols <- colors[indices]

    # create a legend to be used later
    legend <- character(length(breaks) - 1)
    for (i in 1:(length(breaks) - 1)) {
      legend[i] <- paste0( round(breaks[i], 2), "-", round(breaks[i + 1], 2))
    }
  } else {
    colors <- cols <- "black"
  }

  lon <- predicted$meta$longitude
  lat <- predicted$meta$latitude

  # Now we add the (potentially projected) monitor points
  argsList <- list(...)

  if ( is.null(argsList$projection) ) {
    points(lon, lat, pch = 16, cex = cex, col = cols, xpd = NA)
  } else {
    points(mapproj::mapproject(lon, lat, argsList$projection, argsList$parameters,
                               argsList$orientation), pch = 16, cex = cex, col = cols, xpd = NA)
  }

  # # if neither colorBy nor sizeBy is specified, there is nothing to show in the legend
  # if ( is.null(colorBy) & is.null(sizeBy) ) {showlegend = FALSE}
  #
  # # if colorBy and/or sizeBy are/is specified, show the color legend
  # # else if only sizeBy is specified, show the size legend
  # if (showLegend) {
  #   if( !is.null(colorBy) ) {
  #     legend( "topright", cex=cex*0.5, col=rev(colorss), legend=rev(legend), title=paste0(colorBy, " levels") )
  #   } else {
  #     legend( "topright", pt.cex=cex*0.5, col="black", legend=rev(sizeLegend), title=paste0(sizeBy, " levels") )
  #   }
  # }
  if ( showLegend & !is.null(colorBy) ) {
    legend(
      legendPos,
      pch = 16,
      cex = cex * 0.5,
      col = rev(colors),
      legend = rev(legend),
      title = paste0(colorBy, " levels")
    )
  }

}
