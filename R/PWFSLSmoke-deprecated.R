#' @title Deprecated functions in PWFSLSmoke
#'
#' @description
#' These functions still are in the process of being removed from the PWFSLSmoke
#' package, and may not appear in future versions. When available, alternative
#' functions will be listed.
#'
#' @details
#' Deprecations fall into the following categories:
#'
#' \itemize{
#'   \item Silent: These functions immediately call another function without and
#'   warning to the user.
#'   \item Soft: These functions throw a warning when called, then call the
#'   correct function, if available.
#'   \item Hard: These functions will throw an error when called, stopping
#'   execution. If an alternative function exists, it will be listed.
#' }
#'
#' @section Silent Deprecations:
#'
#' \itemize{
#'  \item \code{\link{monitorDygraph}}: Use \code{\link{monitor_dygraph}}
#'  \item \code{\link{monitorEsriMap}}: Use \code{\link{monitor_esriMap}}
#'  \item \code{\link{monitorLeaflet}}: Use \code{\link{monitor_leaflet}}
#'  \item \code{\link{monitorMap}}: Use \code{\link{monitor_map}}
#'  \item \code{\link{monitorMap_performance}}: Use \code{\link{monitor_performanceMap}}
#' }
#'
#' @section Soft Deprecations:
#'
#'
#' @section Hard Deprecations:
#'
#'
#'
#' @name PWFSLSmoke-deprecated
NULL

# hard deprecations -------------------------------------------------------




# soft deprecations -------------------------------------------------------




# silent deprecations -----------------------------------------------------

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorDygraph <- function(ws_monitor,
                           title = "title",
                           ylab = "PM2.5 Concentration",
                           tlim = NULL,
                           rollPeriod = 1,
                           showLegend = TRUE) {

  monitor_dygraph(ws_monitor, title, ylab, tlim, rollPeriod, showLegend)
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorEsriMap <- function(ws_monitor,
                           slice = get("max"),
                           breaks = AQI$breaks_24,
                           colors = AQI$colors,
                           width = 640,
                           height = 640,
                           centerLon = NULL,
                           centerLat = NULL,
                           zoom = NULL,
                           maptype = "worldStreetMap",
                           grayscale = FALSE,
                           mapRaster = NULL,
                           cex = par("cex") * 2.0,
                           pch = 16,
                           ...) {

  monitor_esriMap(
    ws_monitor, slice, breaks, colors, width, height,
    centerLon, centerLat, zoom, maptype, grayscale, mapRaster,
    cex, pch, ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorLeaflet <- function(ws_monitor,
                           slice = get("max"),
                           breaks = AQI$breaks_24,
                           colors = AQI$colors,
                           labels = AQI$names,
                           legendTitle = "Max AQI Level",
                           radius = 10,
                           opacity = 0.7,
                           maptype = "terrain",
                           popupInfo = c("siteName", "monitorID", "elevation")) {

  monitor_leaflet(
    ws_monitor, slice, breaks, colors, labels, legendTitle,
    radius, opacity, maptype, popupInfo
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorMap <- function(ws_monitor,
                       slice = get("max"),
                       breaks = AQI$breaks_24,
                       colors = AQI$colors,
                       cex = par("cex"),
                       stateCol = "grey60",
                       stateLwd = 2,
                       countyCol = "grey70",
                       countyLwd = 1,
                       add = FALSE,
                       ...) {

  monitor_map(
    ws_monitor, slice, breaks, colors, cex,
    stateCol, stateLwd, countyCol, countyLwd, add,
    ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorMap_performance <- function(predicted,
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
                                   ...) {

  monitor_performanceMap(
    predicted, observed, threshold, cex, sizeBy, colorBy, breaks,
    paletteFunc, showLegend, legendPos, stateCol, stateLwd,
    countyCol, countyLwd, add,
    ...
  )
}
