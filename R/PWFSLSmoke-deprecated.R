#' @title Deprecated functions in PWFSLSmoke
#'
#' @description
#' These functions are in the process of being removed from the PWFSLSmoke
#' package, and may not appear in future versions. When available, alternative
#' functions will be listed.
#'
#' @details
#' Deprecations fall into the following categories:
#'
#' \itemize{
#'   \item Silent: These functions immediately call another function without any
#'   warning to the user.
#'   \item Soft: These functions throw a warning when called, then call the
#'   correct function, if available.
#'   \item Hard: These functions will throw an error when called, stopping
#'   execution. If an alternative function exists, it will be listed.
#' }
#'
#' @section Silent Deprecations:
#'
#' \tabular{rl}{
#'  \code{\link{monitorDygraph}} \tab Use \code{\link{monitor_dygraph}}\cr
#'  \code{\link{monitor_esriMap}} \tab Use \code{\link{monitor_staticmap}}\cr
#'  \code{\link{monitorLeaflet}} \tab Use \code{\link{monitor_leaflet}}\cr
#'  \code{\link{monitorMap}} \tab Use \code{\link{monitor_map}}\cr
#'  \code{\link{monitorMap_performance}} \tab Use \code{\link{monitor_performanceMap}}\cr
#'  \code{\link{monitorPlot_dailyBarplot}} \tab Use \code{\link{monitor_dailyBarplot}}\cr
#'  \code{\link{monitorPlot_hourlyBarplot}} \tab Use \code{\link{monitor_hourlyBarplot}}\cr
#'  \code{\link{monitorPlot_noData}} \tab Use \code{\link{monitor_noDataPlot}}\cr
#'  \code{\link{monitorPlot_rollingMean}} \tab Use \code{\link{monitor_rollingMeanPlot}}\cr
#'  \code{\link{monitorPlot_timeseries}} \tab Use \code{\link{monitor_timeseriesPlot}}\cr
#' }
#'
#' @section Hard Deprecations:
#'
#' \tabular{rl}{
#'  \code{\link{monitor_esriMap}} \tab Use \code{\link{monitor_staticmap}}\cr
#'  \code{\link{monitorEsriMap}} \tab Use \code{\link{monitor_staticmap}}\cr
#'  \code{\link{monitorGoogleMap}} \tab Use \code{\link{monitor_staticmap}}\cr
#'  \code{\link{monitorPlot_timeOfDaySpaghetti}} \tab No alternative\cr
#' }
#'
#' @aliases PWFSLSmoke-defunct monitorDygraph monitorEsriMap monitorLeaflet
#'   monitorMap monitorMap_performance monitorGoogleMap
#' @name PWFSLSmoke-deprecated
#' @keywords internal
NULL

# hard deprecations -------------------------------------------------------

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorGoogleMap <- function(
  ws_monitor,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  width = 640,
  height = 640,
  centerLon = NULL,
  centerLat = NULL,
  zoom = NULL,
  maptype = "roadmap",
  grayscale = FALSE,
  map = NULL,
  ...
) {
  .Defunct("monitor_esriMap", package = "PWFSLSmoke")
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorEsriMap <- function(
  ws_monitor,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  width = 640,
  height = 640,
  centerLon = NULL,
  centerLat = NULL,
  zoom = NULL,
  maptype = "roadmap",
  grayscale = FALSE,
  map = NULL,
  ...
) {
  .Defunct("monitor_staticmap", package = "PWFSLSmoke")
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_timeOfDaySpaghetti <- function(
  ws_monitor,
  monitorID=NULL,
  tlim=NULL,
  ylim=NULL,
  aqiLines=TRUE,
  shadedNight=TRUE,
  title=NULL,
  ...
) {
  .Defunct(NULL, package = "PWFSLSmoke")
}


# soft deprecations -------------------------------------------------------




# silent deprecations -----------------------------------------------------

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorDygraph <- function(
  ws_monitor,
  title = "title",
  ylab = "PM2.5 Concentration",
  tlim = NULL,
  rollPeriod = 1,
  showLegend = TRUE
) {
  monitor_dygraph(ws_monitor, title, ylab, tlim, rollPeriod, showLegend)
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitor_esriMap <- function(
  ws_monitor,
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
  ...
) {
  monitor_staticmap(
    ws_monitor, slice, breaks, colors, width, height,
    centerLon, centerLat, zoom, maptype = "world_topo", grayscale, mapRaster,
    cex, pch, ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorLeaflet <- function(
  ws_monitor,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  labels = AQI$names,
  legendTitle = "Max AQI Level",
  radius = 10,
  opacity = 0.7,
  maptype = "terrain",
  popupInfo = c("siteName", "monitorID", "elevation")
) {
  monitor_leaflet(
    ws_monitor, slice, breaks, colors, labels, legendTitle,
    radius, opacity, maptype, popupInfo
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorMap <- function(
  ws_monitor,
  slice = get("max"),
  breaks = AQI$breaks_24,
  colors = AQI$colors,
  pch = par("pch"),
  cex = par("cex"),
  stateCol = "grey60",
  stateLwd = 2,
  countyCol = "grey70",
  countyLwd = 1,
  add = FALSE,
  ...
) {
  monitor_map(
    ws_monitor, slice, breaks, colors, pch, cex,
    stateCol, stateLwd, countyCol, countyLwd, add,
    ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorMap_performance <- function(
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
  monitor_performanceMap(
    predicted, observed, threshold, cex, sizeBy, colorBy, breaks,
    paletteFunc, showLegend, legendPos, stateCol, stateLwd,
    countyCol, countyLwd, add,
    ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_dailyBarplot <- function(
  ws_monitor,
  monitorID=NULL,
  tlim=NULL,
  minHours=18,
  gridPos='',
  gridCol='black',
  gridLwd=0.5,
  gridLty='solid',
  labels_x_nudge=0,
  labels_y_nudge=0,
  ...
) {
  monitor_dailyBarplot(
    ws_monitor, monitorID, tlim, minHours,
    gridPos, gridCol, gridLwd, gridLty,
    labels_x_nudge, labels_y_nudge,
    ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_hourlyBarplot <- function(
  ws_monitor,
  monitorID = NULL,
  tlim = NULL,
  localTime = TRUE,
  style = 'AQI',
  shadedNight = TRUE,
  gridPos = '',
  gridCol = 'black',
  gridLwd = 0.5,
  gridLty = 'solid',
  labels_x_nudge = 0,
  labels_y_nudge = 0,
  dayCol = 'black',
  dayLwd = 2,
  dayLty = 'solid',
  hourCol = 'black',
  hourLwd = 1,
  hourLty = 'solid',
  hourInterval = 6,
  ...
) {
  monitor_hourlyBarplot(
    ws_monitor, monitorID, tlim, localTime, style, shadedNight,
    gridPos, gridCol, gridLwd, gridLty, labels_x_nudge, labels_y_nudge,
    dayCol, dayLwd, dayLty, hourCol, hourLwd, hourLty, hourInterval,
    ...
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_noData <- function(ws_monitor, monitorID=NULL, cex=2.5) {
  monitor_noDataPlot(ws_monitor, monitorID, cex)
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_rollingMean <- function(
  ws_monitor,
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
  showLegend=TRUE
) {
  monitor_rollingMeanPlot(
    ws_monitor, monitorID, width, align, data.thresh, tlim, ylim, localTime,
    shadedNight, aqiLines, gridHorizontal, grid24hr, grid3hr, showLegend
  )
}

#' @export
#' @rdname PWFSLSmoke-deprecated
monitorPlot_timeseries <- function(
  ws_monitor,
  monitorID=NULL,
  tlim=NULL,
  localTime=TRUE,
  style=NULL,
  shadedNight=FALSE,
  add=FALSE,
  gridPos='',
  gridCol='black',
  gridLwd=1,
  gridLty='solid',
  dayLwd=0,
  hourLwd=0,
  hourInterval=6,
  ...
) {
  monitor_timeseriesPlot(
    ws_monitor, monitorID, tlim, localTime, style, shadedNight, add,
    gridPos, gridCol, gridLwd, gridLty, dayLwd, hourLwd, hourInterval, ...
  )
}
