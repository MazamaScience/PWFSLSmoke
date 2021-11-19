#' @keywords ws_monitor
#' @export
#' @title Leaflet interactive map of monitoring stations
#' @param ws_monitor ws_monitor object
#' @param slice either a time index or a function used to collapse the time axis
#'   -- defautls to \code{get('max')}
#' @param breaks set of breaks used to assign colors
#' @param colors a set of colors for different levels of air quality data
#'   determined by \code{breaks}
#' @param labels a set of text labels, one for each color
#' @param legendTitle legend title
#' @param radius radius of monitor circles
#' @param opacity opacity of monitor circles
#' @param maptype optional name of leaflet ProviderTiles to use, e.g. "terrain"
#' @param popupInfo a vector of column names from ws_monitor$meta to be shown in
#'   a popup window
#'
#' @description
#' This function creates interactive maps that will be displayed in RStudio's
#' 'Viewer' tab. The \code{slice} argument is used to collapse a
#' \emph{ws_monitor} timeseries into a single value. If \code{slice} is an
#' integer, that row index will be selected from the \code{ws_monitor$data}
#' dataframe. If \code{slice} is a function (unquoted), that function will be
#' applied to the timeseries with the argument \code{na.rm=TRUE} (e.g.
#' \code{max(..., na.rm=TRUE)}).
#'
#' If \code{slice} is a user defined function it will be used with argument
#' \code{na.rm=TRUE} to collapse the time dimension. Thus, user defined
#' functions must accept \code{na.rm} as an argument.
#'
#' @details
#' The \code{maptype} argument is mapped onto leaflet "ProviderTile" names.
#' Current mappings include: \enumerate{ \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"} \item{"terrain"}{ --
#' "Esri.WorldTopoMap"} \item{"toner"}{ -- "Stamen.Toner"} }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @return Invisbly returns a leaflet map of class "leaflet".
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # Napa Fires -- October, 2017
#' ca <- airnow_load(2017) %>%
#'   monitor_subset(tlim = c(20171001,20171101), stateCodes = 'CA')
#' v_low <- AQI$breaks_24[5]
#' CA_very_unhealthy_monitors <- monitor_subset(ca, vlim = c(v_low, Inf))
#' monitor_leaflet(CA_very_unhealthy_monitors,
#'                legendTitle = "October, 2017",
#'                maptype = "toner")
#'
#' }, silent = FALSE)
#' }

monitor_leaflet <- function(
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

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }

  # ----- Create the 'slice' ---------------------------------------------------

  if ( class(slice) == "function" ) {

    # NOTE:  min/max will return Inf/-Inf when all data are missing while mean
    # NOTE:  returns NaN so we need to replace all those with NA.
    tempTbl <- dplyr::summarise_all(ws_monitor$data, slice, na.rm = TRUE)
    missingMask <- ! as.numeric(dplyr::summarise_all(tempTbl, is.finite))
    tempTbl[missingMask] <- NA
    pm25 <- as.numeric(tempTbl[-1])

  } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {

    pm25 <- as.numeric(dplyr::slice(ws_monitor$data, slice)[-1])

  } else {

    stop("Improper use of slice parameter")

  }

  # ----- Create colors and legend labels --------------------------------------

  # If the user didn't use custom breaks then use AQI names and colors
  if ( all.equal(breaks, AQI$breaks_24) && all.equal(colors, AQI$colors) ) {

    # Ignore warnings from RColorBrewer as leaflet::colorBin does the right thing
    suppressWarnings({
      colorFunc <- leaflet::colorBin(PWFSLSmoke::AQI$colors,
                                     bins = PWFSLSmoke::AQI$breaks_24,
                                     na.color = "#bbbbbb")
      cols <- colorFunc(pm25)
      colors <- PWFSLSmoke::AQI$colors
      labels <- PWFSLSmoke::AQI$names
      legendTitle <- 'AQI Level'
      value <- round(pm25, 1)
      unit <- '\U00B5g/m3'
    })

  } else {

    if ( length(breaks) <= 2) {
      stop("Please specify the correct vector of breaks")
    }

    if (! (length(breaks) - 1 == length(colors)) ) {
      stop("The number of colors provided should be one less than the number of breaks")
    }

    if ( missing(labels) ){
      labels <- paste(sprintf("%.1f", breaks[-length(breaks)]),
                      "--",
                      sprintf("%.1f", breaks[-1]))
    } else if ( length(labels) != length(colors) ) {
      stop("The number of labels should be equal to the number of colors")
    }

    # Create levels and use them to create a color mask
    levels <- .bincode(pm25, breaks, include.lowest = TRUE)
    if ( !all(!is.na(levels)) ) {
      print("NOTE that there are data points outside of your specified breaks, non-requested color(s) might be displayed on your map.")
    }
    cols <- colors[levels]

  }

  # ----- Create popup ---------------------------------------------------------

  # If there are no column names in the popupInfo, then don't make descriptions
  if ( !any(popupInfo %in% names(ws_monitor$meta)) ) {
    popupInfo <- NULL
  }

  # Create popup text vector
  popupText <- character()
  if ( !is.null(popupInfo) ) {
    for ( rowIndex in seq_len(nrow(ws_monitor$meta)) ) {
      monitorText <- ""
      for ( colName in popupInfo ) {
        if ( colName %in% names(ws_monitor$meta) ) {
          value <- ws_monitor$meta[rowIndex, colName]
          if ( is.numeric(value) ) {
            monitorText <- paste0(monitorText, colName, ": ", signif(value, 6), "<br>")
          } else {
            monitorText <- paste0(monitorText, colName, ": ", value, "<br>")
          }
        }
      }
      monitorText <- paste0(monitorText, "pm25: ",
                            round(pm25[rowIndex], 1),
                            "<br>")
      popupText[rowIndex] <- monitorText
    }
  }
  ws_monitor$meta$popupText <- popupText


  # ----- Create map -----------------------------------------------------------

  # Determine appropriate zoom level
  lonRange <- range(ws_monitor$meta$longitude, na.rm = TRUE)
  latRange <- range(ws_monitor$meta$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange), diff(latRange), na.rm = TRUE)

  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # Convert locations to SpatialPointsDataFrame
  ws_monitor$meta <- ws_monitor$meta[!is.na(ws_monitor$meta$latitude), ]
  SPDF <- sp::SpatialPointsDataFrame(
    coords = cbind(ws_monitor$meta$longitude, ws_monitor$meta$latitude),
    data = ws_monitor$meta
  )

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == "terrain") {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # Create leaflet map
  leafletMap <- leaflet::leaflet(SPDF) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius = radius,
      fillColor = cols,
      fillOpacity = opacity,
      stroke = FALSE,
      popup = ws_monitor$meta$popupText) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = rev(colors), # show low levels at the bottom
      labels = rev(labels),  # show low levels at the bottom
      opacity = 1,
      title = legendTitle
    )

  return(leafletMap)

}
