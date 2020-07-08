#' @keywords ws_monitor
#'
#' @title Collapse a ws_monitor Object into a ws_monitor Object with a Single
#' Monitor
#'
#' @param ws_monitor \emph{ws_monitor} object.
#' @param longitude Longitude of the collapsed monitoring station.
#' @param latitude Latitude of the collapsed monitoring station.
#' @param monitorID Monitor ID assigned to the collapsed monitoring station.
#' @param FUN Function to be applied to all the monitors at a single time index.
#' @param na.rm Logical specifying whether NA values should be ignored when FUN
#' is applied.
#' @param ... additional arguments to be passed on to the \code{apply()} function.
#'
#' @return A \emph{ws_monitor} object with meta and data that for the the
#' collapsed single monitor
#'
#' @description
#' Collapses data from all the monitors in \code{ws_monitor} into a
#' single-monitor \emph{ws_monitor} object using the function provided in the
#' \code{FUN} argument. The single-monitor result will be located at the mean
#' longitude and latitude unless \code{longitude} and \code{latitude}
#' parameters are specified.
#'
#' Any columns of meta that are common to all monitors will be retained in
#' the returned \emph{ws_monitor} meta.
#'
#' @note
#' After \code{FUN} is applied, values of \code{+Inf} and \code{-Inf} are
#' converted to \code{NA}. This is a convenience for the common case where
#' \code{FUN=min} or \code{FUN=max} and some of the timesteps have all missing
#' values. See the R documentation for \code{min} for an explanation.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#' library(PWFSLSmoke)
#'
#' N_M <- Northwest_Megafires
#' # monitor_leaflet(N_M) # to identify Spokane monitorIDs
#'
#' Spokane <- monitor_subsetBy(N_M, stringr::str_detect(N_M$meta$monitorID,'^53063'))
#' Spokane_min <- monitor_collapse(Spokane, monitorID='Spokane_min', FUN=min)
#' Spokane_max <- monitor_collapse(Spokane, monitorID='Spokane_max', FUN=max)
#'
#' monitor_timeseriesPlot(Spokane, tlim=c(20150619,20150626),
#'                        style='gnats', shadedNight=TRUE)
#' monitor_timeseriesPlot(Spokane_max, col='red', type='s', add=TRUE)
#' monitor_timeseriesPlot(Spokane_min, col='blue', type='s', add=TRUE)
#' title('Spokane Range of PM2.5 Values, June 2015')

monitor_collapse <- function(ws_monitor,
                             longitude = NULL,
                             latitude = NULL,
                             monitorID = "generated_id",
                             FUN = mean,
                             na.rm = TRUE,
                             ...) {

  # Debugging ------------------------------------------------------------------

  if ( FALSE ) {

    longitude <- -122.392
    latitude <- 41.432
    monitorID <- "generated_id"
    FUN <- mean
    na.rm <- TRUE
    ws_monitor <-  airsis_loadAnnual(2018) %>%
      monitor_subsetByDistance(lon, lat, radius = 10)

    lon <- -122.392
    lat <- 41.432
    Weed <-  airsis_loadAnnual(2018) %>%
      monitor_subsetByDistance(lon, lat, radius = 10) %>%
      monitor_collapse(lon, lat, monitorID = "Weed_combined") %>%
      monitor_trim()

    str(Weed$meta)

  }

  # Validate parameters --------------------------------------------------------

  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }

  # Collapse data --------------------------------------------------------------

  data <- as.matrix(ws_monitor$data[,-1])

  collapsed_data <- suppressWarnings({
    apply(data, MARGIN = 1, FUN = FUN, na.rm = na.rm, ...)
  })

  # Special handling for min and max which return +Inf and -Inf when any row has all NAs
  collapsed_data[collapsed_data == +Inf] <- NA
  collapsed_data[collapsed_data == -Inf] <- NA

  newData <- data.frame(ws_monitor$data$datetime, collapsed_data)
  colnames(newData) <- c('datetime', monitorID)

  # Generate meta --------------------------------------------------------------

  meta <- ws_monitor$meta

  if ( !is.null(longitude) & !is.null(latitude) ) {
    newLat <- latitude
    newLon <- longitude
  } else {
    newLat <- mean(meta$latitude)
    newLon <- mean(meta$longitude)
  }

  timezone <-  MazamaSpatialUtils::getTimezone(newLon, newLat, useBuffering = TRUE)

  # Build new meta dataframe
  newMeta <- data.frame(monitorID = monitorID,
                        longitude = newLon,
                        latitude = newLat,
                        timezone = timezone,
                        stringsAsFactors = FALSE)

  rownames(newMeta) <- newMeta$monitorID

  # Add all other metadata columns, retaining values that are shared
  columns <- setdiff(colnames(meta), colnames(newMeta))
  for ( column in columns ) {
    if ( length(unique(meta[[column]])) == 1 ) {
      newMeta[[column]] <- unique(meta[[column]])
    } else {
      newMeta[[column]] <- as(NA, class(meta[[column]]))
    }
  }

  # Return ---------------------------------------------------------------------

  ws_monitor <- list(meta = newMeta, data = newData)

  return(structure(ws_monitor, class = c("ws_monitor", "list")))

}

