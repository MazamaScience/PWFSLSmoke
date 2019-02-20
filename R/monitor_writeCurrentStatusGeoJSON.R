#' @title Write Current Monitor Data to GeoJSON
#'
#' @description
#' Writes a geoJSON file containing current monitor data. For details on what is
#' included, see \code{\link{monitor_getCurrentStatus}}.
#'
#' @param ws_monitor \emph{ws_monitor} object.
#' @param filename Filename where geojson file will be saved.
#' @param datetime Time to which data will be 'current' (integer or character
#'   representing YYYYMMDDHH or \code{POSIXct}. If not \code{POSIXct},
#'   interpreted as UTC time). So if \code{datetime} is 3 hours ago, a dataframe
#'   with the most current data from 3 hours ago will be returned.
#' @param properties Optional character vector of properties to include for each
#'   monitor in geoJSON. If NULL all are included. May include any ws_monitor
#'   metadata and additional columns generated in
#'   \code{\link{monitor_getCurrentStatus}}.
#' @param propertyNames Optional character vector supplying custom names for
#'   properties in geoJSON. If NULL or different length than \code{properties}
#'   defaults will be used.
#' @param metadataList List of top-level foreign members to include. May include
#'   nested lists as long as they can be converted into JSON using
#'   \code{jsonlite::toJSON()}. For more information on what can be included see
#'   \href{https://tools.ietf.org/html/rfc7946#section-6.1}{https://tools.ietf.org/html/rfc7946#section-6.1}.
#'
#' @return Invisibly returns geoJSON string.
#'
#' @keywords ws_monitor
#' @export
#'
#' @examples
#' \dontrun{
#' wa <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
#' wa_current_geojson <- monitor_writeCurrentStatusGeoJSON(wa, "wa_monitors.geojson")
#' wa_current_list <- jsonlite::fromJSON(wa_current_geojson)
#' wa_spdf <- rgdal::readOGR(dsn = "wa_monitors.geojson", layer = "OGRGeoJSON")
#' map("state", "washington")
#' points(wa_spdf)
#' }
monitor_writeCurrentStatusGeoJSON <- function(ws_monitor,
                                              filename,
                                              datetime = lubridate::now("UTC"),
                                              properties = NULL,
                                              propertyNames = NULL,
                                              metadataList = list()) {


# Sanity checks -----------------------------------------------------------

  if (monitor_isEmpty(ws_monitor)) stop("ws_monitor object contains zero monitors.")


# Load and format data ----------------------------------------------------

  result <- try({
    currentTbl <- monitor_getCurrentStatus(ws_monitor, datetime)
  }, silent = TRUE)
  if ("try-error" %in% class(result)) {
    err_msg <- geterrmessage()
    stop(paste0("error getting current data: ", err_msg))
  }

  ## NOTE:
  #  For highest values to be plotted last on Leaflet Maps we want monitors with
  #  NA's to come first and then everything in increasing order.

  if (!is.null(currentTbl$yesterday_pm25_24hr)) {
    myOrder <- rev(order(currentTbl$yesterday_pm25_24hr, decreasing = TRUE))
    currentTbl <- currentTbl[myOrder, ]
  }

  # Use monitorID as siteName if siteName is missing
  currentTbl$siteName <- ifelse(
    is.na(currentTbl$siteName),
    currentTbl$monitorID,
    currentTbl$siteName
  )


# Get coords from meta and create SPDF ------------------------------------

  # NOTE: To save space we always round to 5 decimal places (~1 meter)
  # NOTE: Get coords before subsetting meta, in case lon and lat aren't included
  coords <- cbind(round(currentTbl$longitude, 5), round(currentTbl$latitude, 5))

  # Subset meta to desired properties
  if (!is.null(properties)) {
    currentTbl <- currentTbl[, properties]
    if (!is.null(propertyNames) & length(propertyNames) == length(properties))
      result <- try(names(currentTbl) <- propertyNames)
      if ("try-error" %in% class(result)) {
        err_msg <- geterrmessage()
        warning(paste0("could not change propertyNames - using defaults: ", err_msg))
      }
  }

  # Create the SpatialPointsDataFrame
  # NOTE:  We use as.data.frame because use of dplyr:: functions adds the "tbl_df" class
  sites <- sp::SpatialPointsDataFrame(
    coords = coords,
    proj4string = sp::CRS("+proj=longlat +ellps=WGS84"),
    data = as.data.frame(currentTbl)
  )


# Convert to geoJSON ------------------------------------------------------

  ## NOTE: about rgdal::writeOGR
  #  * creates .geojson files where numeric properties are not quoted.
  #  * complains if filename already exists.
  #  * does not automatically append '.geojson'

  # filename <- paste0(opt$outputDir,'/',opt$dataSource,'_PM2.5_latest10.geojson')
  # logger.info('Using rgdal::writeOGR to create geojson file %s', filename)

  rgdal::writeOGR(sites, dsn = filename, layer = "currentTbl", driver = "GeoJSON")

  # Add top level metadata as Foreign Members:  https://tools.ietf.org/html/rfc7946#section-6.1
  # and rewrite the geojson file
  geojsonList <- as.list(
    jsonlite::fromJSON(
      filename,
      simplifyVector = TRUE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE,
      flatten = FALSE
    )
  )

  geojsonList <- append(geojsonList, metadataList)
  geojsonText <- jsonlite::toJSON(
    geojsonList,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  cat(geojsonText, file = filename)

  return(invisible(geojsonText))

}
