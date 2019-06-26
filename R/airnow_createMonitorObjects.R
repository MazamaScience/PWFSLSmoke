#' @keywords AirNow
#' @export
#' @import MazamaCoreUtils
#'
#' @title Obain AirNow data and create ws_monitor objects
#'
#' @param parameters vector of names of desired pollutants or NULL for all pollutants
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param hours desired number of hours of data to assemble
#' @param zeroMinimum logical specifying whether to convert negative values to zero
#' @param addGoogleMeta logicial specifying wheter to use Google elevation and reverse geocoding services
#' @return List where each element contains a \emph{ws_monitor} object for a unique parameter (e.g: "PM2.5", "NOX").
#' @description This function uses the \link{airnow_downloadParseData} function
#' to download monthly dataframes of AirNow data and restructures that data into a format that is compatible
#' with the PWFSLSmoke package \emph{ws_monitor} data model.
#'
#' AirNow data parameters include at least the following list:
#' \enumerate{
#' \item{BARPR}
#' \item{BC}
#' \item{CO}
#' \item{NO}
#' \item{NO2}
#' \item{NO2Y}
#' \item{NO2X}
#' \item{NOX}
#' \item{NOOY}
#' \item{OC}
#' \item{OZONE}
#' \item{PM10}
#' \item{PM2.5}
#' \item{PRECIP}
#' \item{RHUM}
#' \item{SO2}
#' \item{SRAD}
#' \item{TEMP}
#' \item{UV-AETH}
#' \item{WD}
#' \item{WS}
#' }
#'
#' Setting \code{parameters=NULL} will generate a separate \emph{ws_monitor} object for each of the above parameters.
#' @note As of 2017-12-17, it appears that hourly data are available only for 2016 and
#' not for earlier years.
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_createMetaDataframes}
#' @examples
#' \dontrun{
#' monList <- airnow_createMonitorObjects(c("PM2.5"), 20190625)
#' pm25 <- monList$PM2.5
#' o3 <- monList$O3
#' }

airnow_createMonitorObjects <- function(
  parameters = NULL,
  startdate = strftime(lubridate::now(), "%Y%m%d", tz = "UTC"),
  hours = 24,
  zeroMinimum = TRUE,
  addGoogleMeta = TRUE
) {

  logger.debug(" ----- airnow_createMonitorObjects() ----- ")

  metaList <- airnow_createMetaDataframes(parameters, 'AIRNOW', addGoogleMeta=addGoogleMeta)
  dataList <- airnow_createDataDataframes(parameters, startdate, hours)

  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  monList <- list()

  for ( parameter in names(metaList) ) {

    # Create the 'ws_monitor' object
    meta <- metaList[[parameter]]
    data <- dataList[[parameter]]
    ws_monitor <- list(meta=meta, data=data)
    # Guarantee that meta rows match data cols
    ws_monitor <- monitor_subset(ws_monitor, countryCodes = c('CA','US','MX'))
    ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
    # Reset all negative values that made it through QC to zero
    if ( zeroMinimum ) {
      logger.trace("Reset negative values to zero ...")
      ws_monitor <- monitor_replaceData(ws_monitor, data < 0, 0)
    }
    # Add to list
    monList[[parameter]] <- ws_monitor

  }

  return(monList)

}

