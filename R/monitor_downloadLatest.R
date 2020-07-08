#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Download recent PM2.5 monitoring data
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory in which to save the data file.
#' @param ... Additional arguments passed to \code{download.file}.
#' @description Downloads 'latest' data files into \code{dataDir} for later use.
#' Downloaded versions of PWFSL monitoring .RData files allow users to work with
#' the package without access to the internet. Once data are downloaded to
#' \code{dataDir}, any of the data loading functions can be called with the
#' \code{dataDir} argument to replace internet downloads with local file access.
#'
#' The recommended directory for PWFSL monitoring data is
#' \code{"~/data/monitoring/RData"}.
#'
#' For daily updates covering the most recent 45 days, use \code{monitor_downloadDaily()}.
#'
#' For data extended more than 45 days into the past, use \code{monitor_downloadAnnual()}.
#'
#' Currently supported parameters include the following:
#' \enumerate{
# #' \item{BARPR}
# #' \item{BC}
# #' \item{CO}
# #' \item{NO}
# #' \item{NO2}
# #' \item{NO2Y}
# #' \item{NO2X}
# #' \item{NOX}
# #' \item{NOOY}
# #' \item{OC}
# #' \item{OZONE}
# #' \item{PM10}
#' \item{PM2.5}
# #' \item{PRECIP}
# #' \item{RHUM}
# #' \item{SO2}
# #' \item{SRAD}
# #' \item{TEMP}
# #' \item{UV-AETH}
# #' \item{WD}
# #' \item{WS}
#' }
#'
#' Avaialble RData files can be seen at:
#' \href{https://haze.airfire.org/monitoring/latest/RData/}{https://haze.airfire.org/monitoring/latest/RData/}
#' @seealso \code{\link{monitor_loadDaily}}
#' @examples
#' \donttest{
#' library(PWFSLSmoke)
#'
#' monitor_loadLatest() %>%
#'   monitor_subset(stateCodes=CONUS) %>%
#'   monitor_map()
#' }

monitor_downloadLatest <- function(parameter='PM2.5',
                                   baseUrl='https://haze.airfire.org/monitoring/latest/RData/',
                                   dataDir = "~/Data/monitoring/RData",
                                   ...) {

  # AirNow
  filename <- paste0("airnow_", parameter, "_latest10.RData")
  downloadDataFile(filename, baseUrl, dataDir, ...)

  # AIRSIS
  filename <- paste0("airsis_", parameter, "_latest10.RData")
  downloadDataFile(filename, baseUrl, dataDir, ...)

  # WRCC
  filename <- paste0("wrcc_", parameter, "_latest10.RData")
  downloadDataFile(filename, baseUrl, dataDir, ...)

  return(invisible(NULL))

}
