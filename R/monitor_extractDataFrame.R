
#' @title Extract dataframes from \emph{ws_monitor} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{ws_monitor} object. These functions are designed to be
#' useful when manipulating data in a pipe chain.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabular{ll}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{monitor_extractData(ws_monitor)} \tab \code{ws_monitor[["data"]]}\cr
#'   \code{monitor_extractMeta(ws_monitor)} \tab \code{ws_monitor[["meta"]]}
#' }
#'
#' @param ws_monitor \emph{ws_monitor} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{ws_monitor} object
#'
#' @examples
#' library(PWFSLSmoke)
#'
#' ws_monitor <- Northwest_Megafires
#'
#' data <- ws_monitor %>%
#'   monitor_subset(
#'     stateCodes = "WA",
#'     tlim = c(20150801, 20150831)
#'   ) %>%
#'   monitor_extractData()
#'
#' meta <- ws_monitor %>%
#'   monitor_subset(
#'     stateCodes = "WA",
#'     tlim = c(20150801, 20150831)
#'   ) %>%
#'   monitor_extractMeta()
#'
#' dplyr::glimpse(meta)
#' dplyr::glimpse(data)
#'
#' @name monitor_extractDataFrame
#' @aliases monitor_extractData monitor_extractMeta
#'
NULL


#' @export
#' @rdname monitor_extractDataFrame
#'
monitor_extractData <- function(ws_monitor) {

  if (!monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  return(ws_monitor[["data"]])

}


#' @export
#' @rdname monitor_extractDataFrame
#'
monitor_extractMeta <- function(ws_monitor) {

  if (!monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  return(ws_monitor[["meta"]])

}
