
#' @title Extract dataframes from \emph{ws_monitor} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{ws_monitor} object. These functions are designed to be
#' useful when manipulating data in a pipe chain.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabluar{rr}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{extract_data(ws_monitor)} \tab \code{ws_monitor[["data"]]}\cr
#'   \code{extract_meta(ws_monitor)} \tab \code{ws_monitor[["meta"]]}
#' }
#'
#' @param ws_monitor \emph{ws_monitor} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{ws_monitor} object
#'
#' @examples
#' \dontrun{
#' ws_monitor <- Northwest_Megafires
#'
#' NMData <- ws_monitor %>%
#'   monitor_subset(
#'     stateCodes = "WA",
#'     tlim = c(20150801, 20150831)
#'   ) %>%
#'   extract_data()
#'
#' monitor_subset(
#'     stateCodes = "WA",
#'     tlim = c(20150801, 20150831)
#'   ) %>%
#'   extract_meta()
#' }
#'
#' @name extract_dataFrame
#' @aliases extract_data extract_meta
#'
NULL


#' @export
#' @rdname extract_dataFrame
#'
extract_data <- function(ws_monitor) {

  if (!monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  return(ws_monitor[["data"]])

}


#' @export
#' @rdname extract_dataFrame
#'
extract_meta <- function(ws_monitor) {

  if (!monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  return(ws_monitor[["meta"]])

}
