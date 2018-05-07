#' @keywords ws_monitor
#' @export
#' @title Test for an correct structure of ws_monitor Object
#' @param ws_monitor \emph{ws_monitor} object
#' @return \code{TRUE} if \code{ws_monitor} has the correct structure, \code{FALSE} otherwise.
#' @description The \code{ws_monitor} is checked for the 'ws_monitor' class name
#' and presence of core metadata columns:
#' \itemize{
#'   \item{monitorID -- per deployment unique ID}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{elevation -- height above sea level in meters}
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#' 
#' @examples
#' monitor_isEmpty(Carmel_Valley)

monitor_isMonitor <- function(ws_monitor) {
  
  # Test a variety of things that could go wrong
  if ( !"ws_monitor" %in% class(ws_monitor) ) return(FALSE)
  
  if ( !"meta" %in% names(ws_monitor) ) return(FALSE)
  if ( !"data" %in% names(ws_monitor) ) return(FALSE)
  
  coreParameters <- c('monitorID','longitude','latitude','elevation',
                      'timezone','countryCode','stateCode')
  
  if ( !all(coreParameters %in% names(ws_monitor$meta)) ) return(FALSE)
  
  if ( !"datetime" %in% names(ws_monitor$data) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}
