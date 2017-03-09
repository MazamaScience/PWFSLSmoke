#' @keywords ws_monitor
#' @export
#' @title Subset Monitoring Data
#' @param ws_monitor emph{ws_monitor} object
#' @param filter an \R{R} expression used to identify values for replacement
#' @param value replacement value
#' @description Use an R expression to identify values for replacement.
#' 
#' The \R{R} expression given in \code{filter} is used to identify elements
#' in \code{ws_monitor$data} that should be replaced.  Typical usage would include
#' \enumerate{
#' \item{replacing negative values with 0}
#' \item{replacing unreasonably high values with \code{NA}}
#' }
#' 
#' Expressions should use \code{data} for the left hand side of the comparison.
#' @examples
#' wa <- monitor_subset(Northwest_Megafires, stateCodes='WA')
#' wa_zero <- monitor_replaceData(wa, data < 0, 0)

monitor_replaceData <- function(ws_monitor, filter, value) {
  
  # Sanity check
  if (!"ws_monitor" %in% class(ws_monitor)) {
    stop("ws_monitor object is not of class 'ws_monitor'.")
  }
  
  # Create a condition call, basically an expression that isn't run yet.
  condition_call <- substitute(filter)
  filterString <- paste(as.character(condition_call)[2], as.character(condition_call)[1],
                        as.character(condition_call)[3])
  
  # NOTE:  Example condition_call:
  # NOTE:  > as.character(condition_call)
  # NOTE:  [1] "<"    "data" "0"   
  
  # filterString must contain 'data'
  if ( any(stringr::str_detect(filterString, 'data')) ) {
    
    # NOTE:  We must do extra work to avoid conversion to numeric in the case 
    # NOTE:  where there is only a single column of data.

    # Create a data-only dataframe by omitting the first 'datetime' column
    data <- as.data.frame(ws_monitor$data[,-1])
    colnames(data) <- colnames(ws_monitor$data)[-1]
    
    # Use FUN to create a mask
    FUN <- function(list) { eval(condition_call, data.frame(data = list)) }
    dataMask <- apply(data, 2, FUN)
    dataMask <- replace(dataMask, is.na(dataMask), FALSE)
    
    # Replace matching data with value
    data[dataMask] <- value
    
    # Replace ws_monitor$data data columns with new data
    ws_monitor$data[,-1] <- data

  } else {    
    
    stop( paste0("Bad filter \"", filterString, "\" passed in.") )
    
  }
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
}
