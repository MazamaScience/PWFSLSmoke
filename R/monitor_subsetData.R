#' @keywords monitor
#' @export
#' @title Subset Monitoring 'data' Dataframe
#' @param data 'data' dataframe from a \code{ws_monitor} data list
#' @param tlim optional time range with lo and hi time values (POSIXct)
#' @param vlim optional data range with lo and hi data values
#' @param monitorIDs optional vector of monitorIDs
#' @param dropMonitors flag specifying whether to remove columns -- defaults to \code{FALSE}
#' @description The incoming 'data' dataframe is filtered to remove any monitors that
#'     lie outisde the specified range of time and values and that are not mentioned
#'     in the list of monitorIDs.
#'     
#' If any parameter is not specified, that parameter will not be used in the filtering
#' @details By default, filtering by tlim or vlim will always return a dataframe with the
#'     same number of columns as the incoming dataframe. If \code{dropMonitors=TRUE}, columns
#'     will be removed if there are not valid data for a specific monitor after subsetting.
#'     
#'     Filtering by vlim is open on the left and closed on the right, i.e.
#'     
#'      \code{x > vlim[1] & x <= vlim[2]}
#'       
#' @return monitoring 'data' dataframe or \code{NULL} if filtering removes all monitors

monitor_subsetData <- function(data, tlim=NULL, vlim=NULL, monitorIDs=NULL, dropMonitors=FALSE) {
  
  # Subset based on monitorID column names first as that is the quickest
  if (!is.null(monitorIDs)) {
    data <- data[,c("datetime", monitorIDs)]
  }
  
  if (!is.null(tlim)) {
    data <- dplyr::filter(data, data$datetime >= tlim[1], data$datetime <= tlim[2])
  }
  
  # Sanity check
  if (length(data[,-1]) < 1) {
    warning("No matching monitors found")
    return (NULL)    
  }
  
  # If specified, remove any data columns that have no valid data after time range subsetting
  if (dropMonitors & !is.null(dim(data[,-1]))) {    
    
    anyMask <- c(TRUE, apply(data[,-1],2,function(x) { any(!is.na(x),na.rm=TRUE) }))
    data <- data[,anyMask]
    
    if (!is.null(vlim)) {
      # NOTE:  The apply() function converts the first argument to a matrix.
      # NOTE:  If we pass in a dataframe whose first column is POSIXct then things fail (Is it converting all subsequent columns to POSIXct?)
      # NOTE:  Therefore, we strip off the first column when we use apply() and then add it back
      vlimMask <- apply(data[,-1], 2, function(x) { any((x > vlim[1]) & (x <= vlim[2]), na.rm=TRUE) } )
      data <- data[,c(TRUE,vlimMask)]
    }
  
  } else if (dropMonitors & is.null(dim(data[,-1]))) {
   
    anyLogical <- c(TRUE, any(!is.na(data[,-1]),na.rm=TRUE))
    data <- data[,anyLogical]
    
    if(!is.null(vlim)) {
      
      dataParam <- data[,-1]
            
      dataParam[dataParam > vlim[2] & !is.na(dataParam)]  <- NA
      dataParam[dataParam <= vlim[1] & !is.na(dataParam)] <- NA
            
      data[,-1] <- dataParam
      
    }
    
  }
  
  # NOTE: If no monitors are returned, we are only left with a time vector,
  # which has no names attached. We set data to NULL in this case
  # since it's easy to test for as the result of a calculation. 
  if (is.null(names(data))) {
    warning("No matching monitors found")
    return (NULL)
  }
  # TODO:  Add back YYYYmmddHHMM rownames discarded by dplyr::filter
  
  return(data)
}
