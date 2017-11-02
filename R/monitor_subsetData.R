#' @keywords ws_monitor
#' @export
#' @title Subset ws_monitor Object 'data' Dataframe
#' @param data \emph{ws_monitor} object \code{data} dataframe
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH] or \code{POSIXct})
#' @param vlim optional vector with low and high data value limits
#' @param monitorIDs optional vector of monitorIDs
#' @param dropMonitors flag specifying whether to remove columns -- defaults to \code{FALSE}
#' @param timezone Olson timezone passed to \code{link{parseDatetime}} when parsing numeric \code{tlim}
#' @return A \emph{ws_monitor} object \code{data} dataframe, or \code{NULL} if filtering removes all monitors.
#' @description Subsets a \emph{ws_monitor} object's \code{data} dataframe by removing any monitors that
#' lie outisde the specified ranges of time and values and that are not mentioned in the
#' list of monitorIDs.
#' 
#' If \code{tlim} or \code{vlim} is not specified, it will not be used in the subsetting.
#' 
#' Intended for use by the monitor_subset function.
#' @details By default, filtering by tlim or vlim will always return a dataframe with the
#' same number of columns as the incoming dataframe. If \code{dropMonitors=TRUE}, columns
#' will be removed if there are not valid data for a specific monitor after subsetting.
#'     
#' Filtering by vlim is open on the left and closed on the right, i.e.
#'     
#'  \code{x > vlim[1] & x <= vlim[2]}

monitor_subsetData <- function(data, tlim=NULL, vlim=NULL, monitorIDs=NULL,
                               dropMonitors=FALSE, timezone="UTC") {
  
  if ( !is.null(tlim) ) {
    
    # Accept numeric, character or POSIXct values for tlim
    if (class(tlim)[1] == 'numeric' || class(tlim)[1] == 'character') {
      # Parse tlim according to the specified timezone
      tlim <- parseDatetime(tlim, timezone=timezone)
    } else if ( "POSIXct" %in% class(tlim) ) {
      # leave tlim alone
    } else {
      stop(paste0("Invalid argument type: class(tlim) = '",paste0(class(tlim),collapse=" "),"'"))
    }
    
    # Convert tlim to the data$datetime timezone (typically 'UTC')
    tlim <- lubridate::with_tz(tlim, lubridate::tz(data$datetime))
    
    # Filter dataframe
    data <- dplyr::filter(data, data$datetime >= tlim[1], data$datetime <= tlim[2])
    
  }
  
  # Sanity check
  if ( length(data[,-1]) < 1 ) {
    warning("No matching monitors found")
    return(NULL)    
  }
  
  # If specified, remove any data columns that have no valid data after time range subsetting
  if ( dropMonitors ) {
    
    if ( ncol(data) > 2 ) { 
      
      # Multiple monitors, we can use apply() without worrying our subsetting will return a vector
      anyMask <- c(TRUE, apply(data[,-1],2,function(x) { any(!is.na(x),na.rm=TRUE) }))
      # Sanity check
      if ( sum(anyMask) == 1 ) {
        # All data missing, only 'datetime' has valid values
        warning("All data are missing values.")
        return(NULL)
      }
      data <- data[,anyMask]
      
      if ( !is.null(vlim) ) {
        # NOTE:  The apply() function converts the first argument to a matrix.
        # NOTE:  If we pass in a dataframe whose first column is POSIXct then things fail (Is it converting all subsequent columns to POSIXct?)
        # NOTE:  Therefore, we strip off the first column when we use apply() and then add it back
        vlimMask <- apply(data[,-1], 2, function(x) { any((x > vlim[1]) & (x <= vlim[2]), na.rm=TRUE) } )
        data <- data[,c(TRUE,vlimMask)]
      }
      
    } else {
      
      # Need to be careful with a single monitor
      anyMask <- c(TRUE, any(!is.na(data[,-1]),na.rm=TRUE))
      data <- data[,anyMask]
      
      if ( !is.null(vlim) ) {
        dataParam <- data[,-1]
        dataParam[dataParam > vlim[2] & !is.na(dataParam)]  <- NA
        dataParam[dataParam <= vlim[1] & !is.na(dataParam)] <- NA
        data[,-1] <- dataParam
      }
      
    }
    
  }
  
  # NOTE: If no monitors are returned, we are only left with a time vector,
  # which has no names attached. We set data to NULL in this case
  # since it's easy to test for as the result of a calculation. 
  if ( is.null(names(data)) ) {
    warning("No matching monitors found")
    return (NULL)  # TODO:  ticket #86 (must be coordinated with monitor_subsetMeta and any code that checks for this)
  }

  # Guarantee that monitors are returned in the order requested
  if ( !is.null(monitorIDs) ) {
    monitorIDs <- as.character(monitorIDs) # allow incoming monitorIDs to be numeric
    foundMonitorIDs <- intersect(monitorIDs, colnames(data)) # perhaps not all monitorIDs were found
    data <- data[,c("datetime",foundMonitorIDs)]
  }
  
  return(data)
  
}
