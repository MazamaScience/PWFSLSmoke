#' @keywords ws_monitor
#' @export
#' @title Subset ws_monitor Object with a Filter
#' @param ws_monitor \emph{ws_monitor} object
#' @param filter a filter to use on the ws_monitor object
#' @return A \emph{ws_monitor} object with a subset of the input ws_monitor object.
#' @description The incoming ws_monitor object is filtered according to \code{filter}.
#' Either meta data or actual data can be filtered. 
#' @examples
#' N_M <- Northwest_Megafires
#' boise_tz <- monitor_subsetBy(N_M, timezone == 'America/Boise')
#' boise_tz_very_unhealthy <- monitor_subsetBy(boise_tz, data > AQI$breaks_24[5])
#' boise_tz_very_unhealthy$meta$siteName

monitor_subsetBy <- function(ws_monitor, filter) {
  
  # Sanity check
  if (!"ws_monitor" %in% class(ws_monitor)) {
    stop("ws_monitor object is not of class 'ws_monitor'.")
  }
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")
  
  # Create a condition call, basically an expression that isn't run yet.
  condition_call <- substitute(filter)
  filterString <- paste(as.character(condition_call)[2], as.character(condition_call)[1],
                        as.character(condition_call)[3])
  
  # NOTE:  Example condition_call:
  # NOTE:  > as.character(condition_call)
  # NOTE:  [1] "=="                  "timezone"            "America/Los_Angeles"
  
  # If the condition_call is valid for ws_monitor$meta
  if ( any(stringr::str_detect(filterString, names(ws_monitor$meta))) ) {
    
    metaMask <- eval(condition_call, ws_monitor$meta)
    monitorIDs <- ws_monitor$meta$monitorID[metaMask]
    # omit the first 'datetime' column
    data <- ws_monitor$data[,-1]
    dataMask <- names(data) %in% monitorIDs
    dataMask <- replace(dataMask, is.na(dataMask), FALSE) # drop NAs
    # Add back first 'datetime' column
    dataMask <- c(TRUE, dataMask)
    
  # If the condition_call is intended for ws_monitor$data
  } else if ( any(stringr::str_detect(filterString, 'data')) ) {
    
    FUN <- function(list) { any(eval(condition_call, data.frame(data = list))) }
    # Omit the first 'datetime' column
    # NOTE:  We must do extra work to avoid conversion to numeric in the case 
    # NOTE:  where there is only a single column of data.
    data <- as.data.frame(ws_monitor$data[,-1])
    colnames(data) <- colnames(ws_monitor$data)[-1]
    dataMask <- apply(data, 2, FUN)
    dataMask <- replace(dataMask, is.na(dataMask), FALSE) # drop NAs
    monitorIDs <- names(data[dataMask])
    metaMask <- ws_monitor$meta$monitorID %in% monitorIDs
    # Add back first 'datetime' column
    dataMask <- c(TRUE, dataMask)
    
  } else {    
    
    stop( paste0("Bad filter \"", filterString, "\" passed in.") )
    
  }
  
  # Subset the ws_monitor dataframes
  meta <- ws_monitor$meta[metaMask,] # mask rows
  data <- ws_monitor$data[,dataMask] # mask columns
  
  ws_monitor <- list(data=data, meta=meta)
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}
