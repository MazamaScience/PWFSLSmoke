#' @keywords ws_monitor
#' @export
#' @title Calculate Daily Statistics
#' @param ws_monitor \emph{ws_monitor} object
#' @param FUN function used to collapse a day's worth of data into a single number for each monitor in the ws_monitor object
#' @param dayStart one of \code{sunset|midnight|sunrise}
#' @param na.rm logical value indicating whether NA values should be ignored
#' @param minHours minimum number of valid data hours required to calculate each daily statistic
#' @return A list of \emph{ws_monitor} objects with daily statistics for each local timezone.
#' @description Calculates daily statistics for each monitor in \code{ws_monitor}.
#' @details Splits the \code{ws_monitor} object by \code{timezone} and applies the
#' \code{monitor_dailyStatistic()} function separately for each timezone. 
#' See \link{monitor_dailyStatistic} for more details.
#' 
#' The results are returned as a list of \emph{ws_monitor} objects with each element of the
#' list named with the associated timezone. Note that each \code{ws_monitor$data$datetime}
#' will be in local time. This is desirable as it ensures proper date formatting in tables and plots.
#' 
#' You should not attempt to reassemble a single \emph{ws_monitor} object from the elements in this list.
#' 
#' @references \link{monitor_dailyStatistic}
#' @examples 
#' \dontrun{
#' airnow <- airnow_loadLatest()
#' nw <- monitor_subset(airnow, stateCodes = c('WA','OR','ID','MT'))
#' dailyList <- monitor_dailyStatisticList(nw)
#' monitorLeaflet(dailyList[["America/Los_Angeles"]])
#' monitorLeaflet(dailyList[["America/Boise"]])
#' monitorLeaflet(dailyList[["America/Denver"]])
#' }

monitor_dailyStatisticList <- function(ws_monitor,
                                       FUN=get("mean"),
                                       dayStart="midnight",
                                       na.rm=TRUE,
                                       minHours=18) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }
  
  dailyList <- list()
  
  timezones <- sort(unique(ws_monitor$meta$timezone))
  
  for ( tz in timezones ) {
    
    # NOTE:  We would like to use monitor_subsetBy() like this:
    # NOTE:    mon_tz <- monitor_subsetBy(ws_monitor, timezone == tz)
    # NOTE:
    # NOTE:  But this causes deep and opaque issues at the level of quote/expression/substitute/eval
    # NOTE:  that generate NOTEs that will prevent submission to CRAN
    # NOTE:  Here we bypass all that by doing our own subsetting.
    
    metaMask <- ws_monitor$meta$timezone == tz
    metaMask <- replace(metaMask, is.na(metaMask), FALSE) # convert NA to FALSE
    monitorIDs <- ws_monitor$meta$monitorID[metaMask]
    # omit the first 'datetime' column
    data <- ws_monitor$data[,-1]
    dataMask <- names(data) %in% monitorIDs
    dataMask <- replace(dataMask, is.na(dataMask), FALSE) # convert NA to FALSE
    # Add back first 'datetime' column
    dataMask <- c(TRUE, dataMask)
    
    # Subset the ws_monitor dataframes
    meta <- ws_monitor$meta[metaMask,] # mask rows
    data <- ws_monitor$data[,dataMask] # mask columns
    
    mon_tz <- structure( list(data=data, meta=meta),
                             class = c("ws_monitor", "list") )
    
    # NOTE:  Now we're back where we wanted to be
    
    daily <- monitor_dailyStatistic(mon_tz,
                                    FUN,
                                    dayStart,
                                    na.rm,
                                    minHours)
    dailyList[[tz]] <- daily
    
  }

  return (dailyList)
  
}
