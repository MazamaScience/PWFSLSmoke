#' @keywords ws_monitor
#' @export
#' @title Return Monitor Data in a Single Dataframe
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID monitor ID of interest (not needed if \code{ws_monitor} contains only one monitor)
#' @param extraColumns optional vector of additional data columns to generate
#' @param metaColumns optional vector of column names from \code{ws_monitor$meta}
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH] or \code{POSIXct})
#' @return A dataframe version of a \emph{ws_monitor} object.
#' @description Creates a dataframe with data from a \emph{ws_monitor} object, essentially
#' \emph{flattening} the object. This is especially useful when monitoring data will be shared with
#' non-R users working with spreadsheets. The returned dataframe will contain data from the monitor
#' specified with \code{monitorID}.
#'
#' The number of data columns in the returned dataframe can include all metadata as well
#' as additional calculated values.
#'
#' By default, the following, core columns are included in the dataframe:
#' \itemize{
#' \item{\code{utcTime}}{ UTC datetime}
#' \item{\code{localTime}}{ local datetime}
#' \item{\code{pm25}}{ PM2.5 values in ug/m3}
#' }
#'
#' Any column from \code{ws_monitor$meta} may be included in the vector of \code{metaColumns}.
#'
#' The following additional columns of data may be included by adding one of the following
#' to the vector of \code{extraColumns{}}
#' \itemize{
#' \item{\code{aqi}}{ hourly AQI values as calculated with \code{monitor_aqi()}}
#' \item{\code{nowcast}}{ hourly Nowcast values as calcualted with \code{monitor_nowcast()}}
#' \item{\code{dailyAvg}}{ daily average PM2.5 values as calculated with \code{monitor_dailyStatistic()}}
#' }
#' @note The \code{tlim} argument is interpreted as localtime, not UTC.
#' @seealso \code{\link{monitor_aqi}}
#' @seealso \code{\link{monitor_nowcast}}
#' @seealso \code{\link{monitor_dailyStatistic}}
#' @examples
#' \dontrun{
#' wa <- monitor_subset(Northwest_Megafires, stateCodes='WA')
#' Omak_df <- monitor_asDataframe(wa, monitorID='530470013_01',
#'                                extraColumns=c('nowcast','dailyAvg'),
#'                                metaColumns=c('aqsID','siteName','timezone'),
#'                                tlim=c(20150801,20150901))
#' }


monitor_asDataframe <- function(ws_monitor,
                                monitorID=NULL,
                                extraColumns=NULL,
                                metaColumns=NULL,
                                tlim=NULL) {

  # Sanity check
  if ( !"ws_monitor" %in% class(ws_monitor) ) {
    stop("ws_monitor object is not of class 'ws_monitor'.")
  }

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")

  # Sanity check
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) > 1 ) {
      stop(paste0("monitorID must be specified when ws_monitor contains more than 1 monitor [", nrow(ws_monitor$meta), "]"))
    }
  }

  # Sanity check metaColumns
  if ( !is.null(metaColumns) ) {
    if ( length(setdiff(metaColumns,names(ws_monitor$meta))) > 0 ) {
      badMetaString <- paste(setdiff(metaColumns,names(ws_monitor$meta)), collapse=", ")
      stop(paste0("The following are not valid metaColumns: ", badMetaString))
    }
  }

  # Sanity check extraColumns
  if ( !is.null(extraColumns) ) {
    if ( length(setdiff(extraColumns,c('aqi','nowcast','dailyAvg'))) > 0 ) {
      badMetaString <- paste(setdiff(metaColumns,c('aqi','nowcast','dailyAvg')), collapse=", ")
      stop(paste0("The following are not valid metaColumns: ", badMetaString))
    }
  }

  # Subset to a single monitor
  if ( !is.null(monitorID) ) {
    ws_monitor <- monitor_subset(ws_monitor, monitorIDs=monitorID)
  }

  # localTime determination
  timezone <- ws_monitor$meta$timezone[1]

  # ---- Create core data frame with appropriate column names

  # datetime from a ws_monitor object should always be UTC
  UTCTime <- lubridate::with_tz(ws_monitor$data$datetime, tzone = "UTC")

  # Save character string representations of UTCTime and localTime
  UTCTimeString <- strftime(UTCTime, "%Y-%m-%d %H:%M:%S %z", tz="UTC")
  localTimeString <- strftime(UTCTime, "%Y-%m-%d %H:%M:%S %z", tz=timezone)

  monitorDataframe <- data.frame(ws_monitor$data$datetime, UTCTimeString, localTimeString, ws_monitor$data[,-1],
                                 check.names = FALSE, stringsAsFactors = FALSE)
  names(monitorDataframe) <- c("datetime","utcTime", "localTime", "pm25")

  # ----- Add extraColumns

  if ( 'aqi' %in% extraColumns ) {
    mon_aqi <- monitor_aqi(ws_monitor, includeShortTerm=TRUE)
    names(mon_aqi$data) <- c('datetime','aqi')
    monitorDataframe <- dplyr::left_join(monitorDataframe, mon_aqi$data, by='datetime')
  }

  if ( 'nowcast' %in% extraColumns ) {
    mon_nowcast <- monitor_nowcast(ws_monitor, includeShortTerm=TRUE)
    names(mon_nowcast$data) <- c('datetime','nowcast')
    monitorDataframe <- dplyr::left_join(monitorDataframe, mon_nowcast$data, by='datetime')
  }

  if ( 'dailyAvg' %in% extraColumns ) {
    mon_daily <- monitor_dailyStatistic(ws_monitor)
    names(mon_daily$data) <- c('datetime','dailyAvg')
    mon_daily$data$dailyAvg <- round(mon_daily$data$dailyAvg, 1)
    # Add columns to both dataframes that we can join by
    mon_daily$data$localDateString <- strftime(mon_daily$data$datetime, "%Y-%m-%d", tz=timezone)
    mon_daily$data$datetime <- NULL # remove datetime
    monitorDataframe$localDateString <- strftime(UTCTime, "%Y-%m-%d", tz=timezone)
    monitorDataframe <- dplyr::left_join(monitorDataframe, mon_daily$data, by='localDateString')
    monitorDataframe$localDateString <- NULL # remove localDateString
  }

  # ----- Add metaColumns

  for ( name in metaColumns ) {
    monitorDataframe[[name]] <- ws_monitor$meta[[name]][1]
  }

  # ----- Temporal subset

  if ( !is.null(tlim) ) {
    starttime <- MazamaCoreUtils::parseDatetime(tlim[1], timezone = timezone)
    endtime <- MazamaCoreUtils::parseDatetime(tlim[2], timezone = timezone)
    timeMask <- (monitorDataframe$datetime >= starttime) & (monitorDataframe$datetime <= endtime)
    monitorDataframe <- monitorDataframe[timeMask,]
  }

  # Remove 'datetime' column
  monitorDataframe$datetime <- NULL

  return(monitorDataframe)

}
