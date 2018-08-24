#' @keywords ws_monitor
#' @export
#' @title Print Monitor Data as CSV
#' @param ws_monitor \emph{ws_monitor} object
#' @param saveFile optional filename where CSV will be written
#' @param metaOnly flag specifying whether to return \code{ws_monitor$meta} only as a machine parseable CSV
#' @param dataOnly flag specifying whether to return \code{ws_monitor$data} only as a machine parseable CSV
#' @param quietly do not print to console, just return the string representation of the CSV
#' @description Prints out the contents of the \code{ws_monitor} object as CSV.
#' By default, the output is a text string with "human readable" CSV that includes both \code{meta} and \code{data}.
#' When saved as a file, this format is useful for point-and-click spreadsheet users who want to have 
#' everything on a single sheet.
#' 
#' To obtain machine parseable CSV strings you can use \code{metaOnly} or \code{dataOnly} which are mutually
#' exclusive but which return CSV strings that can be automatically ingested.
#' 
#' By default, the CSV formatted text is printed to the console as well as returned invisibly.
#' @examples
#' data("Carmel_Valley")
#' Carmel_Valley <- monitor_subset(Carmel_Valley, tlim=c(20160802,20160803))
#' monitor_print(Carmel_Valley)
#' monitor_print(Carmel_Valley, metaOnly = TRUE)
#' monitor_print(Carmel_Valley, dataOnly = TRUE)

monitor_print <- function(ws_monitor,
                          saveFile = NULL,
                          metaOnly = FALSE,
                          dataOnly = FALSE,
                          quietly = FALSE) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")
  
  # localTime determination
  timezones <- ws_monitor$meta$timezone
  timezone <- ifelse( length(unique(timezones)) == 1, unique(timezones), "UTC" )
  
  # ----- Create the meta dataframe -------------------------------------------
  
  # Structure the metaDataframe so that columns match up with dataDataframe
  # NOTE:  second column gets NA to align with the data 'Local Time' column
  usefulColumns <- c("monitorID", "longitude","latitude","elevation",
                     "timezone","countryCode","stateCode",
                     "siteName","agencyName","countyName","msaName",
                     "monitorType","siteID","instrumentID","aqsID","pwfslID",
                     "pwfslDataIngestSource","telemetryAggregator","telemetryUnitID")
  meta <- data.frame(colnames(ws_monitor$meta), "", t(ws_monitor$meta[,usefulColumns]))
  names(meta) <- c("parameter", "blank", ws_monitor$meta$monitorID)
  
  
  # ---- Create the data dataframe --------------------------------------------
  
  # datetime from a ws_monitor object should always be UTC
  UTCTime <- lubridate::with_tz(ws_monitor$data$datetime, tzone = "UTC")
  
  # Save character string representations of UTCTime and localTime
  UTCTimeString <- strftime(UTCTime, "%Y-%m-%d %H:%M:%S %Z", tz="UTC")
  localTimeString <- strftime(UTCTime, "%Y-%m-%d %H:%M:%S %Z", tz=timezone) # defaults to UTC if > 1 timezone involved
  
  data <- data.frame(UTCTimeString, localTimeString, ws_monitor$data[,-1], check.names = FALSE, stringsAsFactors = FALSE)
  if ( length(unique(timezones)) == 1 ) {
    names(data) <- c("UTC Time", "Local Time", ws_monitor$meta$monitorID)
  } else {
    names(data) <- c("UTC Time", "UTC Time (no Local Time because > 1 monitor timezone)", ws_monitor$meta$monitorID)
  }
  
  # Create fake dataframes to use as human-readable separators
  emptyRow <- as.data.frame(matrix("", nrow=1, ncol=ncol(meta)), stringsAsFactors = FALSE)
  metaSeparator <- emptyRow
  metaSeparator[1,1] <- "##### Site metadata begins below here"
  dataSeparator <- emptyRow
  dataSeparator[1,1] <- "##### Hourly data begins below here"
  
  # Format as CSV and combine into a "fake file" text string
  emptyRowText <- readr::format_csv(emptyRow, na="NA", col_names=FALSE)
  metaHeaderText <- readr::format_csv(metaSeparator, na="NA", col_names=FALSE)
  metaBodyText <- readr::format_csv(meta, na="NA", col_names=TRUE)
  dataHeaderText <- readr::format_csv(dataSeparator, na="NA", col_names=FALSE)
  dataBodyText <- readr::format_csv(data, na="NA", col_names=TRUE)
  
  # Three, mutually exclusive output formats are available
  if ( metaOnly ) {
    # Nothing fancy, just the ws_monitor$meta dataframe
    csvText <- readr::format_csv(ws_monitor$meta, na = "NA", col_names = TRUE)
  } else if ( dataOnly ) {
    # Use the improved 'data' dataframe
    csvText <- readr::format_csv(data, na = "NA", col_names = TRUE)
  } else{
    # Fancy, "human readable" format appropriate for point-and-click Excel users
    csvText <- paste0(metaHeaderText, 
                      metaBodyText, 
                      emptyRowText,
                      dataHeaderText, 
                      dataBodyText, 
                      collapse="\n")
  }
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(csvText, file=saveFile),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      warning(err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  if ( !quietly ) {
    cat(csvText)
  }
  
  return(invisible(csvText))
  
}
