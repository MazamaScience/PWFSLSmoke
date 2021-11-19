#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Obain AIRSIS data and parse into a raw tibble
#'
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param provider Identifier used to modify baseURL \code{['APCD'|'USFS']}.
#' @param unitID Character or numeric AIRSIS unit identifier.
#' @param clusterDiameter Diameter in meters used to determine the number of clusters (see \code{addClustering}).
#' @param baseUrl Base URL for data queries.
#' @param saveFile Optional filename where raw CSV will be written.
#' @param flagAndKeep Flag, rather then remove, bad data during the QC process.
#'
#' @return Raw tibble of AIRSIS data.
#'
#' @description Obtains monitor data from an AIRSIS webservice and converts
#' it into a quality controlled, metadata enhanced "raw" tibble
#' ready for use with all \code{raw_~} functions.
#'
#' Steps involved include:
#'
#' \enumerate{
#'  \item{download CSV text}
#'  \item{parse CSV text}
#'  \item{apply quality control}
#'  \item{apply clustering to determine unique deployments}
#'  \item{enhance metadata to include: elevation, timezone, state, country, site name}
#' }
#'
#' @note The downloaded CSV may be saved to a local file by providing an argument
#' to the \code{saveFile} parameter.
#'
#' @seealso \code{\link{airsis_downloadData}}
#' @seealso \code{\link{airsis_parseData}}
#' @seealso \code{\link{airsis_qualityControl}}
#' @seealso \code{\link{addClustering}}
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(PWFSLSmoke)
#'
#' raw <- airsis_createRawDataframe(startdate = 20160901,
#'                                  provider = 'USFS',
#'                                  unitID = '1033')
#' raw <- raw_enhance(raw)
#' rawPlot_timeseries(raw, tlim = c(20160908,20160917))
#'
#' }, silent = FALSE)
#' }
#'

airsis_createRawDataframe <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y010100", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23", tz = "UTC"),
  provider = NULL,
  unitID = NULL,
  clusterDiameter = 1000,
  baseUrl = "http://xxxx.airsis.com/vision/common/CSVExport.aspx?",
  saveFile = NULL,
  flagAndKeep = FALSE
) {

  logger.debug(" ----- airsis_createRawDataframe() ----- ")

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(provider) ) {
    logger.error("Required parameter 'provider' is missing")
    stop(paste0("Required parameter 'provider' is missing"))
  }

  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  # Read in AIRSIS .csv data
  logger.trace("Downloading data ...")
  fileString <- airsis_downloadData(startdate, enddate, provider, unitID, baseUrl)

  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file = saveFile),
                   silent = TRUE )
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      logger.warn("Unable to save data to local file %s: %s", saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }

  # Read csv raw data into a tibble
  logger.trace("Parsing data ...")
  tbl <- airsis_parseData(fileString)

  # Add source of raw data
  if ( nrow(tbl) > 0 ) {
    tbl$rawSource <- "AIRSIS"
  }

  # Apply monitor-appropriate QC to the tibble
  logger.trace("Applying QC logic ...")
  tbl <- airsis_qualityControl(tbl, flagAndKeep = flagAndKeep)

  # Add clustering information to identify unique deployments
  logger.trace("Clustering ...")
  tbl <- addClustering(tbl, lonVar='Longitude', latVar='Latitude', clusterDiameter=clusterDiameter, flagAndKeep=flagAndKeep)

  # Return ---------------------------------------------------------------------

  # Rearrange columns to put QCFlag_* parameters at end if they exist
  if ( flagAndKeep ) {
    QC_columnNames <- c("QCFlag_anyBad",
                        "QCFlag_reasonCode",
                        "QCFlag_badLon",
                        "QCFlag_badLat",
                        "QCFlag_badType",
                        "QCFlag_badFlow",
                        "QCFlag_badAT",
                        "QCFlag_badRHi",
                        "QCFlag_badConcHr",
                        "QCFlag_badDateAndTime",
                        "QCFlag_duplicateHr")
    # TODO: add intersection check here to remove those that do not exist in data
    tbl_QC <- tbl[,QC_columnNames]
    tbl_nonQC <- tbl[,-(which(names(tbl) %in% QC_columnNames))]
    tbl <- cbind(tbl_nonQC,tbl_QC)
  }

  return(tbl)

}
