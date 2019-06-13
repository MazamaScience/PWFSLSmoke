#' @keywords WRCC
#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain WRCC data and parse into a tibble
#'
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param unitID Station identifier (will be upcased).
#' @param clusterDiameter Diameter in meters used to determine the number of clusters (see \code{addClustering}).
#' @param baseUrl Base URL for data queries.
#' @param saveFile Optional filename where raw CSV will be written.
#' @param flagAndKeep Flag, rather then remove, bad data during the QC process.
#'
#' @return Raw tibble of WRCC data.
#'
#' @description Obtains monitor data from a WRCC webservice and converts
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
#' @examples
#' \dontrun{
#' tbl <- wrcc_createRawDataframe(20150701, 20150930, unitID = 'SM16')
#' }
#'
#' @note The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
#' @note Monitor unitIDs can be found at https://wrcc.dri.edu/cgi-bin/smoke.pl.
#'
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}
#'
#' @references \href{https://wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}

wrcc_createRawDataframe <- function(
  startdate = strftime(lubridate::now(),"%Y010100",tz = "UTC"),
  enddate = strftime(lubridate::now(),"%Y%m%d23",tz = "UTC"),
  unitID = NULL,
  clusterDiameter = 1000,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  saveFile = NULL,
  flagAndKeep = FALSE
) {

  logger.debug(" ----- wrcc_createRawDatafram() ----- ")

  # ----- Validate parameters ---------------------------------------------------

  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  # Read in WRCC .csv data
  logger.trace("Downloading WRCC data ...")
  fileString <- wrcc_downloadData(startdate, enddate, unitID, baseUrl)

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
  tbl <- wrcc_parseData(fileString)

  # Add source of raw data
  tbl$rawSource <- "WRCC"

  # Apply monitor-appropriate QC to the tibble
  logger.trace("Applying QC logic ...")
  tbl <- wrcc_qualityControl(tbl, flagAndKeep = flagAndKeep)

  # Add clustering information to identify unique deployments
  logger.trace("Clustering ...")
  tbl <- addClustering(tbl, lonVar = 'GPSLon', latVar = 'GPSLat', clusterDiameter = clusterDiameter, flagAndKeep = flagAndKeep)

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
