#' @keywords WRCC
#' @export
#' @title Obtain WRCC Data and Parse into Dataframe
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param unitID station identifier (will be upcased)
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @param saveFile optional filename where raw CSV will be written
#' @param flagAndKeep flag, rather then remove, bad data during the QC process
#' @return Raw dataframe of WRCC data.
#' @description Obtains monitor data from a WRCC webservice and converts
#' it into a quality controlled, metadata enhanced "raw" dataframe
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
#' df <- wrcc_createRawDataframe(20150701, 20150930, unitID='SM16')
#' }
#' 
#' @note The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
#' @note Monitor unitIDs can be found at http://www.wrcc.dri.edu/cgi-bin/smoke.pl.
#' 
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' 
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}

wrcc_createRawDataframe <- function(startdate=20100101,
                                    enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                    unitID=NULL,
                                    clusterDiameter=1000,
                                    baseUrl="http://www.wrcc.dri.edu/cgi-bin/wea_list2.pl",
                                    saveFile=NULL,
                                    flagAndKeep=FALSE) {
  
  # Sanity check
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  # Read in WRCC .csv data
  logger.info("Downloading WRCC data ...")
  fileString <- wrcc_downloadData(startdate, enddate, unitID, baseUrl)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file=saveFile),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      logger.warn("Unable to save data to local file %s: %s", saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # Read csv raw data into a dataframe
  logger.info("Parsing data ...")
  df <- wrcc_parseData(fileString)
  
  # add monitor type
  df$provider <- "WRCC"
  
  # Apply monitor-appropriate QC to the dataframe
  logger.info("Applying QC logic ...")
  df <- wrcc_qualityControl(df, flagAndKeep=flagAndKeep)
  
  # Add clustering information to identify unique deployments
  logger.info("Clustering ...")
  df <- addClustering(df, lonVar='GPSLon', latVar='GPSLat', clusterDiameter=clusterDiameter, flagAndKeep=flagAndKeep)
  
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
    df_QC <- df[,QC_columnNames]
    df_nonQC <- df[,-(which(names(df) %in% QC_columnNames))]
    df <- cbind(df_nonQC,df_QC)
  }
  
  return(df)
  
}
