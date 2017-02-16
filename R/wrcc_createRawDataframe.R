#' @keywords WRCC
#' @export
#' @title Obtain WRCC Data and Parse into Dataframe
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param stationID station identifier (will be upcased)
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @param saveFile optional filename where raw CSV will be written
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
#' @note The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}

wrcc_createRawDataframe <- function(startdate=20100101,
                                    enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                    stationID=NULL,
                                    clusterDiameter=1000,
                                    baseUrl="http://www.wrcc.dri.edu/cgi-bin/wea_list2.pl",
                                    saveFile=NULL) {
  
  # Sanity check
  if ( is.null(stationID) ) {
    logger.error("Required parameter 'stationID' is missing")
    stop(paste0("Required parameter 'stationID' is missing"))
  }
  
  # Read in WRCC .csv data
  logger.info("Downloading WRCC data ...")
  fileString <- wrcc_downloadData(startdate, enddate, stationID, baseUrl)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file=saveFile),
                   silent=TRUE )
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.warn("Unable to save data to local file %s: %s", saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # Read csv raw data into a dataframe
  logger.info("Parsing data ...")
  df <- wrcc_parseData(fileString)
  
  # Apply monitor-appropriate QC to the dataframe
  logger.info("Applying QC logic ...")
  df <- wrcc_qualityControl(df)
  
  # Add clustering information to identify unique deployments
  logger.info("Clustering ...")
  df <- addClustering(df, lonVar='GPSLon', latVar='GPSLat', clusterDiameter=1000)
  
  return(df)
  
}
