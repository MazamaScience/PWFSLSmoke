#' @keywords AIRSIS
#' @export
#' @title Download USFS Data from AIRSIS
#' @param unitID unit identifier
#' @param startdate integer or character representing start date as YYYYMMDD (GMT)
#' @param enddate integer or character representing end date as YYYYMMDD (GMT)
#' @param baseUrl base URL for data queries
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a dataframe. 
#' @return Dataframe of AIRSIS monitor data.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' df <- airsis_downloadData('1026',startdate=20150701,enddate=201231)
#' }

airsis_downloadData <- function(unitID=NULL, startdate=20020101,
                                enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                baseUrl="http://usfs.airsis.com/vision/common/CSVExport.aspx?") {
  
  # Sanity check
  if (is.null(unitID)) stop(paste0('Please specify a unitID.'))
  
  # Get UTC times
  starttime <- lubridate::ymd(as.character(startdate))
  endtime <- lubridate::ymd(as.character(enddate))
  
  # Example URL:
  #   http://usfs.airsis.com/vision/common/CSVExport.aspx?uid=1026&StartDate=2016-02-03&EndDate=2016-02-03
  
  # Create URL
  url <- paste0(baseUrl, 'uid=', unitID,
                '&StartDate=', strftime(starttime, "%F", tz="GMT"),
                '&EndDate=', strftime(endtime, "%F", tz="GMT"))
  
  # NOTE:  readr::read_csv throws an error if there are no rows but we
  # NOTE:  don't want that behavior. We'd rather have a dataframe with
  # NOTE:  the header in tact but with no rows. This is what base::read.csv
  # NOTE:  provides.
  
  #   # Read in as dataframe
  #   result <- try( df <- readr::read_csv(url),
  #                  silent=TRUE )
  
  df <- read.csv(url)
  
  # EBAM data files (e.g. unitID 1026) are expected to have the following columns:
  #
  #   > names(bop)
  #   [1] "MasterTable_ID" "Alias"          "Latitude"       "Longitude"      "Date.Time.GMT" 
  #   [6] "COncRT"         "ConcHr"         "Flow"           "W.S"            "W.D"           
  #   [11] "AT"             "RHx"            "RHi"            "BV"             "FT"            
  #   [16] "Alarm"          "Type"           "Serial.Number"  "Version"        "Sys..Volts"    
  #   [21] "TimeStamp"      "PDate"
  
  ebamColNames <- c('MasterTable_ID','Alias','Latitude','Longitude','Date.Time.GMT',
                    'COncRT','ConcHr','Flow','W.S','W.D',
                    'AT','RHx','RHi','BV','FT',
                    'Alarm','Type','Serial.Number','Version','Sys..Volts',
                    'TimeStamp','PDate')
  
  # BAM1020 files (eg. unitID=49) are expected to have the following columns:
  #
  #   > names(df)
  #   [1] "MasterTable_ID"   "Alias"            "Latitude"         "Longitude"        "Conc..µg.m3."    
  #   [6] "Qtot..m3."        "WS..KTS."         "Ozone..ppb."      "RTM09..mg3."      "RH...."          
  #   [11] "Ambient.Temp..C." "TimeStamp"        "PDate"           
  
  bam1020ColNames <- c('MasterTable_ID','Alias','Latitude','Longitude','Conc..\u00B5g.m3.',
                       'Qtot..m3.','WS..KTS.','Ozone..ppb.','RTM09..mg3.','RH....',
                       'Ambient.Temp..C.','TimeStamp','PDate')
  
  # Also saw this for an older "EBAM" (e.g. unitID=70) but it has packed values inside the ebam column:
  #
  #   > names(df)
  #   [1] "MasterTable_ID" "Alias"          "Latitude"       "Longitude"      "Time"          
  #   [6] "DataCol1"       "eBam"           "TimeStamp"      "PDate"         
  
  earlyEbamColNames <-c('MasterTable_ID','Alias','Latitude','Longitude','Time','DataCol1','eBam','TimeStamp','PDate') 
  
  if ( all(ebamColNames %in% names(df)) ) {
    
    # ----- EBAM ----------------------
    
    # NOTE:  Latitude, Longitude and Sys..Volts are measured at 6am and 6pm
    # NOTE:  as separate GPS entries in the dataframe. They need to be carried
    # NOTE:  forward so they appear in all rows.
    
    # Carry data forward to fill in all missing values
    df$Longitude <- zoo::na.locf(df$Longitude, na.rm=FALSE)
    df$Latitude <- zoo::na.locf(df$Latitude, na.rm=FALSE)
    df$Sys..Volts <- zoo::na.locf(df$Sys..Volts, na.rm=FALSE)
    
    # Now fill in any missing values at the front end
    df$Longitude <- zoo::na.locf(df$Longitude, na.rm=FALSE, fromLast=TRUE)
    df$Latitude <- zoo::na.locf(df$Latitude, na.rm=FALSE, fromLast=TRUE)
    df$Sys..Volts <- zoo::na.locf(df$Sys..Volts, na.rm=FALSE, fromLast=TRUE)
    
    # Now remove GPS entry where Type == "" so as to avoid duplicate timestamps.
    # These cause problems when reshaping the data in airsis_createDataDataframe().
    df <- df[df$Type != "",]
    
    # Add monitor name
    df$monitorName <- df$Alias
    
  } else if ( all(bam1020ColNames %in% names(df)) ) {
    
    # ----- BAM1020 -------------------
    
    # TODO:  How to assign data with to-the-minute timestamps to a particular hour? floor?
    stop(paste0('BAM1020 files cannot be processed'))
    
  } else if ( all(earlyEbamColNames %in% names(df)) ) {
    
    # ----- BAM1020 -------------------
    
    # TODO:  These files have an awful format that would require a lot of code to unpack.
    stop(paste0('Early EBAM files cannot be processed'))
    
  } else {
    
    stop(paste0('Unrecognized format cannot be processed.'))
    
  }
  
  # Fix data
  
  
  return(df)
}
