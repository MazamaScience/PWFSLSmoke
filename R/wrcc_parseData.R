#' @keywords WRCC
#' @export
#' @title Parse WRCC Data String
#' @param fileString character string containing WRCC data
#' @description Raw character data from WRCC are parsed into a dataframe.
#' The incoming \code{fileString}
#' can be read in directly from WRCC using \code{wrcc_downloadData()} or from a local
#' file using \code{readr::read_file()}.
#' 
#' The type of monitor represented by this fileString is inferred from the column names
#' using \code{wrcc_identifyMonitorType()} and appropriate column types are assigned.
#' The character data are then processed, read into a dataframe and augmented in the following ways:
#' \enumerate{
#' \item{Spaces at the beginning and end of each line are moved.}
#' \item{All header lines beginning with ':' are removed.}
#' }
#' @return Dataframe of WRCC raw monitor data.
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#' @examples
#' \dontrun{
#' fileString <- wrcc_downloadData('SM16',startdate=20150701,enddate=20150930)
#' df <- wrcc_parseData(fileString)
#' }

wrcc_parseData <- function(fileString) {
  
  # Identify monitor type
  monitorTypeList <- wrcc_identifyMonitorType(fileString)
  
  monitorType <- monitorTypeList$monitorType
  rawNames <- monitorTypeList$rawNames
  columnNames <- monitorTypeList$columnNames
  columnTypes <- monitorTypeList$columnTypes
  
  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)
  
  if ( length(lines) <= 4 ) {
    logger.warn('No valid PM2.5 data')
    stop(paste0('No valid PM2.5 data'))
  }
  
  # NOTE:  Here is an example header from WRCC ASCII output:
  # NOTE:  
  # NOTE:  [1] " Smoke #11 "                                                                                                                                                
  # NOTE:  [2] ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \t Unk \tdeg C\t  %  \t m/s \t Deg \tvolts\t     "                                
  # NOTE:  [3] ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Misc  \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm  "
  # NOTE:  [4] ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t  #2   \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t       "
  # NOTE:
  # NOTE:  It appears that, after 1024 lines, the 3 header lines are repeated.
  # NOTE:  Sometimes (always?) an empty string appears in the last line.
  
  # Strip spaces from the beginning and end but retain "\t" (This is why we can't use stringr::str_trim)
  lines <- stringr::str_replace(lines,'^ *','')
  lines <- stringr::str_replace(lines,' *$','')
  
  # Get monitorName from first line and then remove that line
  monitorName <- lines[1]
  lines <- lines[-1]
  
  # Remove header lines beginning with ":", leaving only data
  goodLines <- !is.na(lines) & !stringr::str_detect(lines,'^:')
  
  # Read the data into a dataframe
  fakeFile <- paste0(lines[goodLines], collapse='\n')
  df <- readr::read_tsv(fakeFile, col_names=columnNames, col_types=columnTypes)

  # Add monitor name
  df$monitorName <- monitorName
  
  # Add monitor type (determined from the 'Type' column after reading in the data)
  monitorTypeCode <- unique(df$Type)
  # NOTE:  Drop all negative values to get rid of -9999 or other missing value flags.
  # NOTE:  Conversion of -9999 to NA happens in the ~QualityControl scripts so that
  # NOTE:  all raw data modifications can be found in one place.
  monitorTypeCode <- monitorTypeCode[monitorTypeCode >= 0]
  # Sanity check
  if ( length(monitorTypeCode) > 1 ) {
    logger.error('More than one monitor type detected: %s', paste(monitorTypeCode,sep=", "))
    stop(paste0('More than one monitor type detected: %s', paste(monitorTypeCode,sep=", ")))
  }
  
  # 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
  if ( monitorTypeCode == 0 ) {
    df$monitorType <- 'EBAM'
  } else if ( monitorTypeCode == 9 ) {
    df$monitorType <- 'ESAM'
  } else if ( monitorTypeCode == 1 ) {
    logger.error('EBAM PM10 data parsing is not supported')
    stop(paste0('EBAM PM10 data parsing is not supported'))
  } else {
    logger.error('Unsupported monitor type code: %d',monitorTypeCode)
    stop(paste0('Unsupported monitor type code: %d',monitorTypeCode))
  }

  return(df)
  
}
