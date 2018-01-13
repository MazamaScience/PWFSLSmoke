#' @keywords WRCC
#' @export
#' @title Identify WRCC Monitor Type
#' @param fileString character string containing WRCC data
#' @description Examine the column names of the incoming character vector
#' to identify different types of monitor data provided by WRCC.
#' 
#' The return is a list includes everything needed to identify and parse the raw
#' data using \code{readr::read_tsv()}:
#' 
#' \itemize{
#' \item{\code{monitorType}}{ -- identification string}
#' \item{\code{rawNames}}{ -- column names from the data (including special characters)}
#' \item{\code{columnNames}}{ -- assigned column names (special characters repaced with '.')}
#' \item{\code{columnTypes}}{ -- column type string for use with \code{readr::read_csv()}}
#' }
#' 
#' The \code{monitorType} will be one of:
#' \itemize{
#' \item{"\code{WRCC_TYPE1}"}{ -- ???}
#' \item{"\code{WRCC_TYPE2}"}{ -- ???}
#' \item{"\code{UNKOWN}"}{ -- ???}
#' }
#' 
#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{WRCC Fire Cache Smoke Monitor Archive}
#' @examples
#' \dontrun{
#' fileString <- wrcc_downloadData(20160701, 20160930, unitID='1307')
#' monitorTypeList <- wrcc_identifyMonitorType(fileString)
#' }

wrcc_identifyMonitorType <- function(fileString) {
  
  if ( class(fileString)[1] != "character" ) {
    logger.error('WRCC fileString is of type %s', class(fileString)[1])
    stop(paste0('WRCC fileString is of type %s', class(fileString)[1]))
  }
  
  #     Different header styles     -------------------------------------------
  
  # stationID=E231, year=2016
  type1_header <- vector('character',3)
  type1_header[1] <- ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \t Unk \tdeg C\t  %  \t m/s \t Deg \tvolts\t"                                   
  type1_header[2] <- ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Misc  \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm"
  type1_header[3] <- ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t  #2   \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t"     
  type1_rawNames <- c('DateTime','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
                      'AvAirFlw','AvAirTemp','RelHumidity','Misc2','SensorIntAT','SensorIntRH',
                      'WindSpeed','WindDir','BatteryVoltage','Alarm')
  type1_names <- type1_rawNames
  type1_types <- 'cdddcdddddddddddd'
  
  # stationID=SMF1, year=2016
  type2_header <- vector('character', 3)
  type2_header[1] <- ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \tmbar \tdeg C\t  %  \t m/s \t Deg \tvolts\t"                                   
  type2_header[2] <- ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Barom \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm"
  type2_header[3] <- ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t Press \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t"     
  type2_rawNames <- c('DateTime','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
                      'AvAirFlw','AvAirTemp','RelHumidity','BaromPress','SensorIntAT','SensorIntRH',
                      'WindSpeed','WindDir','BatteryVoltage','Alarm')
  type2_names <- type2_rawNames
  type2_types <- 'cdddcdddddddddddd'
  
  #      Extract  header lines from the incoming fileString     ---------------
  
  # NOTE:  Here is an example header from WRCC ASCII output:
  # NOTE:  
  # NOTE:  [1] " Smoke #11 "                                                                                                                                                
  # NOTE:  [2] ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \t Unk \tdeg C\t  %  \t m/s \t Deg \tvolts\t     "                                
  # NOTE:  [3] ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Misc  \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm  "
  # NOTE:  [4] ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t  #2   \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t       "
  # NOTE:
  # NOTE:  Sometimes the "Misc #2" column is replaced with Barom Press"
  
  lines <- readr::read_lines(fileString)
  
  # Strip spaces from the beginning and end but retain "\t" (This is why we can't use stringr::str_trim)
  lines <- stringr::str_replace(lines,'^ *','')
  lines <- stringr::str_replace(lines,' *$','')
  
  # Extract the header
  header <- lines[2:4]
  
  #     Assign the monitor type     -------------------------------------------
  
  # Default to "UNKONWN" type of monitor
  monitorType <- "UNKNOWN"
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')
  
  # Test the header against known headers to determine the type
  if ( all(header == type1_header) ) {
    monitorType <- "WRCC_TYPE1"
    rawNames <- type1_rawNames
    columnNames <- type1_names
    columnTypes <- type1_types
  } else if (all(header == type2_header) ) {
    monitorType <- "WRCC_TYPE2"
    rawNames <- type2_rawNames
    columnNames <- type2_names
    columnTypes <- type2_types
  }
  
  monitorTypeList <- list(monitorType=monitorType,
                          rawNames=rawNames,
                          columnNames=columnNames,
                          columnTypes=columnTypes)
  
  return(monitorTypeList)
}
