#' @keywords WRCC
#' @export
#' @title Identify WRCC Dump File Monitor Type
#' @param fileString character string containing WRCC dump file
#' @description Examine the column names of the incoming character vector
#' to identify different types of monitor data provided by WRCC.
#' 
#' The return is a list includes everything needed to identify and parse the raw
#' data using \code{readr::read_csv()}:
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
#' This function is intended for internal use at the Pacific Wildland
#' Fire Sciences Lab.

#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{WRCC Fire Cache Smoke Monitor Archive}

wrccDump_identifyMonitorType <- function(fileString) {
  
  # Sanity check
  if ( class(fileString)[1] != "character" ) {
    logger.error("WRCC fileString is of type %s", class(fileString)[1])
    stop(paste0("WRCC fileString is of type %s", class(fileString)[1]))
  }
  
  #     Different header styles     -------------------------------------------
  
  # stationID=E231, year=2016
  type1_header <- vector('character',3)
  type1_header[1] <- ":          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  , Unk ,deg C,  %  , m/s , Deg ,volts,"
  type1_header[2] <- ":   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Misc  ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm"
  type1_header[3] <- ":MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty,  #2   ,Int AT ,Int RH ,  Speed, Direc ,Voltage,"
  type1_rawNames <- c('Date','Time','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
                      'AvAirFlw','AvAirTemp','RelHumidity','Misc2','SensorIntAT','SensorIntRH',
                      'WindSpeed','WindDir','BatteryVoltage','Alarm')
  type1_names <- type1_rawNames
  type1_types <- 'ccdddcdddddddddddd'
  
  # stationID=SMF1, year=2016
  type2_header <- vector('character', 3)
  type2_header[1] <- ":          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  ,mbar ,deg C,  %  , m/s , Deg ,volts,"
  type2_header[2] <- ":   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Barom ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm"
  type2_header[3] <- ":MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty, Press ,Int AT ,Int RH ,  Speed, Direc ,Voltage,"
  type2_rawNames <- c('Date','Time','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
                      'AvAirFlw','AvAirTemp','RelHumidity','BaromPress','SensorIntAT','SensorIntRH',
                      'WindSpeed','WindDir','BatteryVoltage','Alarm')
  type2_names <- type2_rawNames
  type2_types <- 'ccdddcdddddddddddd'
  

  #      Extract  header lines from the incoming fileString     ---------------
  
  lines <- readr::read_lines(fileString)
  
  # Strip spaces from the beginning and end
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
  } else if ( all(header == type2_header) ) {
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
