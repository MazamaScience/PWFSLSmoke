#' @keywords WRCC
#' @export
#' @title Download Data from WRCC
#' @param stationID station identifier (will be upcased)
#' @param startdate integer or character representing start date as YYYYMMDD (GMT)
#' @param enddate integer or character representing end date as YYYYMMDD (GMT)
#' @param url base URL for data queries
#' @description Create an HTTPPOST request to the WRCC data service to request
#' data from a particular station for the desired time period. Raw data are
#' returned as both a dataframe and a character string. Character data are in 
#' tab-separated-value format.
#' @return Dataframe of WRCC monitor data.
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#' @examples
#' \dontrun{
#' df <- wrcc_downloadData('SM16',startdate=20150701,enddate=20150930)
#' }

# Monitor IDs include:
# # http://www.wrcc.dri.edu/cgi-bin/smoke.pl
# CacheMonitors <- c('SM11','SM13','SM15','SM16','SM16','SM17','SM19',
#                    'SM20','SM21','SM22','SM23','SM24',
#                    'S265','SM66','SM67','SM68','SM69',
#                    'S284','S215','S216','S217',
#                    'E231','E840','E866','E925')
# USFSRegionalMonitors <- c()
# MiscellaneousMonitors <- c()


wrcc_downloadData <- function(stationID=NULL, startdate=20100101,
                              enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                              url="http://www.wrcc.dri.edu/cgi-bin/wea_list2.pl") {
  
  # Get UTC times
  starttime <- lubridate::ymd(startdate)
  endtime <- lubridate::ymd(enddate)
  
  # Create CGI parameters
  .params <- list(stn=toupper(stationID),
                  smon=strftime(starttime,"%m",tz="GMT"),
                  sday=strftime(starttime,"%d",tz="GMT"),
                  syea=strftime(starttime,"%y",tz="GMT"),
                  emon=strftime(endtime,"%m",tz="GMT"),
                  eday=strftime(endtime,"%d",tz="GMT"),
                  eyea=strftime(endtime,"%y",tz="GMT"),
                  'Submit Info'='Submit Info',
                  dfor='04',
                  src='W',
                  miss='08',
                  flag='N',
                  Dfmt='01',
                  Tfmt='01',
                  Head='01',
                  Deli='01',
                  unit='M',
                  WsMon='01',
                  WsDay='01',
                  WeMon='12',
                  WeDay='12',
                  WsHou='00',
                  WeHou='24',
                  .cgifields=c('unit','flag','srce'))
  
  rawBytes <-RCurl::postForm(uri=url, .params=.params)
  
  if (class(rawBytes) == "character") {
    stop(rawBytes)
  }
  
  # Convert raw bytes to text
  text <- rawToChar(rawBytes)
  
  # Split single text string based on newline character
  lines <- readr::read_lines(text)
  
  # NOTE:  Here is what things look like:
  # NOTE:  
  # NOTE:  [1] " Smoke #11 "                                                                                                                                                
  # NOTE:  [2] ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \t Unk \tdeg C\t  %  \t m/s \t Deg \tvolts\t     "                                
  # NOTE:  [3] ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Misc  \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm  "
  # NOTE:  [4] ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t  #2   \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t       "
  # NOTE:
  # NOTE:  It appears that after 1024 lines, the 3 header lines are repeated.
  # NOTE:  Sometimes (always?) NA appears in the last line.
  
  type1_header <- vector('character', length=3)
  type1_header[1] <- ":       GMT\t Deg \t Deg \t     \tser #\tug/m3\t Unk \t l/m \tDeg C\t  %  \t Unk \tdeg C\t  %  \t m/s \t Deg \tvolts\t     "
  type1_header[2] <- ": Date/Time\t  GPS  \t  GPS  \tType   \tSerial \tConc   \t Misc  \t Ave.  \t Av Air\t  Rel  \t Misc  \tSensor \tSensor \t  Wind \t Wind  \tBattery\tAlarm  "
  type1_header[3] <- ":YYMMDDhhmm\t  Lat. \t  Lon. \t       \tNumber \t RT    \t  #1   \tAir Flw\t  Temp \tHumidty\t  #2   \tInt AT \tInt RH \t  Speed\t Direc \tVoltage\t       "
  
  # NOTE:  Here is the header that comes with the data:
  # NOTE:  
  # NOTE:  Smoke 215 
  # NOTE:  :       LST	 Deg 	 Deg 	     	ser #	ug/m3	 Unk 	 l/m 	Deg C	  %  	mbar 	deg C	  %  	 m/s 	 Deg 	volts	     
  # NOTE:  : Date/Time	  GPS  	  GPS  	Type   	Serial 	Conc   	 Misc  	 Ave.  	 Av Air	  Rel  	 Barom 	Sensor 	Sensor 	  Wind 	 Wind  	Battery	Alarm  
  # NOTE:  :YYMMDDhhmm	  Lat. 	  Lon. 	       	Number 	 RT    	  #1   	Air Flw	  Temp 	Humidty	 Press 	Int AT 	Int RH 	  Speed	 Direc 	Voltage	 
  # NOTE:
  # NOTE:  Sometimes the "Barom Press" column is replaced with "Misc #2"
  
  # Get monitorName from first line and then remove that line
  monitorName <- stringr::str_trim(lines[1])
  lines <- lines[-1]
  
  # Remove all header lines
  if ( lines[1] == type1_header[1] && lines[2] == type1_header[2] && lines[3] == type1_header[3] ) {
    
    col_names <- c('DateTime','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
                   'AvAirFlw','AvAirTemp','RelHumidity','Misc2','SensorIntAT','SensorIntRH',
                   'WindSpeed','WindDir','BatteryVoltage','Alarm')
    col_types <- 'cdddcdddddddddddd'
    
  } else {
    
    stop(paste0('Unrecognized format parsing is not supported -- header line = \n', paste(lines[1:3],collapse='\n')), call.=FALSE)
    
  }
  
  # Remove header lines, leaving only data
  goodLines <- !is.na(lines) & !stringr::str_detect(lines,'^:')
  fakeFile <- paste0(lines[goodLines], collapse='\n')
  
  df <- readr::read_tsv(fakeFile, col_names=col_names, col_types=col_types)
  
  ###missing_flags <- c(-99899,-9999,-998.9) # TODO
  
  return(df)
  
}
