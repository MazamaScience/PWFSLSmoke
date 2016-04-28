# NOTE:  This is an internal function for now but we will leave the knitr doc-comments
# NOTE:  in place in case we decide to make it public again. (Just start lines with "#'".)

# # Monitor IDs include:
# # # http://www.wrcc.dri.edu/cgi-bin/smoke.pl
# # CacheMonitors <- c('SM11','SM13','SM15','SM16','SM16','SM17','SM19',
# #                    'SM20','SM21','SM22','SM23','SM24',
# #                    'S265','SM66','SM67','SM68','SM69',
# #                    'S284','S215','S216','S217',
# #                    'E231','E840','E866','E925')
# # USFSRegionalMonitors <- c()
# # MiscellaneousMonitors <- c()
#  
# 
# @keywords WRCC
# @export
# @title Download Archive Data from WRCC
# @param stationID station identifier (will be upcased)
# @param startdate integer or character representing start date as YYYYMMDD (GMT)
# @param enddate integer or character representing end date as YYYYMMDD (GMT)
# @param url base URL for data queries
# @description Create an HTTPPOST request to the WRCC data service to request
# data from a particular station for the desired time period. Raw data are
# returned as both a dataframe and a character string. Character data are in 
# tab-separated-value format.
# @return List containing "dataframe" and "text" versions of the requested data.
# @seealso \link{airnow_aggregateHourlyData}
# @seealso \link{airnow_getMonthlyData}
# @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
# @examples
# \dontrun{
# dataList <- wrcc_getArchiveData('SM16',startdate=20150701,enddate=20150930)
# save(dataList$dataframe,file='SM16.RData')
# save(dataList$text,filel='SM16.tsv')
# }
# 
# 
# wrcc_getArchiveData <- function(stationID=NULL, startdate=20100101,
#                                 enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
#                                 url="http://www.wrcc.dri.edu/cgi-bin/wea_list2.pl") {
#   
#   # Get UTC times
#   starttime <- lubridate::ymd(startdate)
#   endtime <- lubridate::ymd(enddate)
#   
#   
#   .params <- list(stn=toupper(stationID),
#                   smon=strftime(starttime,"%m",tz="GMT"),
#                   sday=strftime(starttime,"%d",tz="GMT"),
#                   syea=strftime(starttime,"%y",tz="GMT"),
#                   emon=strftime(endtime,"%m",tz="GMT"),
#                   eday=strftime(endtime,"%d",tz="GMT"),
#                   eyea=strftime(endtime,"%y",tz="GMT"),
#                   'Submit Info'='Submit Info',
#                   dfor='04',
#                   src='W',
#                   miss='08',
#                   flag='N',
#                   Dfmt='01',
#                   Tfmt='01',
#                   Head='01',
#                   Deli='01',
#                   unit='M',
#                   WsMon='01',
#                   WsDay='01',
#                   WeMon='12',
#                   WeDay='12',
#                   WsHou='00',
#                   WeHou='24',
#                   .cgifields=c('unit','flag','srce'))
#   
#   rawBytes <-RCurl::postForm(uri=url, .params=.params)
#   
#   if (class(rawBytes) == "character") {
#     stop(rawBytes)
#   }
#   
#   # Convert raw bytes to text
#   text <- rawToChar(rawBytes)
#   
#   # NOTE:  Here is the header that comes with the data:
#   # NOTE:  
#   # NOTE:  Smoke 215 
#   # NOTE:  :       LST	 Deg 	 Deg 	     	ser #	ug/m3	 Unk 	 l/m 	Deg C	  %  	mbar 	deg C	  %  	 m/s 	 Deg 	volts	     
#   # NOTE:  : Date/Time	  GPS  	  GPS  	Type   	Serial 	Conc   	 Misc  	 Ave.  	 Av Air	  Rel  	 Barom 	Sensor 	Sensor 	  Wind 	 Wind  	Battery	Alarm  
#   # NOTE:  :YYMMDDhhmm	  Lat. 	  Lon. 	       	Number 	 RT    	  #1   	Air Flw	  Temp 	Humidty	 Press 	Int AT 	Int RH 	  Speed	 Direc 	Voltage	 
#   # NOTE:
#   # NOTE:  Sometimes the "Barom Press" column is replaced with "Misc #2"
#   
#   col_names <- c('DateTime','GPSLat','GPSLon','Type','SerialNumber','ConcRT','Misc1',
#                  'AvAirFlw','AvAirTemp','RelHumidity','Misc2','SensorIntAT','SensorIntRH',
#                  'WindSpeed','WindDir','BatteryVoltage','Alarm')
#   col_types <- 'cdddcdddddddddddd'
#   
#   df <- readr::read_tsv(rawBytes, col_names=col_names, col_types=col_types, skip=4)
#   
#   missing_flags <- c(-99899,-9999,-998.9) # TODO
#   
#   
#   
# }
