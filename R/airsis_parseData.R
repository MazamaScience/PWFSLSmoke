#' @keywords AIRSIS
#' @export
#' @title Parse AIRSIS Data String
#' @param fileString character string containing AIRSIS data as a csv
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a dataframe. 
#' @return Dataframe of AIRSIS monitor data.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' fileString <- airsis_downloadData(USFS',unitID='1026',startdate=20150701,enddate=20151231)
#' df <- airsis_parseData(fileString)
#' }

airsis_parseData <- function(fileString) {
  
  #     Different header styles     -------------------------------------------
  
  # provider=USFS, unitID=49, year=2010
  bam1020_header <- "MasterTable_ID,Alias,Latitude,Longitude,Conc (\u00B5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate"
  
  # provider=USFS, unitID=1026, year=2010
  ebam_header <- "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate"
  
  # provider=USFS, unitID=1002, year=2010
  esam_header <- "MasterTable_ID,Alias,Latitude,Longitude,Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Alarm,Start Date/Time (GMT),Serial Number,System Volts,Data 1,Data 2,TimeStamp,PDate"
    
  # provider=USFS, unitID=70, year=2010
  olderEbam_header_1 <- "MasterTable_ID,Alias,Latitude,Longitude,Time,DataCol1,eBam,TimeStamp,PDate"
  
  # provider=APCD, unitID=4, year=2010
  olderEbam_header_2 <- "MasterTable_ID,Alias,Latitude,Longitude,TimeStamp,PDate"
  
  
  #     Determine file type     -----------------------------------------------
  
  lines <- readr::read_lines(fileString)
  
  if ( lines[1] == bam1020_header ) {
    
    # BAM1020
    
    # TODO:  Need to have uniform column names for all file types
    col_names <- c('MasterTable_ID','Alias','Latitude','Longitude','Conc..\u00B5g.m3.',
                   'Qtot..m3.','WS..KTS.','Ozone..ppb.','RTM09..mg3.','RH....',
                   'Ambient.Temp..C.','TimeStamp','PDate')
    col_types <- ''
    
    # TODO:  How to assign data with to-the-minute timestamps to a particular hour? floor?
    stop(paste0('BAM1020 file parsing is not supported'), call.=FALSE)
  
    
  } else if ( lines[1] == ebam_header ) {
    
    # EBAM
    
    col_names <- c('MasterTable_ID','Alias','Latitude','Longitude','Date.Time.GMT','Start.Date.Time..GMT.',
                   'COncRT','ConcHr','Flow','W.S','W.D',
                   'AT','RHx','RHi','BV','FT',
                   'Alarm','Type','Serial.Number','Version','Sys..Volts',
                   'TimeStamp','PDate')
    col_types <- 'ccddccddddddddddicccdcc'
    

  } else if ( lines[1] == esam_header ) {
    
    # ESampler
    
    # TODO:  Need to have uniform column names for all file types
    col_names <- c('MasterTable_ID','Alias','Latitude','Longitude','Conc.mg.m3.','Flow.l.m.','AT.C.',
                   'BP.PA.','RHx...','RHi...','WS.M.S.','WD.Deg.','BV.V.','Alarm','Start.Date.Time..GMT.',
                   'Serial.Number','System.Volts','Data.1','Data.2','TimeStamp','PDate')
    col_types <- ''
    
    # TODO:  Need to look at these.
    stop(paste0('ESampler file parsing is not supported'), call.=FALSE)

        
  } else if ( lines[1] == olderEbam_header_1 ) {
    
    # Older EBAM
    
    stop(paste0('Unrecognized format parsing is not supported -- header line = \n', lines[1]), call.=FALSE)
    ###col_names <- c('MasterTable_ID','Alias','Latitude','Longitude','Time','DataCol1','eBam','TimeStamp','PDate')
    ###col_types <- ''
    
  } else if ( lines[1] == olderEbam_header_2 ) {
    
    # Older EBAM

        stop(paste0('Unrecognized format parsing is not supported -- header line = \n', lines[1]), call.=FALSE)
    ###col_names <- c('MasterTable_ID','Alias','Latitude','Longitude','TimeStamp','PDate')
    ###col_types <- ''
    
  } else {
    
    stop(paste0('Unrecognized format parsing is not supported -- header line = \n', lines[1]), call.=FALSE)
    
  }
  
  
  #     Parse the file     ----------------------------------------------------
  
  # Remove header line, leaving only data
  fakeFile <- paste0(lines[-1], collapse='\n')
  
  df <- readr::read_csv(fakeFile, col_names=col_names, col_types=col_types)
  
  
  #     Various fixes     -----------------------------------------------------
  
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
  
 
  return(df)
}
