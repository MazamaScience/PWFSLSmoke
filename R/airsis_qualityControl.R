#' @keywords AIRSIS
#' @export
#' @title Apply Quality Control to Raw AIRSIS Dataframe
#' @param df single site dataframe created by airsis_downloadData()
#' @param verbose logical flag to generate verbose output
#' @description Perform various QC measures on AIRSIS data.
#' 
#' @return  Cleaned up dataframe of AIRSIS monitor data.


airsis_qualityControl <- function(df, verbose=FALSE) {
  
  #   > names(df)
  #    [1] "MasterTable_ID" "Alias"          "Latitude"       "Longitude"      "Date.Time.GMT" 
  #    [6] "COncRT"         "ConcHr"         "Flow"           "W.S"            "W.D"           
  #   [11] "AT"             "RHx"            "RHi"            "BV"             "FT"            
  #   [16] "Alarm"          "Type"           "Serial.Number"  "Version"        "Sys..Volts"    
  #   [21] "TimeStamp"      "PDate"          "monitorName"
  
  monitorName <- df$monitorName[1]
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags
  
  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range and non-zero
  lonMask <- df$Longitude >= -180 & df$Longitude <= 180 & df$Longitude != 0 & !is.na(df$Longitude)
  latMask <- df$Latitude >= -90 & df$Latitude <= 90 & df$Latitude != 0 & !is.na(df$Latitude)
  df <- df[lonMask & latMask,]
  
  # ----- Time ----------------------------------------------------------------
  
  # TODO:  Times are in local time so we need to extract the timezone and know the location
  # TODO:  before we can procede.
  
  # Add a POSIXct datetime
  df$datetime <- lubridate::mdy_hms(df$Date.Time.GMT)
  
  
  # ----- Type ----------------------------------------------------------------
  
  df <- df[df$Type == "PM 2.5",]
  
  if (nrow(df) < 1) stop(paste0(monitorName,' has no valid records'))
  
  ebamDF <- df[df$Type == "PM 2.5",]
  
  
  # Leland Tarnay QC -----------------------------------------------------------
  
  ###tmp.2014_YOSE_ebam1_ftp$concQA <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                              ifelse(Flow < 16.7 * .95, "FlowLow",
  ###                              ifelse(Flow > 16.7 * 1.05, "FlowHigh",
  ###                              ifelse(AT > 45, "HighTemp",
  ###                              ifelse(RHi > 45,"HighRHi",
  ###                              ifelse(ConcHr < 0, "Negative",
  ###                              ifelse(ConcHr > .984, "HighConc", 'OK')))))))
  ###  
  ###tmp.2014_YOSE_ebam1_ftp$concHR <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                              ifelse(concQA == 'Negative', 0,
  ###                              ifelse(concQA == 'OK', ConcHr * 1000 , NA)))
  
  goodFlow <- !is.na(ebamDF$Flow) & ebamDF$Flow >= 16.7*0.95 & ebamDF$Flow <= 16.7*1.05
  goodAT <- !is.na(ebamDF$AT) & ebamDF$AT <= 45
  goodRHi <- !is.na(ebamDF$RHi) & ebamDF$RHi <= 45
  goodConcHr <- !is.na(ebamDF$ConcHr) & ebamDF$ConcHr <= 0.984
  gooddatetime <- !is.na(ebamDF$datetime) & ebamDF$datetime < lubridate::now("UTC") # saw a future date once
  
  if (verbose) {
    print(paste0('Flow has ',sum(!goodFlow,na.rm=TRUE),' missing or out of range values.'))
    print(paste0('AT has ',sum(!goodAT,na.rm=TRUE),' missing or out of range values.'))
    print(paste0('RHi has ',sum(!goodRHi,na.rm=TRUE),' missing or out of range values.'))
    print(paste0('ConcHr has ',sum(!goodConcHr,na.rm=TRUE),' missing or out of range values.'))
    print(paste0('datetime has ',sum(!gooddatetime,na.rm=TRUE),' missing or out of range values.'))
  }
  
  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  
  ebamDF <- ebamDF[goodMask,]
  
  
  # ----- More QC -------------------------------------------------------------
  
  # Bind the ebam and esampler dataframes back together
  ###df <- rbind(ebamDF, esamplerDF)
  df <- ebamDF
  
  # TODO:  Other QC?
  

  return(df)
  
}
