#' @keywords AIRSIS
#' @export
#' @title Apply Quality Control to Raw AIRSIS EBAM Dataframe
#' @param df single site dataframe created by airsis_downloadData()
#' @param valid_Longitude range of valid Longitude values
#' @param valid_Latitude range of valid Latitude values
#' @param remove_Lon_zero flag to remove rows where Longitude == 0
#' @param remove_Lat_zero flag to remove rows where Latitude == 0
#' @param valid_Flow range of valid Flow values
#' @param valid_AT range of valid AT values
#' @param valid_RHi range of valid RHi values
#' @param valid_ConcHr range of valid ConcHr values
#' @description Perform various QC measures on AIRSIS EBAM data.
#' 
#' The following columns of data are tested against valid ranges:
#' \itemize{
#' \item{\code{Flow}}
#' \item{\code{AT}}
#' \item{\code{RHi}}
#' \item{\code{ConcHr}}
#' }
#' 
#' A \code{POSIXct datetime} column (UTC) is also added based on \code{Date.Time.GMT}.
#' 
#' @return Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \code{\link{airsis_qualityControl}}

airsis_EBAMQualityControl <- function(df,
                                      valid_Longitude=c(-180,180),
                                      valid_Latitude=c(-90,90),
                                      remove_Lon_zero = TRUE,
                                      remove_Lat_zero = TRUE,
                                      valid_Flow = c(16.7*0.95,16.7*1.05),
                                      valid_AT = c(-Inf,45),
                                      valid_RHi = c(-Inf,45),
                                      valid_ConcHr = c(-Inf,.984)) {
  
  #   > names(df)
  #    [1] "MasterTable_ID" "Alias"          "Latitude"       "Longitude"      "Date.Time.GMT" 
  #    [6] "COncRT"         "ConcHr"         "Flow"           "W.S"            "W.D"           
  #   [11] "AT"             "RHx"            "RHi"            "BV"             "FT"            
  #   [16] "Alarm"          "Type"           "Serial.Number"  "Version"        "Sys..Volts"    
  #   [21] "TimeStamp"      "PDate"          "monitorName"    "monitorType"
  
  monitorName <- df$monitorName[1]
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags
  
  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range
  if (remove_Lon_zero) {
    goodLonMask <- !is.na(df$Longitude) & df$Longitude >= valid_Longitude[1] & df$Longitude <= valid_Longitude[2] & df$Longitude != 0
  } else {
    goodLonMask <- !is.na(df$Longitude) & df$Longitude >= valid_Longitude[1] & df$Longitude <= valid_Longitude[2]
  }
  
  if (remove_Lat_zero) {
    goodLatMask <- !is.na(df$Latitude) & df$Latitude >= valid_Latitude[1] & df$Latitude <= valid_Latitude[2] & df$Latitude != 0
  } else {    
    goodLatMask <- !is.na(df$Latitude) & df$Latitude >= valid_Latitude[1] & df$Latitude <= valid_Latitude[2]
  }
  
  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)
  if (badRowCount > 0) {
    logger.info('Discarding %s rows with invalid location information', badRowCount)
    logger.debug('Bad location Longitudes:  %s', paste0(df$Longitude[badRows], collapse=", "))
    logger.debug('Bad location Latitudes:  %s', paste0(df$Latitude[badRows], collapse=", "))
  }
  
  df <- df[goodLonMask & goodLatMask,]
  
  # ----- Time ----------------------------------------------------------------
  
  # Add a POSIXct datetime
  df$datetime <- lubridate::mdy_hms(df$Date.Time.GMT)
  
  
  # ----- Type ----------------------------------------------------------------
  
  goodTypeMask <- !is.na(df$Type) & df$Type == "PM 2.5"
  
  badRows <- !goodTypeMask
  badRowCount <- sum(badRows)
  if (badRowCount > 0) {
    logger.info('Discarding %s rows with invalid Type information', badRowCount)
    logger.debug('Bad Types:  %s', paste0(df$Type[badRows], collapse=", "))
  }
  
  df <- df[goodTypeMask,]
  
  if (nrow(df) < 1) {
    logger.warn('No valid PM2.5 data for %s', monitorName)
    stop(paste0('No valid PM2.5 data for ', monitorName))
  }
  
  
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
  
  goodFlow <- !is.na(df$Flow) & df$Flow >= valid_Flow[1] & df$Flow <= valid_Flow[2]
  goodAT <- !is.na(df$AT) & df$AT >= valid_AT[1] & df$AT <= valid_AT[2]
  goodRHi <- !is.na(df$RHi) & df$RHi >= valid_RHi[1] & df$RHi <= valid_RHi[2]
  goodConcHr <- !is.na(df$ConcHr) & df$ConcHr >= valid_ConcHr[1] & df$ConcHr <= valid_ConcHr[2]
  gooddatetime <- !is.na(df$datetime) & df$datetime < lubridate::now("UTC") # saw a future date once
  
  logger.debug('Flow has %s missing or out of range values', sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.debug('Bad Flow values:  %s', paste0(df$Flow[!goodFlow], collapse=", "))
  logger.debug('AT has %s missing or out of range values', sum(!goodAT))
  if (sum(!goodAT) > 0) logger.debug('Bad AT values:  %s', paste0(df$AT[!goodAT], collapse=", "))
  logger.debug('RHi has %s missing or out of range values', sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.debug('Bad RHi values:  %s', paste0(df$RHi[!goodRHi], collapse=", "))
  logger.debug('ConcHr has %s missing or out of range values', sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.debug('Bad ConcHr values:  %s', paste0(df$ConcHr[!goodConcHr], collapse=", "))
  logger.debug('datetime has %s missing or out of range values', sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.debug('Bad datetime values:  %s', paste0(df$datetime[!gooddatetime], collapse=", "))
  
  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  
  df <- df[goodMask,]
  
  badQCCount <- sum(!goodMask)
  if (badQCCount > 0) {
    logger.info('Discarding %s rows because of QC logic', badQCCount)
  }
  
  
  # ----- More QC -------------------------------------------------------------
  
  # TODO:  Other QC?
  
  logger.debug('Retaining %d rows of validated measurements', nrow(df))
  
  
  return(df)
  
}
