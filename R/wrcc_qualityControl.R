#' @keywords WRCC
#' @export
#' @title Apply Quality Control to Raw WRCC Dataframe
#' @param df single site dataframe created by wrcc_readData()
#' @param verbose logical flag to generate verbose output
#' @description Perform various QC measures on WRCC data.
#' 
#' @return  Cleaned up dataframe of WRCC monitor data.


wrcc_qualityControl <- function(df, verbose=FALSE) {
  
  # NOTE:  > names(df)
  # NOTE:   [1] "Date.Time.YYYYMMDDhhmm" "GPS.Lat."               "GPS.Lon."               "Type."                 
  # NOTE:   [5] "Serial.Number"          "Conc.RT"                "Misc..1"                "Ave..Air.Flw"          
  # NOTE:   [9] "Av.Air.Temp"            "Rel.Humidty"            "Barom.Press"            "Sensor.Int.AT"         
  # NOTE:  [13] "Sensor.Int.RH"          "Wind.Speed"             "Wind.Direc"             "Battery.Voltage"       
  # NOTE:  [17] "Alarm."                 "monitorName"  
  
  monitorName <- df$monitorName[1]
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags (lots of variants of -99x???)
  df[df < -900] <- NA
  df[df == -9.9899] <- NA
  df[df == 99999] <- NA
  
  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range and non-zero
  lonMask <- df$GPS.Lon. >= -180 & df$GPS.Lon. <= 180 & df$GPS.Lon. != 0
  latMask <- df$GPS.Lat. >= -90 & df$GPS.Lat. <= 90 & df$GPS.Lat. != 0
  df <- df[lonMask & latMask,]
  
  # TODO:  Convert longitudes <-10 to positive so they don't end up in China.
  
  # ----- Time ----------------------------------------------------------------

  # Add a POSIXct datetime (All times in the file are in GMT desplite being labeled 'LST'.)
  df$datetime <- lubridate::ymd_hm(df$Date.Time.YYYYMMDDhhmm)
  
  
  # ----- Type ----------------------------------------------------------------

  # Type: 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
  typeMask <- df$Type. == 0 | df$Type. == 9
  df <- df[typeMask,]
  
  if (nrow(df) < 1) stop(paste0(monitorName,' has no valid records'))
  
  ebamDF <- df[df$Type == 0,]
  esamplerDF <- df[df$Type == 9,]
  
  
  # Leland Tarnay QC for E-BAM ------------------------------------------------
  
  ###tmp.2014_YOSE_ebam1_ftp$concQA <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                              ifelse(Flow < 16.7 * .95, "FlowLow",
  ###                              ifelse(Flow > 16.7 * 1.05, "FlowHigh",
  ###                              ifelse(AT > 45, "HighTemp",
  ###                              ifelse(RHi > 45,"HighRHi",
  ###                              ifelse(ConcHr < 0, "Negative",
  ###                              ifelse(ConcHr > .984, "HighConc", 'OK')))))))
  ###  
  ###tmp.2014_YOSE_ebam1_ftp$concHR <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                             ifelse(concQA == 'Negative', 0,
  ###                             ifelse(concQA == 'OK', ConcHr * 1000 , NA)))
  
  # Ave..Air.Flw
  goodFlow <- ebamDF$Ave..Air.Flw >= 16.7*0.95 & ebamDF$Ave..Air.Flw <= 16.7*1.05
  goodFlow[is.na(goodFlow)] <- FALSE
  if (verbose) cat(paste0('E=BAM Flow has ',sum(is.na(ebamDF$Ave..Air.Flw)),' missing values and ',sum(!goodFlow,na.rm=TRUE),' out of range values.\n'))
  
  # Sensor.Int.AT
  goodAirTemp <- ebamDF$Sensor.Int.AT <= 45
  goodAirTemp[is.na(goodAirTemp)] <- FALSE
  if (verbose) cat(paste0('E=BAM AirTemp has ',sum(is.na(ebamDF$Sensor.Int.AT)),' missing values and ',sum(!goodAirTemp,na.rm=TRUE),' out of range values.\n'))
  
  # Sensor.Int.RH
  goodRelativeHumidity <- ebamDF$Sensor.Int.RH <= 45
  goodRelativeHumidity[is.na(goodRelativeHumidity)] <- FALSE
  if (verbose) cat(paste0('E=BAM RelativeHumidity has ',sum(is.na(ebamDF$Sensor.Int.RH)),' missing values and ',sum(!goodRelativeHumidity,na.rm=TRUE),' out of range values.\n'))
  
  # Conc.RT
  goodConcentration <- ebamDF$Conc.RT <= 984 # NOTE:  Concentration has already been multiplied by 1000.
  goodConcentration[is.na(goodConcentration)] <- FALSE
  if (verbose ) cat(paste0('E=BAM Concentration has ',sum(is.na(ebamDF$Conc.RT)),' missing values and ',sum(!goodConcentration,na.rm=TRUE),' out of range values.\n'))
  
  goodMask <- goodFlow & goodAirTemp & goodRelativeHumidity & goodConcentration
  
  if (verbose) cat(paste0('E=BAM has ',nrow(ebamDF),' rows of data of which ',sum(is.na(goodMask)), ' are NA and ', sum(!goodMask,na.rm=TRUE),' are flagged as bad\n\n'))
  
  ebamDF <- ebamDF[goodMask,]
  
  # Leland Tarnay QC for E-Sampler --------------------------------------------
  
  ###tmp.2013_NIFC_GOES65_wrcc$concQA <- with(tmp.2013_NIFC_GOES65_wrcc,
  ###                                         ifelse(Flow < 2 "FlowLow",
  ###                                         ifelse(Flow > 2, "FlowHigh",
  ###                                         ifelse(AT > 150, "HighTemp",
  ###                                         ifelse(RHi > 55,"HighRHi",
  ###                                         ifelse(ConcHr < 0, "Negative",
  ###                                         ifelse(ConcHr > 984, "HighConc", 'OK')))))))
  ####create a concHR numerical column, with NA values that aren't verbose about errors..
  ###  
  ###tmp.2013_NIFC_GOES65_wrcc$concHR <- with(tmp.2013_NIFC_GOES65_wrcc,
  ###                                         ifelse(concQA == 'Negative', 0,
  ###                                         ifelse(concQA == 'OK', ConcHr, NA)))
  
  goodFlow <- esamplerDF$Ave..Air.Flw == 2
  goodFlow[is.na(goodFlow)] <- FALSE
  if (verbose) cat(paste0('E-Sampler Flow has ',sum(is.na(esamplerDF$Ave..Air.Flw)),' missing values and ',sum(!goodFlow,na.rm=TRUE),' out of range values.\n'))
  
  # NOTE:  Conversation with Sim and Lee on 2015-07-09. We should include all AT unless it is > 45.
  ###goodAirTemp <- esamplerDF$Sensor.Int.AT <= 45
  ###goodAirTemp[is.na(goodAirTemp)] <- FALSE
  goodAirTemp <- rep(TRUE,length(esamplerDF$Sensor.Int.AT))
  goodAirTemp[esamplerDF$Sensor.Int.AT > 45] <- FALSE
  if (verbose) cat(paste0('E-Sampler AirTemp has ',sum(is.na(esamplerDF$Sensor.Int.AT)),' missing values and ',sum(!goodAirTemp,na.rm=TRUE),' out of range values.\n'))
  
  # NOTE:  Conversation with Sim and Lee on 2015-07-09. Always include RH for now.
  goodRelativeHumidity <- esamplerDF$Sensor.Int.RH <= 100 ### 45
  goodRelativeHumidity[is.na(goodRelativeHumidity)] <- FALSE
  if (verbose) cat(paste0('E-Sampler RelativeHumidity has ',sum(is.na(esamplerDF$Sensor.Int.RH)),' missing values and ',sum(!goodRelativeHumidity,na.rm=TRUE),' out of range values.\n'))
  
  # NOTE:  Conversation with Sim and Lee on 2015-07-09.
  ###goodConcentration <- esamplerDF$Conc.RT <= 984
  goodConcentration <- rep(TRUE,nrow(esamplerDF))
  goodConcentration[is.na(goodConcentration)] <- FALSE
  if (verbose) cat(paste0('E-Sampler Concentration has ',sum(is.na(esamplerDF$Conc.RT)),' missing values and ',sum(!goodConcentration,na.rm=TRUE),' out of range values.\n'))
  
  goodMask <- goodFlow & goodAirTemp & goodRelativeHumidity & goodConcentration
  
  if (verbose) cat(paste0('E-Sampler has ',nrow(esamplerDF),' rows of data of which ',sum(is.na(goodMask)), ' are NA and ', sum(!goodMask,na.rm=TRUE),' are flagged as bad\n\n'))
  
  esamplerDF <- esamplerDF[goodMask,]
  
  
  # More QC -------------------------------------------------------------------
  
  # Bind the ebam and esampler dataframes back together
  df <- rbind(ebamDF, esamplerDF)

  # NOTE:  Remove records with no value for ConcHr -- the PM2.5 measurement we will be using.
  # NOTE:  Many records have NA here and these can safely be ignored
  
  missingMask <- is.na(df$Conc.RT)                  # many records are missing the pm2.5 value
  futureMask <- df$datetime >= lubridate::now("UTC") # saw a future date once
  # TODO:  Other QC?
  
  goodMask <- !missingMask & !futureMask
  
  df <- df[goodMask,]
  
  return(df)
  
}
