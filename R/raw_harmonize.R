# ==============================

# outline:
# get dataframe in hand
# look at dataframe to determine monitor type and structure
# create new dataframe with select columns and data formatted to consistent standard
# return new dataframe

# TEMP BACKGROUND FILES FOR TESTING, ETC =================

if ( FALSE ) {
 
  library(PWFSLSmoke)
  library(openair)
  setwd("~/Projects/PWFSLSmoke/")
  
  load("localData/airsis_rawList.RData")
  
  names(airsis_rawList)
  lapply(airsis_rawList, names)
  
  #raw <- airsis_rawList$Plain #EBAM AIRSIS
  #raw <- airsis_rawList$Naches #ESAM AIRSIS
  raw <- airsis_rawList$Usk #ESAM WRCC
  
  rawSource <- "WRCC"
  
  threeExamples <- list(airsis_rawList$Plain,airsis_rawList$KettleFalls,airsis_rawList$Usk)
  names(threeExamples) <- c("EBAM_AIRSIS","ESAM_AIRSIS","ESAM_WRCC")
  lapply(threeExamples, names)
  lapply(threeExamples, head)
  
}


# FUNCTION ================================

raw_harmonize <- function(raw,rawSource="AIRSIS") {

# dataframe is in hand; let's go about identifying the type
monType <- paste0(raw$monitorType[1],"_",rawSource,sep="")

df <- data.frame()

# NOTE: NEED TO CHECK UNITS OF MEASURE!!!!
# NOTE: should add check to ensure that data matches the type passed in (e.g. EBAM_AIRSIS); check col names

if (monType=="EBAM_AIRSIS") {
  # add line here to check for data consistency
  df <- data.frame(raw$Alias,
                   raw$datetime,
                   raw$AT,
                   raw$RHx,
                   raw$W.S,
                   raw$W.D,
                   raw$Flow,
                   raw$RHi,
                   raw$BV,
                   raw$Alarm,
                   raw$COncRT,
                   raw$monitorType,
                   raw$Serial.Number,
                   raw$deploymentID,
                   raw$medoidLon,
                   raw$medoidLat,
                   "AIRSIS")
} else if (monType=="ESAM_AIRSIS") {
  # add line here to check for data consistency
  df <- data.frame(raw$Alias,
                   raw$datetime,
                   raw$AT.C.,
                   raw$RHx...,
                   raw$WS.M.S.,
                   raw$WD.Deg.,
                   raw$Flow.l.m.,
                   raw$RHi...,
                   raw$BV.V.,
                   raw$Alarm,
                   raw$Conc.mg.m3.,
                   raw$monitorType,
                   raw$Serial.Number,
                   raw$deploymentID,
                   raw$medoidLon,
                   raw$medoidLat,
                   "AIRSIS")
} else if (monType=="ESAM_WRCC") {
  # add line here to check for data consistency
  df <- data.frame(raw$monitorName,
                   raw$datetime,
                   raw$AvAirTemp,
                   raw$RelHumidity,
                   raw$WindSpeed,
                   raw$WindDir,
                   raw$AvAirFlw,
                   raw$SensorIntRH,
                   raw$BatteryVoltage,
                   raw$Alarm,
                   raw$ConcRT,
                   raw$monitorType,
                   raw$SerialNumber,
                   raw$deploymentID,
                   raw$medoidLon,
                   raw$medoidLat,
                   "WRCC")
} else {
    print("Warning - Type not supported")
}

names(df) <- c("MonitorName","DateTime","AirTemperature(C)","RH_External(%)","WindSpeed(m/s)",
               "WindDirection(deg)","FlowRate(l/m)","RH_Internal(%)","Voltage(V)","AlarmStatus",
               "RTConc(mg/m3)","MonitorType","SerialNumber","DeploymentID","Longitude","Latitude",
               "DataSource")

# could possibly pull out last six columns (meta data) into a $meta field like the ws_monitor object

return(df)

}