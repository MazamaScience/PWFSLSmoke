# ==============================

# outline:
# get dataframe in hand
# look at dataframe to determine monitor type and structure
# add new columns to dataframe with select columns and data formatted to consistent standard
# return new dataframe

# TEMP BACKGROUND FILES FOR TESTING, ETC =================

if ( FALSE ) {
  
  library(PWFSLSmoke)
  library(openair)
  setwd("~/Projects/PWFSLSmoke/")
  
  load("localData/airsis_rawList.RData")
  
  names(airsis_rawList)
  lapply(airsis_rawList, names)
  
  threeExamples <- list(airsis_rawList$Plain,airsis_rawList$KettleFalls,airsis_rawList$Usk)
  names(threeExamples) <- c("EBAM_AIRSIS","ESAM_AIRSIS","ESAM_WRCC")
  lapply(threeExamples, names)
  lapply(threeExamples, head)
  
  raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
  #raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
  #raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
  
}

# FUNCTION ================================

raw_enhanced <- function(raw,rawSource="AIRSIS") {
  
  # dataframe is in hand; let's go about identifying the type
  monType <- paste0(raw$monitorType[1],"_",rawSource,sep="")
  
  df <- raw
  
  # NOTE: NEED TO CHECK UNITS OF MEASURE!!!!
  # NOTE: should add check to ensure that data matches the type passed in (e.g. EBAM_AIRSIS); check col names
  # NOTE: If names don't match expected, we'll get NULL values for an entire field, which isn't ideal.
  
  # NOTE: Could improve logic below to automate a determination as to what kind of data we're working with...
  # NOTE: doing so would eliminate the need to pass in the source; instead just determine source based on type and fields
  
  if (monType=="EBAM_AIRSIS") {
    # add line here to check for data consistency
    
    df$airTemp <- raw$AT
    df$relHum <- raw$RHx
    df$windSpeed <- raw$W.S
    df$windDir <- raw$W.D
    df$pm25 <- raw$COncRT #NOTE: Need to review this field for appropriateness, and units
    df$longitude <- raw$medoidLon
    df$latitude <- raw$medoidLat
    #df$barPress <- NULL
    df$dataSource <- "AIRSIS"
    
  } else if (monType=="ESAM_AIRSIS") {
    # add line here to check for data consistency
    
    df$airTemp <- raw$AT.C.
    df$relHum <- raw$RHx...
    df$windSpeed <- raw$WS.M.S.
    df$windDir <- raw$WD.Deg.
    df$pm25 <- raw$Conc.mg.m3. #NOTE: Need to review this field for appropriateness, and units
    df$longitude <- raw$medoidLon
    df$latitude <- raw$medoidLat
    df$barPress <- (raw$BP.PA.)/100
    df$dataSource <- "AIRSIS"
    
  } else if (monType=="ESAM_WRCC") {
    # add line here to check for data consistency
    
    df$airTemp <- raw$AvAirTemp
    df$relHum <- raw$RelHumidity
    df$windSpeed <- raw$WindSpeed
    df$windDir <- raw$WindDir
    df$pm25 <- raw$ConcRT #NOTE: Need to review this field for appropriateness, and units
    df$longitude <- raw$medoidLon
    df$latitude <- raw$medoidLat
    df$barPress <- raw$BaromPress
    df$dataSource <- "WRCC"
    
  } else {
    print("Warning - Type not supported")
  }
  
  # could possibly pull out last six columns (meta data) into a $meta field like the ws_monitor object.
  # Could call it raw_monitor (as opposed to ws_monitor). Only difference is that there is more than one
  # column of data for each monitor, so this type would not be as amendable to combining with other monitors
  # in the same way as the ws_monitor object.
  
  # NOTE: There is other data available for most (but not all monitors) that could be included, if we are 
  # NOTE: OK with some monitor types always having NULLS in certain fields.
  
  return(df)
  
}


