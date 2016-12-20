# #' @keywords raw_enhance
# #' @export
# #' @title Process Raw Monitoring Data to Create raw_enhance Object
# #' @param raw raw monitor data, as created by airsis_createRawDataframe or wrcc_createRawDataframe
# #' @param rawSource source of raw monitor data (i.e. "AIRSIS" or "WRCC")
# #' @description Processes raw monitor data to create a consistent format that can be handled by various raw* functions.
# #' All original raw data is retained, and additional columns are added with names add consistently-named columns enabling
# #' processing from various sources.
# #' @return Dataframe with original raw data, plus new columns with raw naming scheme for downstream use.
# 
# ==============================
# 
# # NOTES: ===================
# # Should add check to ensure that data matches the type passed in (e.g. EBAM_AIRSIS); check col names
# # If names don't match expected, we'll get NULL values for an entire field, which isn't ideal.
# 
# # Future work and other ideas:
# 
# # could possibly pull out last six columns (meta data) into a $meta field like the ws_monitor object.
# # Could call it raw_monitor (as opposed to ws_monitor). Only difference is that there is more than one
# # column of data for each monitor, so this type would not be as amendable to combining with other monitors
# # in the same way as the ws_monitor object.
# 
# # There is other data available for most (but not all monitors) that could be included, if we are
# # OK with some monitor types always having NULLs in certain fields.
# 
# # TEMP BACKGROUND FILES FOR TESTING, ETC =================
# 
# # NOTE: Leave the following commented out when pushed to package, but leave here for quick reference, e.g. troubleshooting
# if ( FALSE ) {
# 
#   library(PWFSLSmoke)
#   library(openair)
#   setwd("~/Projects/PWFSLSmoke/")
# 
#   load("localData/airsis_rawList.RData")
# 
#   names(airsis_rawList)
#   lapply(airsis_rawList, names)
# 
#   threeExamples <- list(airsis_rawList$Plain,airsis_rawList$KettleFalls,airsis_rawList$Usk)
#   names(threeExamples) <- c("EBAM_AIRSIS","ESAM_AIRSIS","ESAM_WRCC")
#   lapply(threeExamples, names)
#   lapply(threeExamples, head)
# 
#   #raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
#   raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
#   #raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
# 
# }
# # 
# # # FUNCTION ================================
# # 
# # 
# # # TODO:  have rawSource be added to raw dataframe by createRawDataframe, so don't have to call as argument
# raw_enhance <- function(raw, rawSource="AIRSIS") {
# 
#   # Identify monitor type and source and save to variable
#   monType <- paste0(raw$monitorType[1],"_",rawSource,sep="")
# 
#   # NOTE:  Could improve logic below to automate a determination as to what kind of data we're working with...
#   # NOTE:  doing so would eliminate the need to pass in the source; instead just determine source based on type and fields
# 
#   # CREATE NEW FIELDS ==================
#   # TODO:  NEED TO CHECK UNITS OF MEASURE!!!!
#   if ( monType=="EBAM_AIRSIS" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     raw$temperature <- raw$AT
#     raw$humidity <- raw$RHx
#     raw$windSpeed <- raw$W.S
#     raw$windDir <- raw$W.D
#     raw$pm25 <- raw$ConcHr*1000 # TODO: Review this field for appropriateness (e.g. could use COncRT?)
#     raw$longitude <- raw$medoidLon
#     raw$latitude <- raw$medoidLat
#     raw$pressure <- as.numeric(NA)
#     raw$dataSource <- "AIRSIS"
# 
#   } else if ( monType=="ESAM_AIRSIS" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     raw$temperature <- raw$AT.C.
#     raw$humidity <- raw$RHx...
#     raw$windSpeed <- raw$WS.M.S.
#     raw$windDir <- raw$WD.Deg.
#     raw$pm25 <- raw$Conc.mg.m3.*1000
#     raw$longitude <- raw$medoidLon
#     raw$latitude <- raw$medoidLat
#     raw$pressure <- (raw$BP.PA.)/100 #Adjust to hPa for consistency w/ ESAM_WRCC
#     raw$dataSource <- "AIRSIS"
# 
#   } else if ( monType=="ESAM_WRCC" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     raw$temperature <- raw$AvAirTemp
#     raw$humidity <- raw$RelHumidity
#     raw$windSpeed <- raw$WindSpeed
#     raw$windDir <- raw$WindDir
#     raw$pm25 <- raw$ConcRT*1000
#     raw$longitude <- raw$medoidLon
#     raw$latitude <- raw$medoidLat
#     raw$pressure <- raw$BaromPress
#     raw$dataSource <- "WRCC"
# 
#   } else {
# 
#     stop(paste0("Monitor type (",monType,") not currently supported.",sep=""))
# 
#   }
# 
#   #FILL MISSING HOURS ===========
#   df <- as.data.frame(seq(raw$datetime[1],raw$datetime[nrow(raw)],by=3600))
#   names(df) <- "datetime"
#   df <- dplyr::left_join(df,raw,by="datetime")
# 
#   #TIMEZONE ============
#   df$timezone <- ""
#   for (i in unique(df$deploymentID)) {
#     if (is.na(i)) { break } else {
#       index <- which(df$deploymentID==i)
#       df$timezone[index] <- MazamaSpatialUtils::getTimezone(df$longitude[index[1]], df$latitude[index[1]],
#                                                             countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
#     }
#   }
# 
#   #Apply timezone to missing hours based on TZ of last good hour
#   tz <- df$timezone[1]
#   for (i in 2:nrow(df)) {
#     if(df$timezone[i]=="") {df$timezone[i] <- tz}
#     tz <- df$timezone[i]
#   }
# 
#   return(df)
# 
# }