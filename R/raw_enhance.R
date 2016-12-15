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
# # ==============================
# 
# # outline:
# # get dataframe in hand
# # look at dataframe to determine monitor type and structure
# # add new columns to dataframe with select columns and data formatted to consistent standard
# # return new dataframe
# 
# # NOTE:  should add check to ensure that data matches the type passed in (e.g. EBAM_AIRSIS); check col names
# # NOTE:  If names don't match expected, we'll get NULL values for an entire field, which isn't ideal.
# 
# # NOTE: Future work and other ideas:
# 
# # could possibly pull out last six columns (meta data) into a $meta field like the ws_monitor object.
# # Could call it raw_monitor (as opposed to ws_monitor). Only difference is that there is more than one
# # column of data for each monitor, so this type would not be as amendable to combining with other monitors
# # in the same way as the ws_monitor object.
# 
# # NOTE:  There is other data available for most (but not all monitors) that could be included, if we are
# # NOTE:  OK with some monitor types always having NULLs in certain fields.
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
#   raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
#   #raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
#   #raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
# 
# }
# 
# # FUNCTION ================================
# 
# raw_enhance <- function(df, rawSource="AIRSIS") {
# 
#   # Identify monitor type and source and save to variable
#   monType <- paste0(raw$monitorType[1],"_",rawSource,sep="")
# 
#   df <- raw
# 
#   # NOTE:  Could improve logic below to automate a determination as to what kind of data we're working with...
#   # NOTE:  doing so would eliminate the need to pass in the source; instead just determine source based on type and fields
# 
# 
#   # TODO:  have rawSource be added to raw dataframe by createRawDataframe
# 
#   # CREATE NEW FIELDS ==================
#   # TODO:  NEED TO CHECK UNITS OF MEASURE!!!!
#   if ( monType=="EBAM_AIRSIS" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     df$temperature <- raw$AT
#     df$humidity <- raw$RHx
#     df$windSpeed <- raw$W.S
#     df$windDir <- raw$W.D
#     df$pm25 <- raw$COncRT # TODO:  Need to review this field for appropriateness, and units
#     df$longitude <- raw$medoidLon
#     df$latitude <- raw$medoidLat
#     df$pressure <- as.numeric(NA)
#     df$dataSource <- "AIRSIS"
# 
#   } else if ( monType=="ESAM_AIRSIS" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     df$temperature <- raw$AT.C.
#     df$humidity <- raw$RHx...
#     df$windSpeed <- raw$WS.M.S.
#     df$windDir <- raw$WD.Deg.
#     df$pm25 <- raw$Conc.mg.m3. # TODO:  Need to review this field for appropriateness, and units
#     df$longitude <- raw$medoidLon
#     df$latitude <- raw$medoidLat
#     df$pressure <- (raw$BP.PA.)/100
#     df$dataSource <- "AIRSIS"
# 
#   } else if ( monType=="ESAM_WRCC" ) {
# 
#     # TODO:  add line here to check for data consistency
# 
#     df$temperature <- raw$AvAirTemp
#     df$humidity <- raw$RelHumidity
#     df$windSpeed <- raw$WindSpeed
#     df$windDir <- raw$WindDir
#     df$pm25 <- raw$ConcRT # TODO:  Need to review this field for appropriateness, and units
#     df$longitude <- raw$medoidLon
#     df$latitude <- raw$medoidLat
#     df$pressure <- raw$BaromPress
#     df$dataSource <- "WRCC"
# 
#   } else {
# 
#     stop(paste0("Monitor type (",monType,") not currently supported.",sep=""))
# 
#   }
# 
#   # TODO: Fill in missing periods with nulls!
#   
#     
#   # TODO: Assign timezones here! Should only do this for each unique pair of lat/lons. Or use deployment ID.
#   # NOTE: use !duplicated, e.g. raw$Latitude[!duplicated(raw$deploymentID)]
#   
#   df$timezone <- "N/A"
#   for (i in unique(df$deploymentID)) {
#     index <- which(df$deploymentID==i)
#     df$timezone[index] <- MazamaSpatialUtils::getTimezone(df$longitude[index[1]], df$latitude[index[1]], countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
#   }
#   
#   return(df)
# 
# }
