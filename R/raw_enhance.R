#' @keywords raw_enhance
#' @export
#' @title Process Raw Monitoring Data to Create raw_enhance Object
#' @param raw raw monitor data, as created by airsis_createRawDataframe or wrcc_createRawDataframe
#' @param rawSource source of raw monitor data (i.e. "AIRSIS" or "WRCC")
#' @description Processes raw monitor data to create a consistent format that can be handled by various raw* functions.
#' All original raw data is retained, and additional columns are added with names add consistently-named columns enabling
#' processing from various sources.
#' @return Dataframe with original raw data, plus new columns with raw naming scheme for downstream use.
#' @examples
#' \dontrun{
#' raw <- airsis_createRawDataframe(startdate = 20160901,enddate=20161015, unitID = 1012)
#' raw <- raw_enhance(raw,rawSource='AIRSIS')
#' View(raw)
#' }

# TODO: Add check to ensure that column names match expected for the type passed in (e.g. EBAM_AIRSIS).
# TODO: update createRawDataframe to include rawSource, so don't have to call as argument here
# TODO: NEED TO CHECK UNITS OF MEASURE!!!! Check for all fields below.

raw_enhance <- function(raw, rawSource="AIRSIS") {

  # Identify monitor type and source and save to variable
  monType <- paste0(raw$monitorType[1],"_",rawSource,sep="")

  # Create uniform attributes
  if ( monType=="EBAM_AIRSIS" ) {
    raw$temperature <- raw$AT
    raw$humidity <- raw$RHx
    raw$windSpeed <- raw$W.S
    raw$windDir <- raw$W.D
    raw$pm25 <- raw$ConcHr*1000 # TODO: Review this field for appropriateness (e.g. could use COncRT?)
    raw$longitude <- raw$medoidLon
    raw$latitude <- raw$medoidLat
    raw$pressure <- as.numeric(NA)
    raw$dataSource <- "AIRSIS"
  } else if ( monType=="ESAM_AIRSIS" ) {
    raw$temperature <- raw$AT.C.
    raw$humidity <- raw$RHx...
    raw$windSpeed <- raw$WS.M.S.
    raw$windDir <- raw$WD.Deg.
    raw$pm25 <- raw$Conc.mg.m3.*1000
    raw$longitude <- raw$medoidLon
    raw$latitude <- raw$medoidLat
    raw$pressure <- (raw$BP.PA.)/100 #Adjust to hPa for consistency w/ ESAM_WRCC
    raw$dataSource <- "AIRSIS"
  } else if ( monType=="ESAM_WRCC" ) {
    raw$temperature <- raw$AvAirTemp
    raw$humidity <- raw$RelHumidity
    raw$windSpeed <- raw$WindSpeed
    raw$windDir <- raw$WindDir
    raw$pm25 <- raw$ConcRT*1000
    raw$longitude <- raw$medoidLon
    raw$latitude <- raw$medoidLat
    raw$pressure <- raw$BaromPress
    raw$dataSource <- "WRCC"
  } else {
    stop(paste0("Monitor type (",monType,") not currently supported.",sep=""))
  }

  # Fill missing hours
  df <- as.data.frame(seq(raw$datetime[1],raw$datetime[nrow(raw)],by=3600))
  names(df) <- "datetime"
  df <- dplyr::left_join(df,raw,by="datetime")

  # Apply timezone to hours with location information
  df$timezone <- ""
  for (i in unique(df$deploymentID)) {
    if (is.na(i)) { break } else {
      index <- which(df$deploymentID==i)
      df$timezone[index] <- MazamaSpatialUtils::getTimezone(df$longitude[index[1]], df$latitude[index[1]],
                                                            countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
    }
  }

  # Apply timezone to missing hours based on TZ of prior good hour
  tz <- df$timezone[1]
  for (i in 2:nrow(df)) {
    if(df$timezone[i]=="") {df$timezone[i] <- tz}
    tz <- df$timezone[i]
  }

  return(df)

}