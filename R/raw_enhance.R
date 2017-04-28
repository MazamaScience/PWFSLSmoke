#' @keywords raw
#' @export
#' @title Process Raw Monitoring Data to Create raw_enhance Object
#' @param df raw monitor data, as created by airsis_createRawDataframe or wrcc_createRawDataframe
#' @description Processes raw monitor data to add a uniform time axis and consistent data columns that can be handled by various 
#' \code{raw~} functions. All original raw data is retained, and the following additional columns are added:
#' 
#' \itemize{
#' \item{dataSource}
#' \item{longitude}
#' \item{latitude}
#' \item{temperature}
#' \item{humidity}
#' \item{windSpeed}
#' \item{windDir}
#' \item{pressure}
#' \item{pm25}
#' }
#' 
#' The \code{datetime} column in the incoming dataframe may have missing hours. This time axis is expanded to a
#' uniform, hourly axes with missing data fields added for data columns.
#' 
#' @return Dataframe with original raw data, plus new columns with raw naming scheme for downstream use.
#' @examples
#' \dontrun{
#' df <- airsis_createRawDataframe(startdate=20160901, enddate=20161015, provider='USFS', unitID=1012)
#' df <- raw_enhance(df)
#' rawPlot_timeseries(df, tlim=c(20160908,20160917))
#' }

# TODO: Add check to ensure that column names match expected for the type passed in (e.g. EBAM_AIRSIS).

raw_enhance <- function(df) {

  # Identify monitor type and source and save to variable
  monType <- paste0(df$monitorType[1],"_",df$rawSource[1],sep="")

  # Create uniform attributes
  if ( monType=="EBAM_AIRSIS" ) {
    df$temperature <- df$AT
    df$humidity <- df$RHx
    df$windSpeed <- df$W.S
    df$windDir <- df$W.D
    df$pm25 <- df$ConcHr*1000
    df$longitude <- df$medoidLon
    df$latitude <- df$medoidLat
    df$pressure <- as.numeric(NA)
  } else if ( monType=="ESAM_AIRSIS" ) {
    df$temperature <- df$AT.C.
    df$humidity <- df$RHx...
    df$windSpeed <- df$WS.M.S.
    df$windDir <- df$WD.Deg.
    df$pm25 <- df$Conc.mg.m3.*1000
    df$longitude <- df$medoidLon
    df$latitude <- df$medoidLat
    df$pressure <- (df$BP.PA.)/100 #Adjust to hPa for consistency w/ ESAM_WRCC
  } else if ( monType=="ESAM_WRCC" ) {
    df$temperature <- df$AvAirTemp
    df$humidity <- df$RelHumidity
    df$windSpeed <- df$WindSpeed
    df$windDir <- df$WindDir
    df$pm25 <- df$ConcRT
    df$longitude <- df$medoidLon
    df$latitude <- df$medoidLat
    df$pressure <- df$BaromPress
  } else if ( monType=="EBAM_WRCC" ) {
    df$temperature <- df$AvAirTemp
    df$humidity <- df$RelHumidity
    df$windSpeed <- df$WindSpeed
    df$windDir <- df$WindDir
    df$pm25 <- df$ConcRT
    df$longitude <- df$medoidLon
    df$latitude <- df$medoidLat
    df$pressure <- NA
  } else {
    stop(paste0("Monitor type (",monType,") not currently supported.",sep=""))
  }

  # Fill missing hours
  datetime <- data.frame(datetime = seq(df$datetime[1],df$datetime[nrow(df)],by=3600))
  df <- dplyr::left_join(datetime,df,by="datetime")

  # Apply timezone to hours with location information
  df$timezone <- ""
  for ( deploymentID in unique(df$deploymentID) ) {
    if ( is.na(deploymentID) ) {
        break
      } else {
        index <- which(df$deploymentID==deploymentID)
        df$timezone[index] <- MazamaSpatialUtils::getTimezone(df$longitude[index[1]], df$latitude[index[1]],
                                                              countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
    }
  }
  
  # for (i in unique(df$deploymentID)) { 
  #   if (is.na(i)) { break } else {
  #     index <- which(df$deploymentID==i)
  #     df$timezone[index] <- MazamaSpatialUtils::getTimezone(df$longitude[index[1]], df$latitude[index[1]],
  #                                                           countryCodes=NULL, useBuffering=TRUE) #modified from addMazamaMetadata.R
      

  # Apply timezone to missing hours based on TZ of prior good hour
  tz <- df$timezone[1]
  for ( i in 2:nrow(df) ) {
    if(df$timezone[i]=="") {df$timezone[i] <- tz}
    tz <- df$timezone[i]
  }

  return(df)

}
