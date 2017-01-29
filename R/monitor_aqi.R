#' @keywords AQI
#' @export
#' @title Calculate AQI for ws_monitor Object
#' @param ws_monitor ws_monitor object
#' @param parameter pollutant type
#' @param hour the number of hours for concentration values to be averaged
#' @description AQI values are calculated for data in the ws_monitor object for different pollutants.
#' 
#' Available combinations of \code{parameter} and \code{hour} are:
#' \enumerate{
#' \item{parameter="o3",  hour=8}
#' \item{parameter="o3",  hour=1}
#' \item{parameter="pm25,  hour=24}
#' \item{parameter="pm10,  hour=24}
#' \item{parameter="co",  hour=8}
#' \item{parameter="so2",  hour=1}
#' \item{parameter="no2",  hour=1}
#' }
#' 
#' See references for calculation details.
#' @return A ws_monitor object with data replaced by AQI values.
#' @references \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-may2016.pdf}
#' @examples 
#' \dontrun{
#' airnow <- openaq_load(startdate=20161001, enddate=20161031)
#' airnow_AQI <- monitor_aqi(airnow)
#' }
#' 

monitor_aqi <- function( ws_monitor, parameter="pm25", hour=24 ) {
  
  if(parameter != "pm25") {
    stop(sprintf("The parameter %s cannot be implemented yet.", parameter))
  }
  
  AQIbps <- list( Good = c(0, 50),
                  Moderate = c(51, 100),
                  UnhealthySensitive = c(101, 150),
                  Unhealthy = c(151, 200),
                  VeryUnhealthy = c(201, 300),
                  Hazardrous1 = c(301, 400),
                  Hazardrous2 = c(401, 500))
  
#------------------- the concentration calculation below is solely for pm25 right now-------------------
  data <- ws_monitor$data
  datetimeUTC <- data$datetime
  
  # extract the list of unique timezones
  uniqueTimezones <- unique(ws_monitor$meta$timezone)
  
  # number of unique timezones
  n <- length(uniqueTimezones)
  
  # store the indexes of corresponding unique timezones, the plus one is needed b/c 
  # data has a datetime column that shifts all indexes to the right by one
  for( i in 1:n ) {
    assign(uniqueTimezones[i], which(ws_monitor$meta$timezone == uniqueTimezones[i]) + 1 )
  }
  
  # create sub-dataframes to store data by timezones
  uniqueTimezoneDFs <- paste0(uniqueTimezones, "_DF")
  
  for(i in 1:n ) {
    # pull out the indexes of interest
    indexes <- get( uniqueTimezones[i] )
    # convert UTC datetime to local datetime in order to calculate daily concentration
    localDatetime <- as.data.frame( format(datetimeUTC, tz=uniqueTimezones[i]) ) 
    subData <- as.data.frame( data[, indexes ] )
    DF <- dplyr::bind_cols(localDatetime, subData)
    DF[,1] <- as.character(DF[,1])
    colnames(DF)[1] <- "localDatetime"
    colnames(DF)[-1] <- colnames(data)[indexes]
    assign(uniqueTimezoneDFs[i], DF)
  }
  
  # create a dataframe to store concentration values
  startdate <- as.Date(min(data[,1]))
  enddate <- as.Date(max(data[,1]))
  concentration <- as.data.frame(seq(startdate, enddate, by="day"))
  colnames(concentration) <- "date"

  for(i in 1:n) {
    # create a dataframe for each timezone to store their respective concentration values
    DF <- get(uniqueTimezoneDFs[i])
    DFstartdate <-as.Date(min(DF$localDatetime))
    DFenddate <- as.Date(max(DF$localDatetime))
    DFconcentration <- as.data.frame(seq(DFstartdate, DFenddate, by="day"))
    colnames(DFconcentration) <- "date"
    DFconcentration[colnames(DF)[-1]] <- NA
    

    # loop over each date to get the concentration
    for( j in 1:nrow(DFconcentration) ) {
      date <- DFconcentration[j,1]
      
      if ( sum(date == DF[,1]) != 24 ) {  #NOTE 24 is for PM,subject to change
        DFconcentration[j,-1] <- NA
        
      } else { 
        indexes <- which(DF[,1] == date)
        
        for( k in 2:ncol(DF) ) {
          DFconcentration[j, k] <- ifelse( all( is.na(DF[,k][indexes] ) ), NA, round( mean(DF[,k][indexes], na.rm=TRUE), 1) ) #NOTE rounding is for PM, subject to change
        }
      }
    }
    
    concentration <- dplyr::left_join(concentration, DFconcentration, by="date")
  }
  
  # the codes below is to re-order the colomns to make the monitorID order consistent with that order in meta from ws_monitor
  concentrationTranspose <- as.data.frame(t(concentration))
  concentrationTranspose$monitorID <- colnames(concentration)
  monitorOrder <- as.data.frame( c("date", ws_monitor$meta$monitorID) )
  colnames(monitorOrder) <- "monitorID"
  concentrationTranspose <- suppressWarnings( dplyr::left_join(monitorOrder, concentrationTranspose, by="monitorID") )
  concentration <- t(concentrationTranspose)
  
  # get the colnames and rownames back
  colnames(concentration) <- concentration[1,]
  concentration <- concentration[-1,]
  rownames(concentration) <- format( as.Date(concentration[,1]), "%Y%m%d") 
  concentration <- as.data.frame(concentration)
  concentration[,1] <- as.character(concentration[,1])
  concentration[,-1] <- apply(concentration[,-1], 2, as.numeric)
  
# --------------------------- specifiying different breakpoints for parameters ----------------------------
  
  if (parameter == "o3" & hour == 8) {
    
    breakPoints = list( Good = c(0.000, 0.054),
                        Moderate = c(0.055, 0.070),
                        UnhealthySensitive = c(0.071, 0.085),
                        Unhealthy = c(0.086, 0.105),
                        VeryUnhealthy = c(0.106, 0.200))
    
    #data = apply(data, 2, function(x){ round(x, 3) })
    
  } else if (parameter == "o3" & hour == 1) {
    
    breakPoints = list( UnhealthySensitive = c(0.125, 0.164),
                        Unhealthy = c(0.165, 0.204),
                        VeryUnhealthy = c(0.205, 0.404),
                        Hazadrous1 = c(0.405, 0.504),
                        Hazadrous2 = c(0.505, 0.604))
    
    #data = apply(data, 2, function(x){ round(x, 3) })
    
  } else if (parameter == "pm25") {
    
    breakPoints = list( Good = c(0.0, 12.0),
                        Moderate = c(12.1, 35.4),
                        UnhealthySensitive = c(35.5, 55.4),
                        Unhealthy = c(55.5, 150.4),
                        VeryUnhealthy = c(150.5, 250.4),
                        Hazardrous1 = c(250.5, 350.4),
                        Hazardrous2 = c(350.5, 500.4))
    
    #data = apply(data, 2, function(x){ round(x, 1) })
    
  } else if (parameter == "pm10") {
    breakPoints = list( Good = c(0, 54),
                        Moderate = c(55, 154),
                        UnhealthySensitive = c(155, 254),
                        Unhealthy = c(255, 354),
                        VeryUnhealthy = c(355, 424),
                        Hazardrous1 = c(425, 504),
                        Hazardrous2 = c(505, 604))
    
    #data = apply(data, 2, function(x){ round(x, 0) })
    
  } else if (parameter == "co") {
    breakPoints = list( Good = c(0.0, 4.4),
                        Moderate = c(4.5, 9.4),
                        UnhealthySensitive = c(9.5, 12.4),
                        Unhealthy = c(12.5, 15.4),
                        VeryUnhealthy = c(15.5, 30.4),
                        Hazardrous1 = c(30.5, 40.4),
                        Hazardrous2 = c(40.5, 50.4))
    
    #data = apply(data, 2, function(x){ round(x, 1) })
    
  } else if (parameter == "so2") {
    breakPoints = list( Good = c(0, 35),
                        Moderate = c(36, 75),
                        UnhealthySensitive = c(76, 185),
                        Unhealthy = c(186, 304),
                        VeryUnhealthy = c(305, 604),
                        Hazardrous1 = c(605, 804),
                        Hazardrous2 = c(805, 1004))
    
    #data = apply(data, 2, function(x){ round(x, 0) })
    
  } else if (parameter == "no2") {
    breakPoints = list( Good = c(0, 53),
                        Moderate = c(54, 100),
                        UnhealthySensitive = c(101, 360),
                        Unhealthy = c(361, 649),
                        VeryUnhealthy = c(650, 1249),
                        Hazardrous1 = c(1250, 1649),
                        Hazardrous2 = c(1650, 2049))
    
    #data <- apply(data, 2, function(x){ round(x, 0) })
    
  } else {
    stop("The parameter you passed in is not in the list of supported pollutent type: o3, pm25, pm10, co, so2, no2")
  }
  

#---------------------------------  AQI calculation --------------------------------------------
# with concentration and breakpoints, we can start calculate the AQIs with the equation:
# Ip = (Ihi - Ilo)/(BPhi - BPlo) * (Cp - BPlo) + Ilo

AQIdata <- concentration
AQIdata[,-1] <- NA

for( i in 1:nrow(AQIdata) ) {
  
  for( j in 2:ncol(AQIdata) ) {
    Cp <- concentration[i, j]
    
    if( !is.na(Cp) ) {
      
      for( k in 1:length(breakPoints) ) {
        
        if ( (Cp >= breakPoints[[k]][1]) & (Cp <= breakPoints[[k]][2]) ){ #NOTE the index needs to be modified for o3
          BPhi <- breakPoints[[k]][2]
          BPlo <- breakPoints[[k]][1]
          Ihi <- AQIbps[[k]][2]
          Ilo <- AQIbps[[k]][1]
          break
        }
      }
      
      AQIdata[i,j] = round( (Ihi - Ilo)/(BPhi - BPlo) * (Cp - BPlo) + Ilo, 0 )
      
    } else {
      AQIdata[i,j]<- NA
    }
  }
}

AQI_monitor <- list(meta=ws_monitor$meta, data=AQIdata)
AQI_monitor <- structure(AQI_monitor, class=c("ws_monitor", "list") )

return(AQI_monitor)

}

#steps: truncate the highest concentration value, find the break points, calculate the equation, round the integer
# equation:
# Ip = (Ihi - Ilo)/(BPhi - BPlo) * (Cp - BPlo) + Ilo
# Ozone (ppm) – truncate to 3 decimal places
# PM2.5 (µg/m3) – truncate to 1 decimal place
# PM10 (µg/m3) – truncate to integer
# CO (ppm) – truncate to 1 decimal place
# SO2 (ppb) – truncate to integer
# NO2 (ppb) – truncate to integer

# reference table:

#    O3 - 8h        O3 - 1h       PM2.5             PM10         CO           SO2          NO2          AQI
# 0.000 - 0.054        -          0.0 – 12.0        0 - 54    0.0 - 4.4     0 - 35         0 - 53      0 - 50   Good
# 0.055 - 0.070        -         12.1 – 35.4       55 - 154   4.5 - 9.4    36 - 75        54 - 100    51 - 100  Moderate
# 0.071 - 0.085  0.125 - 0.164   35.5 – 55.4      155 - 254   9.5 - 12.4   76 - 185      101 - 360   101 - 150  UnhealthyForSensitiveGroups
# 0.086 - 0.105  0.165 - 0.204  (55.5 - 150.4)3   255 - 354  12.5 - 15.4 (186 - 304)4    361 - 649   151 - 200  Unhealthy
# 0.106 - 0.200  0.205 - 0.404 (150.5 - (250.4)3  355 - 424  15.5 - 30.4 (305 - 604)4    650 - 1249  201 - 300  VeryUnhealthy
#                0.405 - 0.504 (250.5 - (350.4)3  425 - 504  30.5 - 40.4 (605 - 804)4   1250 - 1649  301 - 400  Hazadrous                   
#                0.505 - 0.604 (350.5 - 500.4)3   505 - 604  40.5 - 50.4 (805 - 1004)4  1650 - 2049  401 - 500  Hazardous

