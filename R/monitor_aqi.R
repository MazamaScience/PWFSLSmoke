#steps: truncate the highest concentration value, find the break points, calculate the equation, round the integer
# equation:
# Ip = (Ihi - Ilo)/(BPhi - BPlo) * (Cp - BPlo) + Ilo

# reference table:

#    O3 - 8h        O3 - 1h       PM2.5             PM10         CO           SO2          NO2          AQI
# 0.000 - 0.054        -          0.0 – 12.0        0 - 54    0.0 - 4.4     0 - 35         0 - 53      0 - 50   Good
# 0.055 - 0.070        -         12.1 – 35.4       55 - 154   4.5 - 9.4    36 - 75        54 - 100    51 - 100  Moderate
# 0.071 - 0.085  0.125 - 0.164   35.5 – 55.4      155 - 254   9.5 - 12.4   76 - 185      101 - 360   101 - 150  UnhealthyForSensitiveGroups
# 0.086 - 0.105  0.165 - 0.204  (55.5 - 150.4)3   255 - 354  12.5 - 15.4 (186 - 304)4    361 - 649   151 - 200  Unhealthy
# 0.106 - 0.200  0.205 - 0.404 (150.5 - (250.4)3  355 - 424  15.5 - 30.4 (305 - 604)4    650 - 1249  201 - 300  VeryUnhealthy
#                0.405 - 0.504 (250.5 - (350.4)3  425 - 504  30.5 - 40.4 (605 - 804)4   1250 - 1649  301 - 400  Hazadrous                   
#                0.505 - 0.604 (350.5 - 500.4)3   505 - 604  40.5 - 50.4 (805 - 1004)4  1650 - 2049  401 - 500  Hazardous

monitor_aqi <- function( ws_monitor, parameter="pm25", hour=24 ) {
  
  AQIbps <- list( Good <- c(0, 53),
                  Moderate <- c(54, 100),
                  UnhealthySensitive <- c(101, 360),
                  Unhealthy <- c(361, 649),
                  VeryUnhealthy <- c(650, 1249),
                  Hazardrous1 <- c(1250, 1649),
                  Hazardrous2 <- c(1650, 2049))
  
  data <- ws_monitor$data
  
  startdate <- as.Date(min(data[,1]))
  enddate <- as.Date(max(data[,1]))
  AQIdata <- as.data.frame(seq(startdate, enddate, by="day"))
  colnames(AQIdata) <- "datetime"
  AQIdata$concentration <- NA
  
  if (parameter == "o3" && hour == 8) {
    
    breakPoints <- list( Good <- c(0.000, 0.054),
                         Moderate <- c(0.055, 0.070),
                         UnhealthySensitive <- c(0.071, 0.085),
                         Unhealthy <- c(0.086, 0.105),
                         VeryUnhealthy <- c(0.106, 0.200))
    
    #data <- apply(data, 2, function(x){ round(x, 3) })
    
  } else if (parameter == "o3" && hour == 1) {
    
    breakPoints <- list( UnhealthySensitive <- c(0.125, 0.164),
                         Unhealthy <- c(0.165, 0.204),
                         VeryUnhealthy <- c(0.205, 0.404),
                         Hazadrous1 <- c(0.405, 0.504),
                         Hazadrous2 <- c(0.505, 0.604))
    
    #data <- apply(data, 2, function(x){ round(x, 3) })
    
  } else if (parameter == "pm25") {
    
    breakPoints <- list( Good <- c(0.0, 12.0),
                         Moderate <- c(12.1, 35.4),
                         UnhealthySensitive <- c(35.5, 55.4),
                         Unhealthy <- c(55.5, 150.4),
                         VeryUnhealthy <- c(150.5, 250.4),
                         Hazardrous1 <- c(250.5, 350.4),
                         Hazardrous2 <- c(350.5, 500.4))
    
    data <- apply(data, 2, function(x){ round(x, 1) })
    
  } else if (parameter == "pm10") {
    breakPoints <- list( Good <- c(0, 54),
                         Moderate <- c(55, 154),
                         UnhealthySensitive <- c(155, 254),
                         Unhealthy <- c(255, 354),
                         VeryUnhealthy <- c(355, 424),
                         Hazardrous1 <- c(425, 504),
                         Hazardrous2 <- c(505, 604))
    
    #data <- apply(data, 2, function(x){ round(x, 0) })
    
  } else if (parameter == "co") {
    breakPoints <- list( Good <- c(0.0, 4.4),
                         Moderate <- c(4.5, 9.4),
                         UnhealthySensitive <- c(9.5, 12.4),
                         Unhealthy <- c(12.5, 15.4),
                         VeryUnhealthy <- c(15.5, 30.4),
                         Hazardrous1 <- c(30.5, 40.4),
                         Hazardrous2 <- c(40.5, 50.4))
    
    #data <- apply(data, 2, function(x){ round(x, 1) })
    
  } else if (parameter == "so2") {
    breakPoints <- list( Good <- c(0, 35),
                         Moderate <- c(36, 75),
                         UnhealthySensitive <- c(76, 185),
                         Unhealthy <- c(186, 304),
                         VeryUnhealthy <- c(305, 604),
                         Hazardrous1 <- c(605, 804),
                         Hazardrous2 <- c(805, 1004))
    
    #data <- apply(data, 2, function(x){ round(x, 0) })
    
  } else if (parameter == "no2") {
    breakPoints <- list( Good <- c(0, 53),
                         Moderate <- c(54, 100),
                         UnhealthySensitive <- c(101, 360),
                         Unhealthy <- c(361, 649),
                         VeryUnhealthy <- c(650, 1249),
                         Hazardrous1 <- c(1250, 1649),
                         Hazardrous2 <- c(1650, 2049))
    
    #data <- apply(data, 2, function(x){ round(x, 0) })
    
  } else {
    stop("The parameter you passed in is not in the list of supported pollutent type: o3, pm25, pm10, co, so2, no2")
  }
  
}




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

