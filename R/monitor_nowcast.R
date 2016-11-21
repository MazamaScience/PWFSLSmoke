
# Nowcast URLs:
#
# * https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)
# * https://airnow.zendesk.com/hc/en-us/articles/212303177-How-does-AirNow-make-the-Current-PM-Air-Quality-Index-AQI-maps-
# * https://www.arb.ca.gov/carpa/iascpresentations/2015/nowcastaqiforpm25overview.pdf ("nowcast")
# * https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf ("new nowcast" or "Reff method")
# * http://aqicn.org/faq/2015-03-15/air-quality-nowcast-a-beginners-guide/
#
# Javascript implementation of nowcast:
#
# * https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
#
# Here is an implementation of a "nowcast" function for a vector of pm2.5 values
#
#
# .nowCast <- function(x)
# {
#  returnVal = as.numeric(NA)
#  
#  if ( !all(is.na(x)) ) {
#    
#     weightFac <- 1 - (diff(range(x, na.rm = TRUE)) / max(x, na.rm = TRUE))
#     weightFac <- if (is.infinite(weightFac)) {
#       NA
#     } else if (weightFac > 1) {
#       1 
#     } else if (weightFac < 0.5) {
#       0.5
#     } else {
#       weightFac
#     }
#     
#     if (length(na.omit(x[11:12])) > 1) {
#       returnVal = weighted.mean(x, weightFac^(11 : 0), na.rm = TRUE)
#     }
#     
#   }
#   
#   return(returnVal)
# }
# 
