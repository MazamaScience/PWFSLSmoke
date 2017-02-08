
# ----- AQI breaks -------------------------------------------------------------

#' AQI breaks and associated names and colors
#' 
#' @docType data
#' @name AQI
#' @title Official Air Quality Index Levels, Names and Colors
#' @format A list with five elements
#' @export
#' @description
#' Official AQI levels, names and colors are provided in a list for easy coloring and labeling.
#' @details
#' 
#' AQI breaks were obtained from \url{http://www.arb.ca.gov/carpa/toolkit/data-to-mes/wildfire-smoke-guide.pdf}
#' 
#' AQI colors are defined in \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-dec2013.pdf}
#' @note
#' The low end of each break category is used as the breakpoint.

AQI <- list(breaks_24=c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),
            colors=c(grDevices::rgb(0,228/255,0),
                     grDevices::rgb(255/255,255/255,0),
                     grDevices::rgb(255/255,126/255,0),
                     grDevices::rgb(255/255,0,0),
                     grDevices::rgb(143/255,63/255,151/255),
                     grDevices::rgb(126/255,0,35/255)),
            names=c('good','moderate','USG','unhealthy','very unhealthy','extreme'))

# ----- State codes in the lower 48 --------------------------------------------

#' CONUS state codes
#' 
#' @docType data
#' @name CONUS
#' @title CONUS State Codes
#' @format A vector with 48 elements
#' @export
#' @description
#' State codes for the 48 states that make up the Continental US

CONUS <- c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID",
           "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI",
           "MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
           "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
           "TX","UT","VT","VA","WA","WV","WI","WY","DC")

