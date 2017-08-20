
# ----- WRCC monitors ----------------------------------------------------------

#' Carmel Valley example dataset
#' 
#' @docType data
#' @keywords datasets
#' @name CarmelValley
#' @title Carmel Valley Example Dataset
#' @format A list with two elements
#' @description
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. It was at the time the most expensive wildifre in US history. This dataset contains
#' PM2.5 monitoring data for the monitor in Carmel Valley which shows heavy smoke
#' as well as strong diurnal cycles associated with sea breezes. Data are stored
#' as a \emph{ws_monitor} object and are used in some examples in the package
#' documentation.
NULL

#' Northwest_Megafires example dataset
#' 
#' @docType data
#' @keywords datasets
#' @name Northwest_Megafires
#' @title Northwest Megafires Example Dataset
#' @format A list with two elements
#' @description
#' In the summer of 2015 Washington state had several catastrophic wildfires that led
#' to many days of heavy smoke in eastern Washington, Oregon and northern Idaho.
#' The Northwest_Megafires dataset contains AirNow ambient monitoring data for the 
#' Pacific Northwest from May 31 through November 01, 2015 (UTC). Data are stored
#' as a \emph{ws_monitor} object and are used in many examples in the package
#' documentation.
NULL

#' WRCC monitor names and stationIDs
#' 
#' @docType data
#' @keywords datasets
#' @name WRCC
#' @title WRCC Monitor Names and Station IDs
#' @format A list with three elements
#' @description
#' The WRCC \url{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitor Archive}
#' provides access to a variety of monitors that can be accessed with the \link{wrcc_createMonitorObject}
#' function. Use of this funciton requires a valid stationID. The \code{WRCC} object is 
#' a list of three named vectors, each containing the stationIDs and associated names for
#' one of the categories of monitors available at WRCC:
#' 
#' \itemize{
#' \item{cache}
#' \item{miscellaneous}
#' \item{usfs_regional}
#' }
#' @note This list of monitors was created on Feb 09, 2017.
NULL

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
#' AQI breaks and colors are defined in
#' \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-may2016.pdf}
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

