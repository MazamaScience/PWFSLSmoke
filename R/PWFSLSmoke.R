
# ----- Example datasets ------------------------------------------------------

#' Carmel Valley example dataset
#'
#' @docType data
#' @keywords datasets
#' @name Carmel_Valley
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

# ----- Internal Package State -------------------------------------------------

pwfslSmokeEnv <- new.env(parent = emptyenv())
pwfslSmokeEnv$googleApiKey <- NULL
pwfslSmokeEnv$esriToken <- NULL

#' @docType data
#' @keywords environment
#' @name googleApiKey
#' @title API Key used for Google Geocoding Requests
#' @format Character string.
#' @description All package functions that interact with Google location services will
#' use API key whenever a request is made.
#' @seealso addGoogleAddress
#' @seealso addGoogleElevation
NULL

#' @keywords environment
#' @export
#' @title Get Google API Key
#' @description Returns the current Google API key.
#' @return String.
#' @seealso addGoogleAddress
#' @seealso addGoogleElevation
#' @seealso googleApiKey
#' @seealso setGoogleApiKey
getGoogleApiKey <- function() {
  return(pwfslSmokeEnv$googleApiKey)
}

#' @keywords environment
#' @export
#' @title Set Google API Key
#' @param key Google API key used when interacting with Google location services
#' @description Sets the current Google API key.
#' @return Silently returns previous value of googleApiKey.
#' @seealso addGoogleAddress
#' @seealso addGoogleElevation
#' @seealso getGoogleApiKey
#' @seealso googleApiKey
setGoogleApiKey <- function(key) {
  old <- pwfslSmokeEnv$googleApiKey
  pwfslSmokeEnv$googleApiKey <- key
  return(invisible(old))
}

#' @docType data
#' @keywords environment
#' @name esriToken
#' @title Token used for ESRI Geocoding Requests
#' @format Character string.
#' @description All package functions that interact with ESRI location services will
#' use the token whenever a request is made.
#' @seealso addEsriAddress
NULL

#' @keywords environment
#' @export
#' @title Get ESRI Token
#' @description Returns the current esriToken.
#' @return String.
#' @seealso addEsriAddress
#' @seealso esriToken
#' @seealso setEsriToken
getEsriToken <- function() {
  return(pwfslSmokeEnv$esriToken)
}

#' @keywords environment
#' @export
#' @title Set ESRI Token
#' @param token ESRI token used when interacting with ESRI location services
#' @description Sets the current esriToken.
#' @return Silently returns previous value of esriToken.
#' @seealso addEsriAddress
#' @seealso getEsriToken
#' @seealso esriToken
setEsriToken <- function(token) {
  old <- pwfslSmokeEnv$esriToken
  pwfslSmokeEnv$esriToken <- token
  return(invisible(old))
}

# ----- WRCC related info -----------------------------------------------------

#' WRCC monitor names and unitIDs
#'
#' @docType data
#' @keywords datasets
#' @name WRCC
#' @title WRCC Monitor Names and Unit IDs
#' @format A list of lists
#' @description
#' The WRCC \url{https://wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitor Archive}
#' provides access to a variety of monitors that can be accessed with the \link{wrcc_createMonitorObject}
#' function. Use of this funciton requires a valid unitID. The \code{WRCC} object is
#' a list of lists. The element named \code{unitIDs} is itself a list of three named vectors,
#' each containing the unitIDs and associated names for
#' one of the categories of monitors available at WRCC:
#'
#' \itemize{
#' \item{cache}
#' \item{miscellaneous}
#' \item{usfs_regional}
#' }
#' @note This list of monitor IDs relfects \code{unitIDs} found on the WRCC
#' site on June 12, 2019.
NULL

# ----- AIRSIS related info ---------------------------------------------------

#' AIRSIS monitor types and codes
#'
#' @export
#' @docType data
#' @name AIRSIS
#' @title AIRSIS Unit Types
#' @format A list of lists
#' @description
#' AIRSIS provides access to data by unit type at URLs like:
#'   http://usfs.airsis.com/vision/common/CSVExport.aspx?utid=38&StartDate=2017-11-06&EndDate=2017-11-07
#'
#' The \code{AIRSIS} objectis a list of lists. The element named \code{unitTypes} is itself
#' a list of named unit types:
#'
#' Unit types include:
#' \itemize{
#' \item{DATARAM}{ 21 = Dataram}
#' \item{BAM1020}{ 24 = Bam 1020}
#' \item{EBAM_NEW}{ 30 = eBam-New}
#' \item{EBAM}{ 38 = Iridium - Ebam}
#' \item{ESAM}{ 39 = Iridium - Esam}
#' \item{AUTOMET}{ 43 = Automet}
#' }
#' @note This list of monitor types was created on Feb 09, 2017.
AIRSIS <- list(unitTypes=list(DATARAM=21,
                              BAM1020=24,
                              EBAM_NEW=30,
                              EBAM=38,
                              ESAM=39,
                              AUTOMET=43))


# ----- AQI breaks -------------------------------------------------------------

#' AQI breaks and associated names and colors
#'
#' @export
#' @docType data
#' @name AQI
#' @title Official Air Quality Index Levels, Names and Colors
#' @format A list with named elements
#' @description
#' Official AQI levels, names and colors are provided in a list for easy coloring and labeling.
#' @details
#' The \code{AQI} object contains english language text.
#'
#' AQI breaks and colors are defined at
#' \url{https://docs.airnowapi.org/aq101}
#' @note
#' The low end of each break category is used as the breakpoint.
#' @seealso \code{\link{AQI_en}} \code{\link{AQI_es}}

AQI <- list(
  breaks_24 = c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),
  colors = c(grDevices::rgb(0,228/255,0),
             grDevices::rgb(255/255,255/255,0),
             grDevices::rgb(255/255,126/255,0),
             grDevices::rgb(255/255,0,0),
             grDevices::rgb(143/255,63/255,151/255),
             grDevices::rgb(126/255,0,35/255)),
  mv4Colors = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#9b59b6", "#8c3a3a"),
  names = c('Good','Moderate','USG','Unhealthy','Very Unhealthy','Hazardous'),
  actions = c(
    'None.',
    'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
    'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
    'People within Sensitive Groups should avoid all physical outdoor activity.',
    'Everyone should avoid prolonged or heavy exertion.',
    'Everyone should avoid any outdoor activity.'
  )
)

#' AQI breaks and associated names and colors (english language)
#'
#' @export
#' @docType data
#' @name AQI_en
#' @title Official Air Quality Index Levels, Names and Colors
#' @format A list with named elements
#' @description
#' Official AQI levels, names and colors are provided in a list for easy coloring and labeling.
#' @details
#' The \code{AQI_es} object contains english language text. It is equalivalent to the
#' \code{AQI} object and provided for consistency with other language versions.
#'
#' AQI breaks and colors are defined at
#' \url{https://docs.airnowapi.org/aq101}
#' @note
#' The low end of each break category is used as the breakpoint.
#' @seealso \code{\link{AQI}} \code{\link{AQI_es}}

AQI_en <- list(
  breaks_24 = c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),
  colors = c(grDevices::rgb(0,228/255,0),
             grDevices::rgb(255/255,255/255,0),
             grDevices::rgb(255/255,126/255,0),
             grDevices::rgb(255/255,0,0),
             grDevices::rgb(143/255,63/255,151/255),
             grDevices::rgb(126/255,0,35/255)),
  mv4Colors = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#9b59b6", "#8c3a3a"),
  names = c('Good','Moderate','USG','Unhealthy','Very Unhealthy','Hazardous'),
  actions = c(
    'None.',
    'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
    'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
    'People within Sensitive Groups should avoid all physical outdoor activity.',
    'Everyone should avoid prolonged or heavy exertion.',
    'Everyone should avoid any outdoor activity.'
  )
)

#' AQI breaks and associated names and colors (Spanish language)
#'
#' @export
#' @docType data
#' @name AQI_es
#' @title Official Air Quality Index Levels, Names and Colors
#' @format A list with named elements
#' @description
#' Official AQI levels, names and colors are provided in a list for easy coloring and labeling.
#' @details
#' The \code{AQI_es} object contains spanish language text.
#'
#' AQI breaks and colors are defined at
#' \url{https://docs.airnowapi.org/aq101}
#' @note
#' The low end of each break category is used as the breakpoint.
#' @seealso \code{\link{AQI_en}} \code{\link{AQI}}

AQI_es <- list(
  breaks_24 = c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),
  colors = c(grDevices::rgb(0,228/255,0),
             grDevices::rgb(255/255,255/255,0),
             grDevices::rgb(255/255,126/255,0),
             grDevices::rgb(255/255,0,0),
             grDevices::rgb(143/255,63/255,151/255),
             grDevices::rgb(126/255,0,35/255)),
  mv4Colors = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#9b59b6", "#8c3a3a"),
  names = c('Buena','Moderada','IGS','Insalubre','Muy insalubre','Peligrosa'),
  # NOTE:  R packages require that unicode characters be escaped.
  actions = c(
    'Ninguna.',
    'Personas inusualmente sensitivas deber\\u00edan considerar limitar la labor prolongada \\u00f3 intensa.',
    'Personas dentro de los grupos sensitivos deben reducir la labor prolongada \\u00f3 intensa al aire libre.',
    'Personas dentro de los grupos sensitivos deben evitar toda actividad f\\u00edsica al aire libre.',
    'Todos deben evitar la labor prolongada \\u00f3 intensa.',
    'Todos deben evitar cualquier actividad al aire libre.'
  )
)

# ----- State codes -----------------------------------------------------------

#' CONUS state codes
#'
#' @export
#' @docType data
#' @name CONUS
#' @title CONUS State Codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US

CONUS <- c(     "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
                "ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC"     )

#' US state codes
#'
#' @export
#' @docType data
#' @name US_52
#' @title US State Codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico)

US_52 <- c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
           "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC","PR")

