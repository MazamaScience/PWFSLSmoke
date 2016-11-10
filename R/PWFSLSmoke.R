# ----- AQI breaks ----------------------------------------------------------------

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

AQI <- list(breaks_24=c(0, 12, 35.5, 55.5, 150.5, 250.5, 10000),
            colors=c(grDevices::rgb(0,228/255,0),
                     grDevices::rgb(255/255,255/255,0),
                     grDevices::rgb(255/255,126/255,0),
                     grDevices::rgb(255/255,0,0),
                     grDevices::rgb(143/255,63/255,151/255),
                     grDevices::rgb(126/255,0,35/255)),
            names=c('good','moderate','USG','unhealthy','very unhealthy','extreme'))

# ----- State codes in the lower 48 ---------------------------------------------------

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

# ----- Internal Package State --------------------------------------------------------

PWFSLSmokeEnv <- new.env(parent = emptyenv())
PWFSLSmokeEnv$dataDir <- NULL

# ----- Data Directory Configuration --------------------------------------------------

#' @docType data
#' @keywords environment
#' @name SmokeDataDir
#' @title Directory for Smoke Data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which users can set
#' using \code{setSmokeDataDir()}. All package functions use this directory whenever datasets
#' are created or loaded.
#' 
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getSmokeDataDir}
#' @seealso \link{setSmokeDataDir}
NULL

#' @keywords environment
#' @export
#' @title Get Package Data Directory
#' @description Returns the package data directory where smoke data is located.
#' @return Absolute path string.
#' @seealso \link{SmokeDataDir}
#' @seealso \link{setSmokeDataDir}

getSmokeDataDir <- function() {
  if (is.null(PWFSLSmokeEnv$dataDir)) {
    stop('No data directory found. Please set a data directory with setSmokeDataDir("~/Data/Smoke").',call.=FALSE)
  } else {
    return(PWFSLSmokeEnv$dataDir)
  }
}

#' @keywords environment
#' @export
#' @title Set Package Data Directory
#' @param dataDir directory where smoke datasets are created
#' @description Sets the package data directory where spatial data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso \link{SmokeDataDir}
#' @seealso \link{getSmokeDataDir}

setSmokeDataDir <- function(dataDir) {
  old <- PWFSLSmokeEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    PWFSLSmokeEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setSmokeDataDir(",dataDir,")."))
  })
  return(invisible(old))
}


