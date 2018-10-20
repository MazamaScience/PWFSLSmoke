#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load PM2.5 monitoring data
#' @param startdate Desired start date (integer or character in ymd[hms] format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in ymd[hms] format
#'        or \code{POSIXct}).
#' @param monitorIDs Optional vector of monitorIDs.
#' @param latestDataDir Local Directory where 'latest' data files are found.
#' @param dailyDataDir Local Directory where 'daily' data files are found.
#' @param annualDataDir Local Directory where annual data files are found.
#' @description Loads monitoring data for a given time range. Data from AirNow,
#' AIRSIS and WRCC are combined into a single \emph{ws_monitor} object.
#' 
#' Archival datasets are joined with 'daily' and 'latest' datasets as needed to
#' satisfy the requested date range. 
#' @note Joining datasets is a computationally expensive task when many monitors
#' are involved. It is highly recommend that \code{monitorIDs} be specified when
#' loading recent data with this function.
#' @seealso \code{\link{loadDaily}}
#' @seealso \code{\link{loadLatest}}
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @examples
#' \dontrun{
#' ca <- loadData(20170601,20171001) %>% monitor_subset(stateCodes='CA')
#' }

loadData <- function(startdate = NULL,
                     enddate = NULL,
                     monitorIDs = NULL,
                     latestDataDir = NULL,
                     dailyDataDir = NULL,
                     annualDataDir = NULL) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( is.null(startdate) ) {
    stop(paste0("Required parameter 'startdate' is missing"))
  }
  
  if ( is.null(enddate) ) {
    stop(paste0("Required parameter 'enddate' is missing"))
  }
  
  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate)
  
  if ( starttime > endtime ) {
    temptime <- endtime
    endtime <- starttime
    starttime <- temptime
    rm(temptime)
  }
  
  if ( lubridate::year(starttime) != lubridate::year(endtime) ) {
    stop("Requests covering multiple years are not supported", call.=FALSE)
  }
  
  now <- lubridate::now("UTC")
  now_m1 <- now - lubridate::ddays(1)
  now_m10 <- now - lubridate::ddays(10)
  now_m45 <- now - lubridate::ddays(45)
  
  # Check for very long monitor_join times
  if ( starttime < now_m45 && endtime >= now_m45 && is.null(monitorIDs) ) {
    stop("Requests joining anuual files with 'daily' or 'latest' files can be ",
         "time consuming. Please supply monitorIDs to subset the data.")
  }
  
  # Load annual data -----------------------------------------------------------
  
  if ( starttime < now_m45 ) {
    
    # NOTE:  https://haze.airfire.org/monitoring/<source>/RData/
    #
    # NOTE:  Each archive has a specific start year
    
    epaLastCompleteYear <- 2016
    monitorList <- list()
    year <- lubridate::year(starttime)
    
    if ( year > epaLastCompleteYear ) {
      result <- try({
        if ( !is.null(annualDataDir) ) {
          file <- paste0("airnow_PM2.5_",year,".RData")
          filepath <- file.path(annualDataDir, file)
          monitorList$airnow <- load(filepath)
        } else {
          monitorList$airnow <- airnow_load(year)
        }
      }, silent=FALSE)
    }
    if ( year >= 2004 ) {
      result <- try({
        if ( !is.null(annualDataDir) ) {
          file <- paste0("airsis_PM2.5_",year,".RData")
          filepath <- file.path(annualDataDir, file)
          monitorList$airnow <- load(filepath)
        } else {
          monitorList$airsis <- airsis_load(year)
        }
      }, silent=FALSE)
    }
    if ( year >= 1998 && year <= epaLastCompleteYear) {
      result <- try({
        if ( ~is.null(annualDataDir) ) {
          file <- paste0("epa_PM2.5_88101",year,".RData")
          filepath <- file.path(annualDataDir, file)
          epa_88101 <- load(filepath)
          file <- paste0("epa_PM2.5_88502",year,".RData")
          filepath <- file.path(annualDataDir, file)
          epa_88502 <- load(filepath)
        } else {
          epa_88101 <- epa_load(year, "88101")
          epa_88502 <- epa_load(year, "88502")
        }
        suppressWarnings({
          monitorList$epa <- monitor_combine(list(epa_88101, epa_88502))
        })
      }, silent=FALSE)
    }
    if ( year >= 2010 ) {
      result <- try({
        if ( !is.null(annualDataDir) ) {
          file <- paste0("wrcc_PM2.5_",year,".RData")
          filepath <- file.path(annualDataDir, file)
          monitorList$airnow <- load(filepath)
        } else {
          monitorList$wrcc <- wrcc_load(year)
        }
      }, silent=FALSE)
    }
    
    annualData <- monitor_combine(monitorList) %>%
      monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)
    
  }
  
  # Load daily data ------------------------------------------------------------
  
  if ( endtime >= now_m45 && starttime < now_m10 ) {
    
    # Load latest45 from all sources
    result <- try({
      if ( !is.null(latestDataDir) ) {
        load(file.path(dailyDataDir, "airnow_PM2.5_latest45.RData"))
        load(file.path(dailyDataDir, "airsis_PM2.5_latest45.RData"))
        load(file.path(dailyDataDir, "wrcc_PM2.5_latest45.RData"))
      } else {
        airnow_PM2.5_latest45 <- airnow_loadLatest()
        airsis_PM2.5_latest45 <- airsis_loadLatest()
        wrcc_PM2.5_latest45 <- wrcc_loadLatest()
      }
      dailyData <- monitor_combine(list(airnow_PM2.5_latest45,
                                        airsis_PM2.5_latest45,
                                        wrcc_PM2.5_latest45)) %>%
        monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)
    }, silent=TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # logger.trace("Error loading daily monitoring data.")
      # err_msg <- geterrmessage()
      # logger.error(err_msg)
      stop("Error loading daily monitoring data.", call. = FALSE)
    }
    
  }
  
  # Load latest data -----------------------------------------------------------
  
  if ( endtime >= now_m1 ) {
    
    # Load latest10 from all sources
    result <- try({
      if ( !is.null(latestDataDir) ) {
        load(file.path(latestDataDir, "airnow_PM2.5_latest10.RData"))
        load(file.path(latestDataDir, "airsis_PM2.5_latest10.RData"))
        load(file.path(latestDataDir, "wrcc_PM2.5_latest10.RData"))
      } else {
        airnow_PM2.5_latest10 <- airnow_loadLatest()
        airsis_PM2.5_latest10 <- airsis_loadLatest()
        wrcc_PM2.5_latest10 <- wrcc_loadLatest()
      }
      latestData <- monitor_combine(list(airnow_PM2.5_latest10,
                                         airsis_PM2.5_latest10,
                                         wrcc_PM2.5_latest10)) %>%
        monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)
    }, silent=TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # logger.trace("Error loading latest monitoring data.")
      # err_msg <- geterrmessage()
      # logger.error(err_msg)
      stop("Error loading latest monitoring data.", call. = FALSE)
    }
    
  }
  
  # Join ws_monitor objects ----------------------------------------------------
  
  if ( exists("annualData") ) {
    ws_monitor <- annualData
  }
  
  if ( exists("dailyData") ) {
    if ( exists("ws_monitor") ) {
      ws_monitor <- monitor_join(ws_monitor, dailyData, monitorIDs)
    } else {
      ws_monitor <- dailyData
    }
  }
  
  if ( exists("latestData") ) {
    if ( exists("ws_monitor") ) {
      ws_monitor <- monitor_join(ws_monitor, latestData, monitorIDs)
    } else {
      ws_monitor <- latestData
    }
  }

  # Subset to the requested time range
  ws_monitor <- monitor_subset(ws_monitor, tlim=c(starttime, endtime))
  
  return(ws_monitor)
  
}
