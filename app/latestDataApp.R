
library(methods)       # always included for Rscripts
library(jug)           # web framework

library(PWFSLSmoke)


jug() %>%
  
  # AirNow
  get("/airnow", function(req, res, err){
    
    # get parameters
    state <- req$params$state
    
    # get state subset
    monitors <- airnow_loadLatest()
    
    if ( nrow(monitors$meta) == 0 ) {
      stop('No data available for AirNow.', call.=FALSE)
    }
    
    if ( is.null(state) ) {
      stateMonitors <- monitor_subset(monitors)
    } else {
      stateMonitors <- monitor_subset(monitors, stateCodes=state)
    }
    
    # calculate max values in the last 24 hours
    meta <- stateMonitors$meta
    data <- stateMonitors$data[,-1]
    rowCount <- nrow(data)
    data <- data[(rowCount-24):rowCount,]
    meta$maxPM25 <- apply(data, 2, max, na.rm=TRUE)
    
    meta <- meta[,c('monitorID','stateCode','siteName','maxPM25','longitude','latitude')]
    result <- readr::format_csv(meta)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # AIRSIS
  get("/airsis", function(req, res, err){
    
    # get parameters
    state <- req$params$state
    
    # get state subset
    monitors <- airsis_loadLatest()

    if ( nrow(monitors$meta) == 0 ) {
      stop('No data available for AIRSIS', call.=FALSE)
    }
    
    if ( is.null(state) ) {
      stateMonitors <- monitor_subset(monitors)
    } else {
      stateMonitors <- monitor_subset(monitors, stateCodes=state)
    }
    
    # calculate max values in the last 24 hours
    meta <- stateMonitors$meta
    data <- stateMonitors$data[,-1]
    rowCount <- nrow(data)
    data <- data[(rowCount-24):rowCount,]
    meta$maxPM25 <- apply(data, 2, max, na.rm=TRUE)
    
    meta <- meta[,c('monitorID','stateCode','siteName','maxPM25','longitude','latitude')]
    result <- readr::format_csv(meta)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # WRCC
  get("/wrcc", function(req, res, err){
    
    # get parameters
    state <- req$params$state
    
    # get state subset
    monitors <- wrcc_loadLatest()
    
    if ( nrow(monitors$meta) == 0 ) {
      stop('No data available for WRCC.', call.=FALSE)
    }
    
    if ( is.null(state) ) {
      stateMonitors <- monitor_subset(monitors)
    } else {
      stateMonitors <- monitor_subset(monitors, stateCodes=state)
    }
    
    # calculate max values in the last 24 hours
    meta <- stateMonitors$meta
    data <- stateMonitors$data[,-1]
    rowCount <- nrow(data)
    data <- data[(rowCount-24):rowCount,]
    meta$maxPM25 <- apply(data, 2, max, na.rm=TRUE)
    
    meta <- meta[,c('monitorID','stateCode','siteName','maxPM25','longitude','latitude')]
    result <- readr::format_csv(meta)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # Handle errors
  simple_error_handler_json() %>%

  # Serve up the results
  ###serve_it()
  serve_it(host = Sys.getenv("JUG_HOST"), port = as.integer(Sys.getenv("JUG_PORT")))

# ----- END -------------------------------------------------------------------

