#' @keywords airsis
#' @export
#' @title Load Recent AIRSIS Monitoring Data
#' @param baseUrl The location of the meta and data files
#' @return ws_monitor object with subsetted time, monitorIDs and parameter
#' @description The most recent 10 days of AIRSIS data are updated in quasi-real time
#' at PWFSL and can be loaded with this function.
#' @examples
#' \dontrun{
#' airsis <- airsis_loadLatest()
#' }

airsis_loadLatest <- function(baseUrl='http://tools.airfire.org/monitoring/v3/data') {
  
  # Get the most recent data and metadata
  data <- get(load(url(paste0(baseUrl,'/EBAM_PM2.5_Latest.RData'))))
  meta <- get(load(url(paste0(baseUrl,'/EBAM_SitesMetadata.RData'))))
  
  # Only include metadata for sites found in data
  meta <- monitor_subsetMeta(meta, monitorIDs=colnames(data)[-1])

  # Add missing metadata to 'old-style' Latest data
  # The columns are already present but may contain missing data
  if ( any(is.na(meta$timezone)) || any(is.na(meta$countryCode)) || any(is.na(meta$stateCode)) ) {
    meta <- addMazamaMetadata(meta)
  }
  if ( any(is.na(meta$elevation)) || any(is.na(meta$siteName)) ) { # OK if countyName is NA
    meta <- addGoogleMetadata(meta)
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
 
  # Run through subsetting to remove sites with all missing data
  ws_monitor <- monitor_subset(ws_monitor, dropMonitors=TRUE)
  
  return(ws_monitor)
}
