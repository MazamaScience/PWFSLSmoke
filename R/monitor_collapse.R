#' @keywords ws_monitor
# @export
#' @title Collapse a ws_monitor Object into a ws_monitor Object with a Single Monitor
#' @param ws_monitor ws_monitor object
#' @param lon The custom longitude of the collapsed monitoring station. (Default = mean of the longitudes)
#' @param lat The custom latitude of the collapsed monitoring station. (Default = mean of the latitudes)
#' @param monitorID The custom monitorID of the collapsed monitoring station. (Default = 'generated_id')
#' @param FUN The function to be applied to all the monitors at a single time index. (Default = mean)
#' @param ... Any additional argument to be passed on to the apply() function
#' @return A ws_monitor object with meta and data that corresponds to the collapsed single monitor
#' @description Collapses all the monitors from a given ws_monitor object into a single-monitor ws_monitor object using
#' the function provided in the FUN argument. The collapse applies to both ws_monitor$meta and ws_monitor$data. Intended 
#' for use with gridded model data, rather than actual monitor data itself (meta data is largely chopped; NAs propagate).
#' @examples
#' \dontrun{
#' setSmokeDataDir('~/Data/Smoke/')
#' # Smokey days in the Okanagan
#' airnow <- airnow_load(startdate=2015080100, enddate=2015083100)
#' Omak <- monitor_subset(airnow, monitorIDs='530470013')
#' bs <- bluesky_load(modelRun=2015081900)
#' bs_near_Omak <- grid_subsetByDistance(bs, lon=Omak$meta$longitude, lat=Omak$meta$latitude, radius=5)
#' bs_Omak <- monitor_collapse(bs_near_Omak, monitorID='bs_Omak')
#' } 

monitor_collapse <- function(ws_monitor, lon=NULL, lat=NULL, monitorID='generated_id', FUN=mean, ...) {
  
  data <- ws_monitor$data[,-1]
  
  collapsed_data <- apply(data, MARGIN = 1, FUN = FUN, ...)
 
  newData <- data.frame(ws_monitor$data$datetime, collapsed_data)
  colnames(newData) <- c('datetime', monitorID)
  
  meta <- ws_monitor$meta
  
  if (!is.null(lon) & !is.null(lat)) {
        
    newLat <- lat
    newLon <- lon
  
  } else {
          
    newLat <- mean(meta$latitude)
    newLon <- mean(meta$longitude)
  
  }
  
  timezone <-  MazamaSpatialUtils::getTimezone(newLon, newLat, useBuffering=TRUE)
  
  newMeta <- data.frame(monitorID=colnames(newData[2]),
                        longitude=newLon,
                        latitude=newLat,
                        timezone=timezone,
                        stringsAsFactors=FALSE)
      
  rownames(newMeta) <- newMeta$monitorID
  
  ws_monitor <- list(meta=newMeta, data=newData)
  
  return(structure(ws_monitor, class = c("ws_monitor", "list")))
  
}
  
