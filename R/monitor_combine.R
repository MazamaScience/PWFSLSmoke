#' @keywords ws_monitor
#' @export
#' @title Combine ws_monitor Objects
#' @param ws_monitor1 data list of class \code{ws_monitor}
#' @param ws_monitor2 a \code{ws_monitor} object to be merged with ws_monitor1
#' @return A ws_monitor object where the ws_monitor1 and ws_monitor2 are combined
#' @description When given two ws_monitor objects, the function generates a single ws_monitor object 
#' by merging both of their meta and data
#' @examples
#' \dontrun{
#' setSmokeDataDir('~/Data/Smoke/')
#' # Smokey days in the Okanagan
#' airnow <- airnow_load(startdate=2015081900, enddate=2015082200)
#' Omak <- monitor_subset(airnow, monitorIDs='530470013')
#' bs <- bluesky_load(modelRun=2015081900)
#' bs_near_Omak <- grid_subsetByDistance(bs, lon=Omak$meta$longitude, lat=Omak$meta$latitude, radius=5)
#' bs_Omak <- monitor_collapse(bs_near_Omak, monitorID='bs_Omak')
#' Omak_both <- monitor_combine(Omak, bs_Omak)
#' monitor_dygraph(Omak_both)
#' } 

monitor_combine <- function(ws_monitor1, ws_monitor2) {
  
  # Merge data and meta dataframes separately
  newData <- merge(ws_monitor1$data, ws_monitor2$data, all=TRUE)
  
  newMeta <- merge(ws_monitor1$meta, ws_monitor2$meta, all=TRUE)
  rownames(newMeta) <- newMeta$monitorID
  
  # Create a new ws_monitor object
  ws_monitor <- list(meta=newMeta, data=newData)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  return(ws_monitor)
  
}
