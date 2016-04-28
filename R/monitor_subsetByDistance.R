#' @keywords monitor
#' @export
#' @title Subset Monitor Data by Distance from Location
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param lon target longitude from which the radius will be calculated
#' @param lat target latitude from which the radius will be calculated
#' @param radius distance (km) of radius from target location -- default=300
#' @description This function subsets a monitoring dataList to include only those
#' monitors within a certain radius of a target location.
#' If no monitors fall within the specified radius, \code{ws_monitor$data} and 
#' \code{ws_monitor$meta} are set to \code{NULL}.
#' @return \code{ws_monitor} data list containing monitors near a location
#' @seealso monitorDistance
#' @examples
#' \dontrun{
#' setSmokeDataDir('~/Data/Smoke')
#' airnow <- airnow_load(20140913, 20141010)
#' King_Fire <- monitor_subsetByDistance(airnow, lon=-120.604, lat=38.782, radius=50)
#' monitor_leaflet(King_Fire)
#' } 

monitor_subsetByDistance <- function(ws_monitor, lon=NULL, lat=NULL, radius=300) {
  
  # sanity check
  #if (is.null(distances)) {
    #stop('No distances supplied')   
  #}
  
  # Create distance mask
  # distanceMask <- distances <= radius
  distances <- monitor_distance(ws_monitor, lon, lat) 
  distanceMask <- distances <= radius
  
  if (any(distanceMask)) {
    # Apply distaneMask to 'meta' and add a new column
    metaSub <- ws_monitor$meta[distanceMask,]
    metaSub[['distance']] <- distances[distanceMask]
    
    # Subset 'data' to match
    dataSub <- monitor_subsetData(ws_monitor$data, monitorIDs=metaSub$monitorID, dropMonitors=TRUE)
    
    dataList <- list(data=dataSub, meta=metaSub)
    
  } else {
    
    dataList <- list(data=NULL, meta=NULL)
    
  }
  
  # Return the unraveled list
  return(structure(dataList, class = c("ws_monitor", "list")))
  
}

