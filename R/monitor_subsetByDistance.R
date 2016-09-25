#' @keywords monitor
#' @export
#' @title Subset Monitor Data by Distance from Location
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param lon target longitude from which the radius will be calculated
#' @param lat target latitude from which the radius will be calculated
#' @param radius distance (km) of radius from target location -- default=300
#' @param count number of grid cells to return
#' @description This function subsets a monitoring dataList to include only those
#' monitors within a certain radius of a target location.
#' If no monitors fall within the specified radius, \code{ws_monitor$data} and 
#' \code{ws_monitor$meta} are set to \code{NULL}.
#' 
#' When \code{count} is used, a \code{ws_monitor} object is created containing \strong{up to}
#' \code{count} monitors ordered by increasing distance from the target location. Only
#' grid cells are found within \code{radius} will be returned even if this number is 
#' fewer than \code{count}.
#' @return \code{ws_monitor} data list containing monitors near a location
#' @seealso monitorDistance
#' @examples
#' \dontrun{
#' setSmokeDataDir('~/Data/Smoke')
#' airnow <- airnow_load(20140913, 20141010)
#' King_Fire <- monitor_subsetByDistance(airnow, lon=-120.604, lat=38.782, radius=50)
#' monitor_leaflet(King_Fire)
#' } 

monitor_subsetByDistance <- function(ws_monitor, lon=NULL, lat=NULL, radius=50, count=NULL) {
  
  # Create distance vector and mask
  # distanceMask <- distances <= radius
  distanceVector <- monitor_distance(ws_monitor, lon, lat) 
  distanceMask <- distanceVector <= radius
  
  if ( !(any(distanceMask)) ) {
    meta <- NULL
    data <- NULL
  } else {
    meta <- ws_monitor$meta[distanceMask,]
    data <- monitor_subsetData(ws_monitor$data, monitorIDs=meta$monitorID, dropMonitors=TRUE)
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Update the distanceVector for the new subset of monitors
  distanceVector <- monitor_distance(ws_monitor, lon, lat) 
  
  # If user specified the count, return only the sites closest to the target location.
  if ( !is.null(count) ) {
    
    # NOTE:  When using count, return monitors in distance order and make sure
    # NOTE:  that the distances are also subset and returned in distance order.
    
    count <- as.integer(count)
    withinRadiusCount <- nrow(ws_monitor$meta)
    
    # Sanity check 
    if ( count > withinRadiusCount ) {
      futile.logger::flog.info("count=%s cells requested but only %s within radius=%s.  Returning %s.", count, withinRadiusCount, radius, withinRadiusCount)
      count <- withinRadiusCount
    }
    
    # Find the 'count' closest monitors
    closestIndices <- order(distanceVector)[1:count]
    
    # Subset distances and monitors
    distanceVector <- distanceVector[closestIndices]
    ws_monitor <- monitor_subset(ws_monitor, monitorIDs=ws_monitor$meta$monitorID[closestIndices])
    
  }
  
  # Add 'distance' to the ws_monitor object
  ws_monitor[['distance']] <- distanceVector
  
  return(ws_monitor)
  
}

