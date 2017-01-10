#' @keywords ws_monitor
#' @export
#' @title Subset ws_monitor Object by Distance from Target Location
#' @param ws_monitor ws_monitor object
#' @param lon target longitude from which the radius will be calculated
#' @param lat target latitude from which the radius will be calculated
#' @param radius distance (km) of radius from target location -- default=300
#' @param count number of grid cells to return
#' @description Subsets a ws_monitor object to include only those monitors (or grid cells) 
#' within a certain radius of a target location. If no monitors (or grid cells) fall 
#' within the specified \code{radius}, \code{ws_monitor$data} and \code{ws_monitor$meta} 
#' are set to \code{NULL}.
#' @description When \code{count} is used, a \code{ws_monitor} object is created containing \strong{up to}
#' \code{count} monitors, ordered by increasing distance from the target location. Thus, note that the number 
#' of monitors (or grid cells) returned may be less than the specified \code{count} value if fewer than 
#' \code{count} monitors (or grid cells) are found within the specified \code{radius} of the target location.
#' @return \code{ws_monitor} object containing monitors near a location
#' @seealso monitorDistance
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20140913, 20141010)
#' KingFire <- monitor_subsetByDistance(airnow, lon=-120.604, lat=38.782, radius=50)
#' monitor_leaflet(KingFire)
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

