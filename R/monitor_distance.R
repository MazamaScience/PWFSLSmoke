#' @keywords monitor
#' @export
#' @title Calculate Distances from Target Location
#' @param ws_monitor object
#' @param lon longitude of the location of interest
#' @param lat latitude of the location of interest
#' @description This function returns the distances (km) between monitoring sites and
#' the location of interest. These distances can be used to create a mask identifying
#' monitors within a certain radius of the target location.
#' @return vector of of distances (km)
#' @seealso distance
#' @examples 
#' \dontrun{
#' airnow <- airnow_load(2014091300, 2014101023)
#' distance <- monitor_distance(airnow, -120.604, 38.782)
#' closestIndex <- which(distance == min(distance))
#' distance[closestIndex]
#' airnow$meta[closestIndex,]
#' }

monitor_distance <- function(ws_monitor, lon, lat) { 
  distance <- distance(lon, lat, ws_monitor$meta$longitude, ws_monitor$meta$latitude)  
  names(distance) <- ws_monitor$meta$monitorID
  return(distance)
}

