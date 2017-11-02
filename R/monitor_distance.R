#' @keywords ws_monitor
#' @export
#' @title Calculate Distances From ws_monitor Monitors to Location of Interest
#' @param ws_monitor \emph{ws_monitor} object
#' @param longitude longitude of the location of interest
#' @param latitude latitude of the location of interest
#' @description This function returns the distances (km) between monitoring sites and
#' a location of interest. These distances can be used to create a mask identifying
#' monitors within a certain radius of the location of interest.
#' @return vector of of distances (km)
#' @seealso distance
#' @examples 
#' N_M <- Northwest_Megafires
#' # Walla Walla
#' WW_lon <- -118.330278
#' WW_lat <- 46.065
#' distance <- monitor_distance(N_M, WW_lon, WW_lat)
#' closestIndex <- which(distance == min(distance))
#' distance[closestIndex]
#' N_M$meta[closestIndex,]

monitor_distance <- function(ws_monitor, longitude, latitude) { 
  distance <- distance(longitude, latitude, ws_monitor$meta$longitude, ws_monitor$meta$latitude)  
  names(distance) <- ws_monitor$meta$monitorID
  return(distance)
}

