#' @keywords ws_monitor
#' @export
#' @title Subset ws_monitor Object by Distance from Target Location
#' @param ws_monitor \emph{ws_monitor} object
#' @param longitude target longitude from which the radius will be calculated
#' @param latitude target latitude from which the radius will be calculated
#' @param radius distance (km) of radius from target location -- default=300
#' @param count number of grid cells to return
#' @return A \emph{ws_monitor} object with monitors near a location.
#' @description Subsets \code{ws_monitor} to include only those monitors (or grid cells) 
#' within a certain radius of a target location. If no monitors (or grid cells) fall 
#' within the specified \code{radius}, \code{ws_monitor$data} and \code{ws_monitor$meta} 
#' are set to \code{NULL}.
#' @description When \code{count} is used, a \emph{ws_monitor} object is created containing \strong{up to}
#' \code{count} monitors, ordered by increasing distance from the target location. Thus, note that the number 
#' of monitors (or grid cells) returned may be less than the specified \code{count} value if fewer than 
#' \code{count} monitors (or grid cells) are found within the specified \code{radius} of the target location.
#' @seealso monitorDistance
#' @examples
#' \dontrun{
#' # Napa Fires -- October, 2017
#' ca <- airnow_load(2017) %>%
#'   monitor_subset(tlim=c(20171001,20171101), stateCodes='CA')
#' Vallejo <- monitor_subset(ca, monitorIDs='060950004_01')
#' Napa_Fires <- monitor_subsetByDistance(ca,
#'                                        longitude = Vallejo$meta$longitude,
#'                                        latitude = Vallejo$meta$latitude,
#'                                        radius = 50)
#' monitorLeaflet(Napa_Fires)
#' } 

monitor_subsetByDistance <- function(ws_monitor,
                                     longitude=NULL,
                                     latitude=NULL,
                                     radius=50,
                                     count=NULL) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")
  
  # Create distance vector and mask
  # distanceMask <- distances <= radius
  distanceVector <- monitor_distance(ws_monitor, longitude, latitude) 
  distanceMask <- distanceVector <= radius
  
  if ( !(any(distanceMask)) ) {
    meta <- createEmptyMetaDataframe(rows=0)
    data <- as.data.frame(ws_monitor$data$datetime)
    names(data) <- 'datetime'
  } else {
    meta <- ws_monitor$meta[distanceMask,]
    data <- monitor_subsetData(ws_monitor$data, monitorIDs=meta$monitorID, dropMonitors=TRUE)
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Update the distanceVector for the new subset of monitors
  distanceVector <- monitor_distance(ws_monitor, longitude, latitude) 
  
  # If user specified the count, return only the sites closest to the target location.
  if ( !is.null(count) ) {
    
    # NOTE:  When using count, return monitors in distance order and make sure
    # NOTE:  that the distances are also subset and returned in distance order.
    
    count <- as.integer(count)
    withinRadiusCount <- nrow(ws_monitor$meta)
    
    # Sanity check 
    if ( count > withinRadiusCount ) {
      message(paste0("count=", count, " cells requested but only", 
                     withinRadiusCount, "within radius=", radius,
                     ".  Returning", withinRadiusCount, "."))
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
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}

