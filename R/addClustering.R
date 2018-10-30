#' @keywords internal
#' @export
#' @import MazamaCoreUtils
#' @title Add Clustering Information to a Dataframe
#' @param tbl tibble with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see description)
#' @param lonVar name of longitude variable in the incoming tibble
#' @param latVar name of the latitude variable in the incoming tibble
#' @param maxClusters maximum number of clusters to try
#' @param flagAndKeep flag, rather then remove, bad data during clustering
#' @description Clustering is used to assign individual measurements to deployment locations.
#'
#' The value of \code{clusterRadius} is compared with the output of \code{cluster::pam(...)$clusinfo[,'av_diss']}
#' to determine the number of clusters.
#'
#' @return Input tibble with additional columns: \code{deploymentID, medoidLon, mediodLat}.
#' @references \href{http://mazamascience.com/WorkingWithData/?p=1694}{When k-means Clustering Fails}

addClustering <- function(tbl, clusterDiameter=1000,
                          lonVar="longitude", latVar="latitude",
                          maxClusters=50, flagAndKeep=FALSE) {

  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    logger.error("Unable to perform clustering: tibble empty")
    stop(paste0("Unable to perform clustering: tibble empty"))
  }

  # Sanity check -- names
  if ( !lonVar %in% names(tbl) ) {
    logger.error("No lonVar='%s' column found in 'tbl' tibble with columns: %s", lonVar, paste0(names(tbl), collapse=", "))
    stop(paste0("Longitudes could not be found.  Did you specify the lonVar argument?"))
  }
  if ( !latVar %in% names(tbl) ) {
    logger.error("No latVar='%s' column found in 'tbl' tibble with columns: %s", latVar, paste0(names(tbl), collapse=", "))
    stop(paste0("Latitudes could not be found.  Did you specify the latVar argument?"))
  }

  # If we only have a single row, return immediately
  if ( nrow(tbl) == 1 ) {
    tbl$medoidLon <- tbl[[lonVar]][1]
    tbl$medoidLat <- tbl[[latVar]][1]
    lonString <- format( round(tbl$medoidLon,3), nsmall=3 ) # 3 decimal places
    latString <- format( round(tbl$medoidLat,3), nsmall=3 ) # 3 decimal places
    locationString <- paste0( 'lon_', lonString, '_lat_', latString )
    tbl$deploymentID <- make.names(locationString)
    return(tbl)
  }

  # temporarily remove rows with bad locations if flagAndKeep = TRUE
  if ( flagAndKeep ) {
    tbl$rowID <- as.integer(rownames(tbl))
    badLocationMask <- is.na(tbl[lonVar]) | is.na(tbl[latVar])
    badLocationRows <- tbl[badLocationMask,]
    tbl <- tbl[!badLocationMask,]
  }

  # NOTE:  A monitor wil be moved around from time to time, sometimes across the country
  # NOTE:  and sometimes across the street.  We need to assign unique identifiers to each
  # NOTE:  new "deployment" but not when the monitor is moved a short distance.
  # NOTE:
  # NOTE:  We use clustering to find an appropriate number of unique "deployments".
  # NOTE:  The sensitivity of this algorithm can be adjused with the clusterDiameter argument.
  # NOTE:
  # NOTE:  Standard kmeans clustering does not work well when clusters can have widely
  # NOTE:  differing numbers of members. A much better result is acheived with
  # NOTE:  the Partitioning Around Mediods method available in cluster::pam.
  # NOTE:
  # NOTE:  Try the following example:
  # NOTE:    x <- y <- c(rep(0,3),rep(1,3),rep(10,20),rep(11,20),rep(100,50),rep(101,50))
  # NOTE:    m <- cbind(x,y)
  # NOTE:    layout(matrix(seq(2)))
  # NOTE:    plot(x, y, pch=as.character( stats::kmeans(m,3)$cluster ))
  # NOTE:    plot(x, y, pch=as.character( cluster::pam(m,3)$cluster ))
  # NOTE:    plot(x, y, pch=as.character( cluster::clara(m,3)$cluster ))
  # NOTE:
  # NOTE:  Run the plots a few times and you will see that kmeans clustering sometimes
  # NOTE:  gets it wrong.

  logger.debug("Testing up to %s clusters", maxClusters)

  # NOTE:  We need to use cluster::clara when we get above ~2K points.
  # NOTE:  For this reason we need to use clusinfo[,'max_diss'] instead
  # NOTE:  of clusinfo[,'diameter'] as the latter is only provided by
  # NOTE:  cluster::pam and not cluster::clara.
  # NOTE:  (Is there really any difference between 'max_diss' and 'distance'?)

  # Perform clustering
  for ( clusterCount in 1:maxClusters ) {
    if ( nrow(tbl) < 2000 ) {
      logger.trace("\ttesting %d clusters using cluster::pam", clusterCount)
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)],clusterCount)
    } else {
      logger.trace("\ttesting %d clusters using cluster::clara", clusterCount)
      clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)],clusterCount, samples=50)
    }
    medoidLats <- clusterObj$medoids[,latVar]
    diameters <- 2 * clusterObj$clusinfo[,'max_diss'] # decimal degrees
    # NOTE:  We don't know whether distance is pure NS, EW or some combination
    # NOTE:  so we can't accurately convert to meters. We approximate by
    # NOTE:  assuming a 50-50 split and using 111,320 meters/degree at the equator.
    radianMedoidLats <- medoidLats * pi/180
    meters <- diameters * (1 + cos(radianMedoidLats))/2 * 111320
    if ( max(meters) < clusterDiameter ) break
  }

  logger.debug("Using %d cluster(s) with a diameter of %d meters", clusterCount, clusterDiameter)

  # Create the vector of deployment identifiers
  if ( nrow(tbl) < 2000 ) {
    clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)],clusterCount)
  } else {
    clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)],clusterCount, samples=50)
  }

  # Add medoid lons and lats to the tibble for use by wrcc_createMetaDataframe
  tbl$medoidLon <- clusterObj$medoids[,lonVar][clusterObj$clustering]
  tbl$medoidLat <- clusterObj$medoids[,latVar][clusterObj$clustering]

  # NOTE:  Each deploymentID identifies a unique location. We paste together
  # NOTE:  longitude and latitude rounded to 3 decimal places to create this identifier.
  # NOTE:  This should be enough accuracy for a unique deploymentID as
  # NOTE:  0.001 degrees of longitude = 102.47m at 23N, 43.496m at 67N

  lonString <- format( round(tbl$medoidLon,3), nsmall=3 ) # 3 decimal places
  latString <- format( round(tbl$medoidLat,3), nsmall=3 ) # 3 decimal places
  locationString <- paste0( 'lon_', lonString, '_lat_', latString )
  tbl$deploymentID <- make.names(locationString)

  # reinsert rows with bad locations if flagAndKeep = TRUE
  if ( flagAndKeep ) {
    # add new columns to badLocationRows for rbind
    if ( sum(badLocationMask) > 0 ) {
      badLocationRows$deploymentID <- NA
      badLocationRows$medoidLon <- NA
      badLocationRows$medoidLat <- NA
      # merge tibbles and sort based on dummy rowID
      tbl <- rbind(tbl, badLocationRows)
      tbl <- tbl[order(tbl$rowID),]
    }
    tbl$rowID <- NULL
  }

  return(tbl)
}
