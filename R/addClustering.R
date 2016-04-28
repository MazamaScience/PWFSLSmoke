#' @keywords internal
#' @export
#' @title Add Clustering Information to a Dataframe
#' @param df dataframe with geolocation information (e.g. created by wrcc_qualityControl() or airsis_qualityControl)
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see description)
#' @param minCount minimum number of measurements for a location to be valid (see description)
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param maxClusters maximum number of clusters to try
#' @param verbose logical flag to generate verbose output
#' @description Clustering is used to assign individual measurements to deployment locations.
#' 
#' clusterRadius is compared with the output of cluster::pam(...)$clusinfo[,'av_diss']
#' to determine the number of clusters.
#' 
#' @return Input dataframe with additional columns: deploymentID, medoidLon, mediodLat.

addClustering <- function(df, clusterDiameter=1000, minCount=48,
                          lonVar="longitude", latVar="latitude",
                          maxClusters=50, verbose=FALSE) {
  
  # Sanity check -- make sure df does not have class "tbl_df"
  df <- as.data.frame(df)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) )
    stop(paste0("Longitudes or latitudes could not be found.  Did you specify lonVar and latVar arguments?"))
  
  
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


  if (verbose) cat(paste0('Trying up to ',maxClusters,' clusters '))
  
  # NOTE:  We need to use cluster::clara when we get above ~2K points.
  # NOTE:  For this reason we need to use clusinfo[,'max_diss'] instead
  # NOTE:  of clusinfo[,'diameter'] as the latter is only provided by 
  # NOTE:  cluster::pam and not cluster::clara.
  # NOTE:  (Is there really any difference between 'max_diss' and 'distance'?)
  
  # Perform clustering
  for (clusterCount in 1:maxClusters) {
    if (verbose) cat(paste0('.'))
    if ( nrow(df) < 2000 ) {
      clusterObj <- cluster::pam(df[,c(lonVar,latVar)],clusterCount)
    } else {
      clusterObj <- cluster::clara(df[,c(lonVar,latVar)],clusterCount, samples=50)
    }
    medoidLats <- clusterObj$medoids[,latVar]
    diameters <- clusterObj$clusinfo[,'av_diss'] # decimal degrees
    # NOTE:  We don't know whether distance is pure NS, EW or some combination
    # NOTE:  so we can't accurately convert to meters. We approximate by 
    # NOTE:  assuming a 50-50 split and using 111,320 meters/degree at the equator.
    meters <- diameters * (1 + cos(medoidLats))/2 * 111320
    if ( max(meters) < clusterDiameter ) break
  }
  
  if (verbose) cat(paste0('\nUsing ',clusterCount,' clusters.\n'))
  
  # Create the vector of deployment identifiers
  if ( nrow(df) < 2000 ) {
    clusterObj <- cluster::pam(df[,c(lonVar,latVar)],clusterCount)
  } else {
    clusterObj <- cluster::clara(df[,c(lonVar,latVar)],clusterCount, samples=50)
  }
  df$deploymentID <- clusterObj$clustering
  
  # Add medoid lons and lats to the dataframe for use by wrcc_createMetaDataframe
  df$medoidLon <- clusterObj$medoids[,lonVar][df$deploymentID]
  df$medoidLat <- clusterObj$medoids[,latVar][df$deploymentID]
  
  return(as.data.frame(df))
  
}
