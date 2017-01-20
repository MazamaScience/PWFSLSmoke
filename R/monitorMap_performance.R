#' @keywords monitor
#' @export
#' @title Create Map of Monitor Prediction Performance
#' @param predicted data list of class \code{ws_monitor}
#' @param observed data list of class \code{ws_monitor}
#' @param threshold value used to classify \code{predicted} and \code{observed} measurements
#' @param sizeBy name of the metric used to create relative sizing
#' @param colorBy name of the metric used to create relative colors
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks - Must also specify the colorBy paramater
#' @param paletteFunc a palette generating function as returned by \code{colorRampPalette}
#' @param showLegend logical specifying whether to add a legend (default: \code{TRUE})
#' @param stateCol color for state outlines on the map
#' @param stateLwd width for state outlines
#' @param countyCol color for county outline on the map
#' @param countyLwd width for county outlines
#' @param add logical flag that specifies whether to add to the current plot
#' @param ... additional arguments to be passed to the map() funciton such as graphical parameters (see par)
#' @description 
#' @examples 
#' \dontrun{
#' }

if(FALSE) {
  threshold=AQI$breaks_24[3]
  cex=par('cex')
  sizeBy=NULL
  colorBy="heidikeSkill"
  breaks=c(-Inf,.5,.6,.7,.8,Inf)
  paletteFunc=grDevices::colorRampPalette(RColorBrewer::brewer.pal(6,"Purples")[-1])
  legendTitle="Max AQI Level"
  showLegend=TRUE
  stateCol="grey60"
  stateLwd=2
  countyCol="grey70"
  countyLwd=1
  add=FALSE
}

monitorMap_performance <- function (predicted,
                                    observed,
                                    threshold=AQI$breaks_24[3],
                                    cex=par('cex'), 
                                    sizeBy=NULL,
                                    colorBy="heidikeSkill",
                                    breaks=c(-Inf,.5,.6,.7,.8,Inf),
                                    paletteFunc=grDevices::colorRampPalette(RColorBrewer::brewer.pal(6,"Purples")[-1]),
                                    showLegend=TRUE,
                                    stateCol="grey60",
                                    stateLwd=2,
                                    countyCol="grey70",
                                    countyLwd=1,
                                    add=FALSE, 
                                    ...) {
  
  #------------- below copies from monitor_performanceMap-------------------
  # Get the performance dataframe
  performanceDF <- monitor_performance(predicted, observed, threshold, threshold)

  # Plot the basemap
  if ( !add ) {
    
  # list of unique states to be plotted as base map
  stateCode <- as.data.frame( unique( c( as.character( predicted$meta$stateCode) ) ) )
  colnames(stateCode) <- "abb"
  state.fips <- maps::state.fips
  duplicateIndex <- duplicated(state.fips$abb)
  state.fips <- state.fips[!duplicateIndex,]
  suppressWarnings(stateName <- dplyr::left_join(stateCode, state.fips, by="abb"))
  stateName <- apply(as.data.frame(stateName$polyname),2,function(x){stringr::str_split_fixed(x, ':', 2)})[1:nrow(stateName)]
  

    maps::map("state", stateName, col=stateCol, lwd=stateLwd, ...)
    maps::map('county', stateName, col=countyCol, lwd=countyLwd, add=TRUE, ...)
  }
  

  # Sizing
  if ( !is.null(sizeBy) && sizeBy %in% names(performanceDF)) {
    cex <- cex * performanceDF[[sizeBy]] / max(performanceDF[[sizeBy]], na.rm = TRUE) 
  }
  
  # This chunk  of code figures out colors for the map.
  if ( !is.null(colorBy) && colorBy %in% names(performanceDF) ) {
    if ( length(breaks) == 1 ) {
      probs <- seq(0,1,length.out=(breaks+1))
      breaks <- stats::quantile(performanceDF[[colorBy]], probs=probs, na.rm=TRUE)
    }
    indices <- .bincode(performanceDF[[colorBy]], breaks=breaks, include.lowest=TRUE)
    colors <- paletteFunc(max(indices,na.rm=TRUE))
    cols <- colors[indices]
    
    # create a legend to be used later
    legend <- character(length(breaks) - 1)
    for(i in 1: (length(breaks) - 1 ) ){
      legend[i] <- paste0(breaks[i], "-", breaks[i+1])
    }
  }
  
  lon <- predicted$meta$longitude
  lat <- predicted$meta$latitude
  
  # Now we add the (potentially projected) monitor points
  argsList <- list(...)
  
  if( is.null(argsList$projection) ) {
    points(lon, lat, pch=16, cex=cex, col=cols)
  } else {
    points( mapproj::mapproject(lon,lat,argsList$projection, argsList$parameters, argsList$orientation), 
            pch=16, cex=cex, col=cols )
  }
  
  if (showLegend) { addLegend( col=rev(colors), legend=rev(legend), title=paste0(colorBy, " levels") ) }
  
}