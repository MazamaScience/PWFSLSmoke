#' @keywords ws_monitor
#' @export
#' @title Subset ws_monitor Object 'meta' Dataframe
#' @param meta \emph{ws_monitor} object \code{meta} dataframe
#' @param xlim optional vector with low and high longitude limits
#' @param ylim optional vector with low and high latitude limits
#' @param stateCodes optional vector of stateCodes
#' @param countryCodes optional vector of countryCodes
#' @param monitorIDs optional vector of monitorIDs
#' @return A \emph{ws_monitor} object \code{meta} dataframe, or \code{NULL} if filtering removes all monitors.
#' @description Subsets the \code{ws_monitor$data} dataframe by removing any monitors that
#' lie outisde the geographical ranges specified (i.e. outside of the given longitudes and 
#' latitudes and/or states) and that are not mentioned in the list of monitorIDs.
#' @description If any parameter is not specified, that parameter will not be used in the subsetting.
#' @description Intended for use by the monitor_subset function.
#' @details Longitudes must be specified in the domain [-180,180].

monitor_subsetMeta <- function(meta, xlim=NULL, ylim=NULL, stateCodes=NULL, countryCodes=NULL, monitorIDs=NULL) {
  
  if ( !is.null(xlim) ) {
    # Sanity check -- longitude domain
    for (i in seq(2)) {
      if (xlim[i] < -180 || xlim[i] > 360) {
        xlim[i] <- xlim[i] %% 360
      }
      if (xlim[i] > 180) {
        xlim[i] <- xlim[i] - 360
      }
    }
    # NOTE:  Branch cut is at -180. We will not allow ranges that cross the branch cut.
    xlim <- sort(xlim)
    meta <- dplyr::filter(meta, meta$longitude >= xlim[1] & meta$longitude <= xlim[2])
  }
  
  if ( !is.null(ylim) ) {
    # Sanity check -- latitude domain
    if (min(ylim) < -90 || max(ylim) > 90) {
      stop(paste0('ylim = ',ylim,' contains values outside the domain [-90,90]'))
    }
    ylim <- sort(ylim)
    meta <- dplyr::filter(meta, meta$latitude >= ylim[1] & meta$latitude <= ylim[2])
  }
  
  if ( !is.null(countryCodes) ) {
    # Guarantee upper case countrycodes
    countryCodes <- stringr::str_to_upper(countryCodes)
    if ( 'countryCode' %in% names(meta) ) {
      meta <- dplyr::filter(meta, meta$countryCode %in% countryCodes)
    } else {
      warning("No 'countryCode' column found in monitor metadata.")
    }
  }
  
  if ( !is.null(stateCodes) ) {
    # Guarantee upper case statecodes
    stateCodes <- stringr::str_to_upper(stateCodes)
    if ( 'stateCode' %in% names(meta) ) {
      meta <- dplyr::filter(meta, meta$stateCode %in% stateCodes)
    } else {
      warning("No 'stateCode' column found in monitor metadata.")
    }
  }
  
  if ( !is.null(monitorIDs) ) {
    monitorIDs <- as.character(monitorIDs) # allow incoming monitorIDs to be numeric
    meta <- dplyr::filter(meta, meta$monitorID %in% monitorIDs)
  }
  
  if ( nrow(meta) == 0 ) {
    warning("No matching monitors found.")
    return (NULL) # TODO:  ticket #86 (must be coordinated with monitor_subsetData and any code that checks for this)
  }
  
  # Restore rownames that dplyr::filter discards
  rownames(meta) <- meta$monitorID
  
  # Guarantee that monitors are returned in the order requested
  if ( !is.null(monitorIDs) ) {
    foundMonitorIDs <- intersect(monitorIDs, rownames(meta)) # perhaps not all monitorIDs were found
    meta <- meta[foundMonitorIDs,]
  }
  
  return(meta)
}


