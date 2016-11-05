#' @keywords monitor
# @export
#' @title Subset ws_monitoring Object 'meta' Dataframe
#' @param meta 'meta' dataframe from a \code{ws_monitor} object
#' @param xlim optional longitude lim with lo and hi longitude values
#' @param ylim optional latitude lim with lo and hi latitude values
#' @param stateCodes optional vector of stateCodes
#' @param monitorIDs optional vector of monitorIDs
#' @description The incoming 'meta' dataframe is filtered to remove any monitors that
#'     lie outisde the specified range of longitudes and latitudes and that are not mentioned
#'     in the list of monitorIDs.
#'     
#' If any parameter is not specified, that parameter will not be used in the filtering 
#' @details Longitudes must be specified in the domain [-180,180].
#' 
#' @return monitoring 'meta' dataframe or \code{NULL} if filtering removes all monitors

monitor_subsetMeta <- function(meta, xlim=NULL, ylim=NULL, stateCodes=NULL, monitorIDs=NULL) {
  
  if (!is.null(xlim)) {
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
  
  if (!is.null(ylim)) {
    # Sanity check -- latitude domain
    if (min(ylim) < -90 || max(ylim) > 90) {
      stop(paste0('ylim = ',ylim,' contains values outside the domain [-90,90]'))
    }
    ylim <- sort(ylim)
    meta <- dplyr::filter(meta, meta$latitude >= ylim[1] & meta$latitude <= ylim[2])
  }
  
  if (!is.null(stateCodes)) {
    if ( 'stateCode' %in% names(meta) ) {
      meta <- dplyr::filter(meta, meta$stateCode %in% stateCodes)
    } else {
      warning("No 'stateCode' column found in monitor metadata.")
    }
  }
  
  if (!is.null(monitorIDs)) {
    meta <- dplyr::filter(meta, meta$monitorID %in% as.character(monitorIDs)) # allow for numeric monitorIDs
  }
  
  if (nrow(meta) == 0) {
    warning("No matching monitors found.")
    return (NULL)
  }
  
  # Restore rownames that dplyr::filter discards
  rownames(meta) <- meta$monitorID
  
  return(meta)
}


