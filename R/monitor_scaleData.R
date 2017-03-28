#' @keywords ws_monitor
#' @export
#' @title Scale Monitoring Data
#' @param ws_monitor \emph{ws_monitor} object
#' @param expression an \R{R} expression used to scale data
#' @description Use an R expression scale the data in a \emph{ws_monitor} object.
#' 
#' The \R{R} expression given in \code{expression} is applied to all data columns in
#' in \code{ws_monitor$data}.
#' 
#' Expressions should use \code{data} for the left hand side of the expression.
#' @examples
#' wa <- monitor_subset(Northwest_Megafires, stateCodes='WA')
#' wa_zero <- monitor_scaleData(wa, data * 3.4)

monitor_scaleData <- function(ws_monitor, expression) {
  
  # Sanity check
  if ( !"ws_monitor" %in% class(ws_monitor) ) {
    stop("ws_monitor object is not of class 'ws_monitor'.")
  }
  
  # Create a condition call, basically an expression that isn't run yet.
  condition_call <- substitute(expression)
  expressionString <- paste(as.character(condition_call)[2], as.character(condition_call)[1],
                      as.character(condition_call)[3])
  
  # NOTE:  Example condition_call:
  # NOTE:  > as.character(condition_call)
  # NOTE:  [1] "*"    "data" "3.4"   
  
  # expressionString must contain 'data'
  if ( any(stringr::str_detect(expressionString, 'data')) ) {
    
    # NOTE:  We must do extra work to avoid conversion to numeric in the case 
    # NOTE:  where there is only a single column of data.
    
    # Create a data-only dataframe by omitting the first 'datetime' column
    data <- as.data.frame(ws_monitor$data[,-1])
    colnames(data) <- colnames(ws_monitor$data)[-1]
    
    # FUN will be applied to each data column in turn
    FUN <- function(list) { eval(condition_call, data.frame(data = list)) }
    data <- apply(data, 2, FUN)

    # Replace ws_monitor$data data columns with new data
    ws_monitor$data[,-1] <- data
    
  } else {    
    
    stop( paste0("Bad expression \"", expressionString, "\" passed in.") )
    
  }
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}
