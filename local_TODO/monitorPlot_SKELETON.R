#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create XXX Plot
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param ylim y limits for the plot
#' @description Creates a XXX plot showing PM 2.5 values for a specific monitor in a \emph{ws_monitor} object.
#' MORE DESCRIPTION HERE.
#' @examples
#' \dontrun{
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150715,20150930))
#' main <- "Daily Average PM2.5 for Omak, WA"
#' monitorPlot_XXX(N_M, monitorID="530470013")
#' }

monitorPlot_XXX<- function(ws_monitor,
                           monitorID=NULL,
                           tlim=NULL,
                           ylim=NULL) {

  # ----- Style ---------------------------------------------------------------
  
  # item1 as gray circle
  pch_item1 <- 16
  cex_item1 <- 3
  col_item1 <- adjustcolor('black',0.5)


  # ----- Data Preparation ----------------------------------------------------
  
  # Whatever you need to do to further process the monitor data
  
  x <- 1:10 # example variable that you create here
  y <- 1:10 # example variable that you create here
  
  
  # ----- Plotting ------------------------------------------------------------
  
  # Any changes to overall graphical paramters, eg. mar, 
  
  default_mar <- c(5.1,4.1,4.1,2.1)
  par(mar=c(5.1,7.1,4.1,7.1))
  
  # Probalby set up a blank plot with axes
  
  # Blank plot to get the scaling
  plot(x, y, col='transparent', axes=FALSE,
       xlab='X Axis', ylab='Y Axis')
  
  # Sometimes you want to create custom axes if "axes=FALSE" above
  # Add box and Y-axis
  box()
  axis(1) # Lots of room to cutomize if needed
  axis(2, las=1)
  
  # Add first layer of data 
  points(x, y, pch=pch_item1, cex=cex_item1, col=col_item1)
  
  # Add second layer of data
  
  # Add ...
 
   
  # ----- Annotations ---------------------

  # Title
  title('Title Goes Here')
  
  # Other margin text
  
  # Labels
  
  # Lines and rectangles (if they need to go underneath, plot them first)
  
  # Add a second Y axis if needed
  
  # Add a legend
  
  # Add anything else
  
  # Add the final things 
  
  # Reset margins and other graphical parameters
  par(mar=default_mar)
  
}

