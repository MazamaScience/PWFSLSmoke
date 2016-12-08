###############################################################################
# 
# Description of plot goes here
#
###############################################################################


# NOTE:  This next section has to be commented out when you build the package
# NOTE:  but it's great to keep around for debugging

# if (FALSE) {
# 
#  # Set up individual parameter values so that you can run the code inside
#  # the function for debugging.
# 
#  ws_monitor <- arinow_load(20160901,20160930)
#  # somehow subset ws_monitor to a single monitor
#  a <- 11
#  b <- 22
#  c <- TRUE
#  d <- FALSE
#  e <- 'Joyful'
#   
# }

myPlot_functionName<- function(ws_monitor,
                               a=1,
                               b=2,
                               c=TRUE,
                               d=TRUE,
                               e='Happy') {
  
  # ----- Style ---------------------------------------------------------------
  
  # circle as gray
  pch_circle <- 16
  cex_circle <- 3
  col_circle <- adjustcolor('black',0.5)
  lwd_circle <- 2

  # events as gray triangles with red outline
  cex_events <- 3
  pch_events <- 17
  col_events <- 'gray60'
  pch_eventsBorder <- 2
  col_eventsBorder <- 'red'
  lwd_eventsBorder <- 2
  
  
  # ----- Data Preparation ----------------------------------------------------
  
  # Whatever you need to do to further process the data

  x <- 1:10 # example variable that you create here
  y <- 1:10 # example variable that you create here


  # ----- Plotting ------------------------------------------------------------
  
  # Any changes to overall graphical paramter, eg. mar, 

  old_mar <- c(5.1,4.1,4.1,2.1)
  par(mar=c(5.1,7.1,4.1,7.1))
  
  # Probalby set up a blank plot with axes

  # Blank plot to get the scaling
  plot(x, y, col='transparent', axes=TRUE, xlab='', ylab='')
  
  # Sometimes you want to create custom axes if "axes=FALSE" above
  # Add box and Y-axis
  box()
  axis(1) # Lots of room to cutomize if needed
  axis(2, las=1)
  
  # Add first layer of data 

  # Add second layer of data

  # ...

  # Annotations
  title(title)
  
  # Labels scattered around

  # Lines and rectangles (if they need to go underneath, plot them first)

  # Add a second Y axis if needed

  # Add a legend
  
  # Add another
  
  # Add the final things 

  # Reset margins and other graphical parameters
  par(mar=old_mar)
  
}

