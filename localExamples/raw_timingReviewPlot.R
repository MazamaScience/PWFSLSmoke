
raw_timingReviewPlot <- function(df, tlim=NULL, bg='white') {
  
  # Handle different monitor types
  monitorType <- unique(df$monitorType)
  monitorName <- unique(df$monitorName)
  
  # ----- Data preparation ------------------------------------------
  
  if ( monitorType == "ESAM" ) {
    time <- lubridate::mdy_hms(df$TimeStamp)
    pm25 <- df$Conc.mg.m3. * 1000
  } else {
    stop(paste0("monitorType ',monitorType,' is not recognized"))
  }
  
  # Apply tlim
  if ( !is.null(tlim) ) {
    tlo <- parseDatetime(tlim[1])
    thi <- parseDatetime(tlim[2])
    timeMask <- time >= tlo & time <= thi
    time <- time[timeMask]
    pm25 <- pm25[timeMask]
  }
  
  # Get variables for plotting
  minutes <- lubridate::minute(time)
  diffMinutes <- c(NA, diff(as.numeric(time))/60)
  # Assign measurements taken between 1:00 and 2:00 to 2:00 (end of hour reporting)
  assignedTime <- time
  lubridate::second(assignedTime) <- 0
  lubridate::minute(assignedTime) <- 0
  assignedTime <- assignedTime + lubridate::dhours(1)
  diffAssignedMinutes <- c(NA, diff(as.numeric(assignedTime))/60)
  colors <- AQI$colors[.bincode(pm25, AQI$breaks_24, include.lowest=TRUE)]
  
  
  # ----- Plotting ------------------------------------------------------------
  
  par(bg=bg)
  
  # Over/Under plot
  layout(matrix(seq(3)))
  
  # Minutes colored by value
  plot(time, minutes, pch=1, col=colors,
       xlab='TimeStamp', ylab='Minute of the Hour')
  title(paste0(monitorName, ' -- Raw Times'))
  
  # Minutes since last measurement
  plot(time, diffMinutes, pch=1, col=colors,
       xlab='TimeStamp', ylab='Minutes between timesteps')
  title(paste0(monitorName, ' -- Raw Times'))
  
  # Minutes since last measurement
  plot(assignedTime, diffAssignedMinutes, pch=1, col=colors,
       xlab='TimeStamp', ylab='Minutes between timesteps')
  title(paste0(monitorName, ' -- Assigned Times'))
  
  # Restore layout
  layout(1)
  
  par(bg='white')
  
}