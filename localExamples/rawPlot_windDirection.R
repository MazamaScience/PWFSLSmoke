# # The following code was an initial attempt at creating a wind direction plot on an X-Y plane, using color to indicate
# # whether the wind direction has an easterly or westerly component (i.e. axes would go from 0-180, then from 180-360, 
# # but on the same axis -- color would indicate which value applies).
# 
# # Alternatively, may consider creating a wind speed/direction plot similar to that used by Weather Underground...
# 
# # sample code for wind direction plot
# # double axes from https://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
#   
# windDir <- df$windDir
# 
# cols <- rep("black",length(windDir))
# eMask <- windDir <180 & windDir>=0
# wMask <- windDir <360 & windDir>=180
# 
# cols[which(eMask)] <- "blue"
# cols[which(wMask)] <- "red"
# 
# #par(mar=c(5, 12, 4, 4) + 0.1)
# 
# windDir[wMask] <- 360-windDir[wMask]
# 
# plot(df$datetime,windDir,col=cols,ylim=c(180,0))
# title("Wind Direction: Red = from east, Blue = from West") #this really needs a second axis to be legit
# #axis(4,ylim=c(0,20),col="red",lwd=2)
# 
# #basic plot
# plot(df$datetime,df$windDir,col=cols,ylim=c(180,0))
# #box()
# #axis(2, ylim=c(0,180),col="blue",lwd=2)
# #axis(1)