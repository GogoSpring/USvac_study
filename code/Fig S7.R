library(USvac)
data(All_df_S2)
# Fig S7a
#---PLOT
time = 1:nrow(All_df_S2)
Date<- All_df_S2$Date
actual_case<-All_df_S2$columunG1
ylim<-1.05*max(All_df_S2$columunG1_up)

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n", 
     xlab= "", ylab="Daily reported cases per million",main=columun,cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(All_df_S2$columunG1_low[time], rev(All_df_S2$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df_S2$columunG2_low[time], rev(All_df_S2$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df_S2$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df_S2$columunG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(All_df_S2),5),labels = Date[seq(1,nrow(All_df_S2),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 


# Fig S7B
#---PLOT
data(All_df_S3)
time = 1:nrow(All_df_S3)
Date<- All_df_S3$Date
actual_case<-All_df_S3$columunG1

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(-60,38), yaxt = "n", 
     xlab= "", ylab="Daily mobility index(%)",main=,cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(All_df_S3$columunG1_low[time], rev(All_df_S3$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df_S3$columunG2_low[time], rev(All_df_S3$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df_S3$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df_S3$columunG2, lwd = 1, col ="#0473B6") 

legend("bottom", legend=c("Group1: Low vaccine coverage", "Group2: High vaccine coverage"),
       col=c("#C54F35","#0473B6"), lty=1:1, cex=0.8)

axis(1,at =seq(1,nrow(All_df_S3),5),labels = Date[seq(1,nrow(All_df_S3),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 


# Fig S7D
#---PLOT
data(Mob_mean_S)
boxplot(Mob_mean_S$transit_stations_percent_change_from_baseline ~ Mob_mean_S$group, col=c("#F4DCD8","#CCE4F0"),
        xlab = "Group",
        ylab="Transit stations percent change from baseline (%)",
        outline = FALSE)
# Add df_omicron_cum2 points
mylevels <- c("Low", "High")
levelProportions <- as.vector(table(as.vector(Mob_mean_S$group))/nrow(Mob_mean_S))
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- Mob_mean_S[Mob_mean_S$group==thislevel, "transit_stations_percent_change_from_baseline"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}


