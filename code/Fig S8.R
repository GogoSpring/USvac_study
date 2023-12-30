library(USvac)
#---PLOT
data(All_df_S4)
time = 1:nrow(All_df_S4)
Date<- All_df_S4$Date
actual_case<-All_df_S4$columunG1
ylim<-1.05*max(All_df_S4$columunG1_up)

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n", 
     xlab= "", ylab="Daily reported cases per million",main="",cex.main=1.2,cex.lab=1.2) #Hospital admission rate (%)

polygon(c(time, rev(time)), c(All_df_S4$columunG1_low[time], rev(All_df_S4$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df_S4$columunG2_low[time], rev(All_df_S4$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df_S4$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df_S4$columunG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(All_df_S4),5),labels = Date[seq(1,nrow(All_df_S4),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

# FIG S8B
data(cum_cases1_S)
#Boxplot1  for cumulative cases density
boxplot(cum_cases1_S$New_Cases_per1000000 ~ cum_cases1_S$group, col=c("#F4DCD8","#CCE4F0"),
        xlab = "Group",
        ylab="Cumulative cases per million",
        outline = FALSE)
# Add cum_cases1_S points
mylevels <- c("Low", "High")
levelProportions <- as.vector(table(as.vector(cum_cases1_S$group))/nrow(cum_cases1_S))
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- cum_cases1_S[cum_cases1_S$group==thislevel, "New_Cases_per1000000"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}

