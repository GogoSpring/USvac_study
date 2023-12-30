library(USvac)

# two groups
Group1<-c("Alabama","Alaska","Arizona","Arkansas","Georgia","Idaho","Indiana",       
          "Iowa","Kansas","Kentucky","Louisiana","Michigan","Mississippi","Missouri",      
          "Montana","Nebraska","Nevada","North Carolina","North Dakota","Ohio","Oklahoma",      
          "South Carolina","South Dakota","Tennessee","Texas","Utah","West Virginia","Wyoming" )
g1_num<-c(1,  2,  3,  4, 11, 13, 15, 16, 17, 18, 19, 23, 25, 26, 27, 28, 29, 34, 35, 36, 37, 41, 42, 43, 44, 45, 49, 51)

Group2<-c("California","Colorado","Connecticut","Delaware","District of Columbia",
          "Florida","Hawaii","Illinois","Maine","Maryland",            
          "Massachusetts","Minnesota","New Hampshire","New Jersey","New Mexico",          
          "New York","Oregon","Pennsylvania","Rhode Island","Vermont",             
          "Virginia","Washington","Wisconsin")
g2_num<-c(5,  6,  7,  8,  9, 10, 12, 14, 20, 21, 22, 24, 30, 31, 32, 33, 38, 39, 40, 46, 47, 48, 50)

# Fig 3a. Cases -----------------------------------------------------------------------
data(All_df)
#---PLOT
#pdf('../Fig 3a.pdf', height = 5.1, width = 6)
time = 1:nrow(All_df)
Date<- All_df$Date
actual_case<-All_df$columunG1
ylim<-1.05*max(All_df$columunG1_up)

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n", 
     xlab= "Time since the first recored Omicron case (days)", ylab="Daily reported cases per million",main="",cex.main=1.2,cex.lab=1.2) #Hospital admission rate (%)

polygon(c(time, rev(time)), c(All_df$columunG1_low[time], rev(All_df$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df$columunG2_low[time], rev(All_df$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df$columunG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(All_df),5),labels = Date[seq(1,nrow(All_df),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 

# Fig 3b. CHR -----------------------------------------------------------------------
data(All_df2)
#pdf('../Fig 3b.pdf', height = 5.1, width = 6)
time = 1:nrow(All_df2)
Date<- All_df2$Date
actual_case<-All_df2$columunG1*100
All_df2$columunG1<-All_df2$columunG1*100
All_df2$columunG2<-All_df2$columunG2*100
All_df2$columunG1_low<-All_df2$columunG1_low*100
All_df2$columunG2_low<-All_df2$columunG2_low*100
All_df2$columunG1_up<-All_df2$columunG1_up*100
All_df2$columunG2_up<-All_df2$columunG2_up*100

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,15), yaxt = "n", 
     xlab= "", ylab="CHR (%)",main="",cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(All_df2$columunG1_low[time], rev(All_df2$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df2$columunG2_low[time], rev(All_df2$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df2$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df2$columunG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(All_df2),5),labels = Date[seq(1,nrow(All_df2),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 
#dev.off() 


# Fig 3c. box-plot cumulative cases -----------------------------------------------------------------------
data(df_omicron_cum2)
df_omicron_cum2$group <- factor(df_omicron_cum2$group,levels = c("Low", "High"))
#pdf('.../Fig 3c.pdf', height = 4.25*1.25*1.25, width = 7.5)
boxplot(df_omicron_cum2$cases_per1000000 ~ df_omicron_cum2$group, col=c("#F4DCD8","#CCE4F0"),
        xlab = "Group",
        ylab="Cumulative cases per million",
        outline = FALSE)
# Add points
mylevels <- c("Low","High")
levelProportions <- as.vector(table(as.vector(df_omicron_cum2$group))/nrow(df_omicron_cum2))
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- df_omicron_cum2[df_omicron_cum2$group==thislevel, "cases_per1000000"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}
#dev.off()

# Fig 3d. box-plot mean_CHR -----------------------------------------------------------------------
data(df_chr)
df_chr$group <- as.factor(df_chr$group)
df_chr$group <- factor(df_chr$group,levels = c("Low", "High"))
#pdf('.../Fig 3d.pdf', height = 4.25*1.25*1.25, width = 7.5)
boxplot(df_chr$hosp_mean ~ df_chr$group, col=c("#F4DCD8","#CCE4F0"), 
        xlab = "Group",
        ylab="Mean CHR (%)",ylim=c(0,10),yaxt = "n",
        outline = FALSE)
axis(2, at = c(0,2,4,6,8,10))
# Add points
mylevels <- c("Low", "High")
levelProportions <- as.vector(table(as.vector(df_chr$group))/nrow(df_chr))
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- df_chr[df_chr$group==thislevel, "hosp_mean"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}
#dev.off()
