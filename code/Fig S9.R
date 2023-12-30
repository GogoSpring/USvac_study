library(USvac)
data(arrivedata_S)
##compare mean_CHR
g1<-arrivedata_S$arrive_date[which(arrivedata_S$group=="Low")]-as.Date("2021-11-01")
g2<-arrivedata_S$arrive_date[which(arrivedata_S$group=="High")]-as.Date("2021-11-01")
t.test(g1,g2)

# Boxplot basic
arrivedata_S$group <- as.factor(arrivedata_S$group)
arrivedata_S$group <- factor(arrivedata_S$group,levels = c("Low", "High"))
#--- Boxplot1  for mean_hosp
boxplot(arrivedata_S$arrive_time_diff ~ arrivedata_S$group, col=c("#F4DCD8","#CCE4F0"), 
        xlab = "Group",
        ylab="Arrive time (since 2021-10-01)",yaxt = "n",
        outline = FALSE) #ylim=c(0,10),
axis(2, at = c(0,14,30,44), 
     labels = c("2021-11-01","2021-11-15","2021-12-01","2021-12-15"))

mylevels <- c("Low", "High")
levelProportions <- as.vector(table(as.vector(arrivedata_S$group))/nrow(arrivedata_S))
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- arrivedata_S[arrivedata_S$group==thislevel, "arrive_time_diff"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}
