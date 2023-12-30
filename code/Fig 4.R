setwd(".../US/")
library(USvac)

# Fig 4a -----------------------------------------------------------------------
BA1_prevalence<-read.csv("data/BA1_prevalence.csv")
#---PLOT
#pdf('.../Fig 4a.pdf', height = 5.1, width = 6)
time = 1:nrow(BA1_prevalence)
background_time<-1:53  #BA1 
Date<- BA1_prevalence$Date
actual_case<-BA1_prevalence$mutationG1
ylim<-1

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="Prevalence",main="BA.1/BA.1.1-associated mutations",cex.main=1.2,cex.lab=1.2) 

polygon(c(background_time, rev(background_time)), c(rep(0,length(background_time)), rev(rep(1,length(background_time))) ), density = NA,
        col = rgb(246,246,246, 190, maxColorValue=255), border = NA)

polygon(c(time, rev(time)), c(BA1_prevalence$mutationG1_low[time], rev(BA1_prevalence$mutationG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(BA1_prevalence$mutationG2_low[time], rev(BA1_prevalence$mutationG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(BA1_prevalence$mutationG1, lwd = 1, col ="#C54F35")  
lines(BA1_prevalence$mutationG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(BA1_prevalence),5),labels = Date[seq(1,nrow(BA1_prevalence),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 



# Fig 4b. -----------------------------------------------------------------------
BA1BA2_prevalence<-read.csv("data/BA1BA2_prevalence.csv")
#---PLOT
#pdf('.../Fig 4b.pdf', height = 5.1, width = 6) 
time = 1:nrow(BA1BA2_prevalence)
background_time<-1:53   

Date<- BA1BA2_prevalence$Date
actual_case<-BA1BA2_prevalence$mutationG1
ylim<-1

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="Prevalence",main="Shared mutations between BA.1/BA.1.1 and BA.2",cex.main=1.2,cex.lab=1.2) 

polygon(c(background_time, rev(background_time)), c(rep(0,length(background_time)), rev(rep(1,length(background_time))) ), density = NA,
        col = rgb(246,246,246, 190, maxColorValue=255), border = NA)

polygon(c(time, rev(time)), c(BA1BA2_prevalence$mutationG1_low[time], rev(BA1BA2_prevalence$mutationG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(BA1BA2_prevalence$mutationG2_low[time], rev(BA1BA2_prevalence$mutationG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(BA1BA2_prevalence$mutationG1, lwd = 1, col ="#C54F35")  
lines(BA1BA2_prevalence$mutationG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(BA1BA2_prevalence),5),labels = Date[seq(1,nrow(BA1BA2_prevalence),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 

# Fig 4c -----------------------------------------------------------------------
BA2_prevalence<-read.csv("data/BA2_prevalence.csv")
#---PLOT
#pdf('.../Fig 4c.pdf', height = 5.1, width = 6)      
time = 1:nrow(BA2_prevalence)
background_time<-32:102 #BA2

Date<- BA2_prevalence$Date
actual_case<-BA2_prevalence$mutationG1
ylim<-1

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="Prevalence",main="BA.2-associated mutations",cex.main=1.2,cex.lab=1.2) 
# background regions
polygon(c(background_time, rev(background_time)), c(rep(0,length(background_time)), rev(rep(1,length(background_time))) ), density = NA,
        col = rgb(246,246,246, 190, maxColorValue=255), border = NA)

polygon(c(time, rev(time)), c(BA2_prevalence$mutationG1_low[time], rev(BA2_prevalence$mutationG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(BA2_prevalence$mutationG2_low[time], rev(BA2_prevalence$mutationG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(BA2_prevalence$mutationG1, lwd = 1, col ="#C54F35") 
lines(BA2_prevalence$mutationG2, lwd = 1, col ="#0473B6") 

axis(1,at =seq(1,nrow(BA2_prevalence),5),labels = Date[seq(1,nrow(BA2_prevalence),5)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 

# Fig 4d-4f -----------------------------------------------------------------------
library(USvac)
data(extdata)
model_fitting("Intermediate model",extdata)

# Fig 4g -----------------------------------------------------------------------
BA1_sim<-read.csv(".../output/BA1_sim.csv")
#pdf('.../Fig 4g.pdf', height = 4.335, width = 5.1)
time = 1:nrow(BA1_sim)
Date<- BA1_sim$Date
actual_case<-BA1_sim$onlyBA1_7MA

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,1), yaxt = "n",
     xlab= "", ylab="Proportion of mutations",main="BA.1/BA.1.1-associated mutations",cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(BA1_sim$prelow_onlyBA1_vac45_7MA, rev(BA1_sim$prehigh_onlyBA1_vac45_7MA)), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA) 

polygon(c(time, rev(time)), c(BA1_sim$prelow_onlyBA1_vac70_7MA, rev(BA1_sim$prehigh_onlyBA1_vac70_7MA)), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA) 

lines(BA1_sim$pre_onlyBA1_vac70_7MA, lwd = 1, col ="#43A7DC")
lines(BA1_sim$onlyBA1_7MA, lwd = 1, col ="black",lty=2)
lines(BA1_sim$pre_onlyBA1_vac45_7MA, lwd = 1, col ="#DB7346") ##4034C1 

axis(1,at =seq(1,nrow(BA1_sim),4),labels = Date[seq(1,nrow(BA1_sim),4)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

legend("bottom", legend=c("Observed (vaccine coverage of 59%)",
                          "Low vaccine coverage of 45% (senario 1)", 
                          "High vaccine coverage of 70% (senario 2)"),
       col=c("black","#DB7346","#43A7DC"), lty=c(2,1,1), cex=0.8)

#dev.off() 

# Fig 4h -----------------------------------------------------------------------
BA1BA2_sim<-read.csv(".../output/BA1BA2_sim.csv")
#pdf('.../Fig 4h.pdf', height = 4.335, width = 5.1)
time = 1:nrow(BA1BA2_sim)
Date<- BA1BA2_sim$Date
actual_case<-BA1BA2_sim$bothBA12_7MA

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,1), yaxt = "n",
     xlab= "", ylab="Proportion of mutations",main="Shared mutations between BA.1/BA.1.1 and BA.2 ",cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(BA1BA2_sim$prelow_bothBA12_vac45_7MA, rev(BA1BA2_sim$prehigh_bothBA12_vac45_7MA)), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA) 

polygon(c(time, rev(time)), c(BA1BA2_sim$prelow_bothBA12_vac70_7MA, rev(BA1BA2_sim$prehigh_bothBA12_vac70_7MA)), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA) 

lines(BA1BA2_sim$pre_bothBA12_vac70_7MA, lwd = 1, col ="#43A7DC")
lines(BA1BA2_sim$bothBA12_7MA, lwd = 1, col ="black",lty=2)
lines(BA1BA2_sim$pre_bothBA12_vac45_7MA, lwd = 1, col ="#DB7346")  

axis(1,at =seq(1,nrow(BA1BA2_sim),4),labels = Date[seq(1,nrow(BA1BA2_sim),4)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 

# Fig 4i -----------------------------------------------------------------------
BA2_sim<-read.csv(".../US/output/BA2_sim.csv")
#pdf('.../Fig 4i', height = 4.335, width = 5.1)
time = 1:nrow(BA2_sim)
Date<- BA2_sim$Date
actual_case<-BA2_sim$onlyBA2_7MA

plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,1), yaxt = "n",
     xlab= "", ylab="Proportion of mutations",main="BA.2-associated mutations",cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(BA2_sim$prelow_onlyBA2_vac45_7MA, rev(BA2_sim$prehigh_onlyBA2_vac45_7MA)), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA) 

polygon(c(time, rev(time)), c(BA2_sim$prelow_onlyBA2_vac70_7MA, rev(BA2_sim$prehigh_onlyBA2_vac70_7MA)), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA) 

lines(BA2_sim$pre_onlyBA2_vac70_7MA, lwd = 1, col ="#43A7DC")
lines(BA2_sim$onlyBA2_7MA, lwd = 1, col ="black",lty=2)
lines(BA2_sim$pre_onlyBA2_vac45_7MA, lwd = 1, col ="#DB7346")  

axis(1,at =seq(1,nrow(BA2_sim),4),labels = Date[seq(1,nrow(BA2_sim),4)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

#dev.off() 