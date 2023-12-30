library(USvac)
data(overall_hosp_S)
data(overall_cases_S)

par(mfrow=c(3,1))
# Plot - hospital admissions
ylim=max(overall_hosp_S$Daily_hospital_Admission)*1.04
time<-1:nrow(overall_hosp_S)
plot(time,overall_hosp_S$Daily_hospital_Admission,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="7-Day moving average of daily new hospital admissions",cex.main=1.2,cex.lab=1.2)
lines(overall_hosp_S$Daily_hospital_Admission, lwd = 1, col ="#F70B0B")
abline(v = 159, lty=2, col="red")
axis(1,las = 2,at =seq(1,nrow(overall_hosp_S),10),labels = overall_hosp_S$Date[seq(1,nrow(overall_hosp_S),10)],cex.axis=1.1)
axis(2,las = 2,cex.axis = 1.2,cex.axis=1.1) 

# Plot - cases
ylim=max(overall_cases_S$New_Cases)*1.04
time<-1:nrow(overall_cases_S)
plot(time,overall_cases_S$New_Cases,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="7-Day moving average of daily new cases ",cex.main=1.2,cex.lab=1.2)
lines(overall_cases_S$New_Cases, lwd = 1, col ="#952690")  
abline(v = 159, lty=2, col="red")
axis(1,at =seq(1,nrow(overall_cases_S),10),labels = overall_cases_S$Date[seq(1,nrow(overall_cases_S),10)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 

# Plot hospitalization rate
ylim=max(overall_hosp_S$Daily_hospital_Admission/overall_cases_S$New_Cases)*1.04
time<-1:nrow(overall_cases_S)
plot(time,overall_hosp_S$Daily_hospital_Admission/overall_cases_S$New_Cases,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
     xlab= "", ylab="7-Day moving average of CHR",cex.main=1.2,cex.lab=1.2)
lines(overall_hosp_S$Daily_hospital_Admission/overall_cases_S$New_Cases, lwd = 1, col ="#952690") 
abline(v=159,lty=2,col="red")
axis(1,at =seq(1,nrow(overall_cases_S),10),labels = overall_cases_S$Date[seq(1,nrow(overall_cases_S),10)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1)
