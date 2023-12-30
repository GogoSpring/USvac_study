library(USvac)
data(ALL_df_S)

#----Fig S5 CHR plot
data(state_mutation_all_S)

plot.each.mutation <- function(mutation) {
  
  State_df<-IQR.mutation(state_mutation_all_S, mutation)
  time = 1:nrow(State_df)
  Date<- State_df$Date
  actual_case<-State_df$mutation
  ylim<-1
  plot(time,actual_case,pch=16,xaxt = "n",col="white",ylim = c(0,ylim), yaxt = "n",
       xlab= "", ylab="Prevalence",main=mutation,cex.main=1.2,cex.lab=1.2) 
 
  polygon(c(time, rev(time)), c(State_df$mutation_low[time], rev(State_df$mutation_up[time])), density = NA,
          col = rgb(146,193,220, 190, maxColorValue=255), border = NA)  
  
  lines(State_df$mutation, lwd = 1, col ="#4375B6") 
  abline(v=168,lty=2,col="black")
  axis(1,at =seq(1,nrow(State_df),3),labels = Date[seq(1,nrow(State_df),3)],las = 2,cex.axis=1.1)
  axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 
  
}

#pdf('C:/Users/jliang68local/Desktop/extend study period/BA12.pdf', height = 20, width = 23)
par(mfrow=c(5,4))

plot.each.mutation("S371L") #BA1
plot.each.mutation("G496S") #BA1
plot.each.mutation("G446S") #BA1

plot.each.mutation("S371F") #BA2
plot.each.mutation("T376A") #BA2
plot.each.mutation("R408S") #BA2
plot.each.mutation("D405N") #BA2


plot.each.mutation("G339D")
plot.each.mutation("S373P")
plot.each.mutation("S375F")
plot.each.mutation("K417N")
plot.each.mutation("N440K")
plot.each.mutation("S477N")
plot.each.mutation("T478K")
plot.each.mutation("E484A")
plot.each.mutation("Q493R")
plot.each.mutation("Q498R")
plot.each.mutation("N501Y")
plot.each.mutation("Y505H")

#dev.off()

