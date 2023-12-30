library(USvac)
df1<-read.csv(".../All_MA.csv")[,-1] 
df1$Date<-as.Date(df1$Date) 
head(df1)
df1<-df1[order(df1$State,df1$Date),] 
df1$New_Cases_MA<-as.numeric(df1$New_Cases_MA) 
df1<-df1[which(as.Date("2020-1-1")<df1$Date & df1$Date<as.Date("2021-1-1")),]
cases_df<-df1[,c("State","Date","New_Cases_MA")]

Pop<-read.csv(".../US_population.csv")
State_list<-read.csv(".../Sate_list.csv")[,-1]
cases_df1<-merge(cases_df,Pop,by="State")
cases_df1$New_Cases_MA_per1000000<-(cases_df1$New_Cases_MA/cases_df1$Pop)*1000000
head(cases_df1)

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

IQR_columun_df <- function (df,columun) {
  IQR_columun_df<-df[,c(columun,"Date")]
  colnames(IQR_columun_df)<-c("columun","Date")
  median_df<-aggregate(columun~Date,IQR_columun_df,median)
  colnames(median_df)<-c("Date","columun")
  
  median_df_low<-aggregate(columun~Date,IQR_columun_df,quantile,probs = 0.25) #probs = 0.25 probs = 0.025
  colnames(median_df_low)<-c("Date","columun_low")
  
  median_df_up<-aggregate(columun~Date,IQR_columun_df,quantile,probs = 0.75) #probs = 0.75 probs = 0.975
  colnames(median_df_up)<-c("Date","columun_up")
  
  IQR_columun_df1<-merge(median_df,median_df_low)
  IQR_columun_df2<-merge(IQR_columun_df1,median_df_up)
  
  return(IQR_columun_df2)
}

##########################
# FIG S10 A
##########################
columun<-"New_Cases_MA_per1000000"
G1<-IQR_columun_df(cases_df1[cases_df1$State %in% Group1,],columun)
G2<-IQR_columun_df(cases_df1[cases_df1$State %in% Group2,],columun)
colnames(G1)<-c("Date","columunG1","columunG1_low","columunG1_up")
colnames(G2)<-c("Date","columunG2","columunG2_low","columunG2_up")
All_df<-merge(G1,G2)


#---PLOT
time = 1:nrow(All_df)
Date<- All_df$Date
actual_case<-All_df$columunG1

plot(time,actual_case,pch=16,xaxt = "n",col="white",yaxt = "n", ylim = c(0,1000),
     xlab= "", ylab="Daily reported cases per million",main=columun,cex.main=1.2,cex.lab=1.2) 

polygon(c(time, rev(time)), c(All_df$columunG1_low[time], rev(All_df$columunG1_up[time])), density = NA,
        col = rgb(244,220,216, 190, maxColorValue=255), border = NA)  

polygon(c(time, rev(time)), c(All_df$columunG2_low[time], rev(All_df$columunG2_up[time])), density = NA,
        col = rgb(204,228,240, 150, maxColorValue=255), border = NA)  
lines(All_df$columunG1, lwd = 1, col ="#C54F35")  
lines(All_df$columunG2, lwd = 1, col ="#0473B6") 

legend("top", legend=c("Group1: Low vaccine coverage", "Group2: High vaccine coverage"),
       col=c("#C54F35","#0473B6"), lty=1:1, cex=0.8)

axis(1,at =seq(1,nrow(All_df),10),labels = Date[seq(1,nrow(All_df),10)],las = 2,cex.axis=1.1)
axis(2,las = 1,cex.axis = 1.2,cex.axis=1.1) 


####################################
# FIG S10 B
####################################
df_cum<-aggregate(New_Cases~State,data=df1,sum,na.action = na.omit)
df_cum1<-merge(df_cum,Pop,by="State")
df_cum1$cases_per1000000<-(df_cum1$New_Cases/df_cum1$Pop)*1000000
df_cum1$group[which(df_cum1$State %in% Group1)]<-"Low"
df_cum1$group[which(df_cum1$State %in% Group2)]<-"High"

df_cum1$group <- as.factor(df_cum1$group)
df_cum1$group <- factor(df_cum1$group,levels = c("Low", "High"))

df_cum2<-df_cum1
g1<-df_cum2$cases_per100000[which(df_cum2$group=="Low")]
g2<-df_cum2$cases_per100000[which(df_cum2$group=="High")]
t.test(g1,g2)  #p<0.05 有差異

#--- Boxplot1  for cumulative cases
boxplot(df_cum2$cases_per1000000 ~ df_cum2$group, col=c("#F4DCD8","#CCE4F0"),
        xlab = "Group",
        ylab="Cumulative cases per million",
        outline = FALSE)
# Add df_omicron_cum2 points
mylevels <- c("Low", "High")
levelProportions <- as.vector(table(as.vector(df_cum2$group))/nrow(df_cum2))
for(i in 1:length(mylevels)){
  #i<-1
  thislevel <- mylevels[i]
  thisvalues <- df_cum2[df_cum2$group==thislevel, "cases_per1000000"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
}

####################################
# FIG S10 CD
####################################
Sys.setlocale("LC_TIME","English")
State_list<-read.csv("D:/PROJECT/US/data/output/Sate_list.csv")[,-1]

Mob<-read.csv("D:/PROJECT/US/data/mobility/Global_Mobility_Report.csv")
Mob<-Mob[which(Mob$country_region_code=="US"),]
Mob1<-Mob[,c(3,9,10:15)]
Mob1$date<-as.Date(Mob1$date)
head(Mob1);unique(Mob1$sub_region_1);colnames(Mob1)
Mob_df<-Mob1[which(as.Date("2020-1-1")<Mob1$date & Mob1$date<as.Date("2021-1-1")),] 
colnames(Mob_df)[c(1,2)]<-c("State","Date")

mobility(Mob_df)
