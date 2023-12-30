########################################
# mediation analysis
########################################
# Load packages
library(psych)
library(lavaan)
library(ggplot2)
library(readxl)
library(semPlot)

library(USvac)
data(extdata)

State_name<-c("Alabama",              "Alaska",               "Arizona",              "Arkansas",             "California",           "Colorado",            
              "Connecticut",          "Delaware",             "District of Columbia", "Florida",              "Georgia",              "Hawaii",              
              "Idaho",                "Illinois",             "Indiana",              "Iowa",                 "Kansas",               "Kentucky",            
              "Louisiana",            "Maine",                "Maryland",             "Massachusetts",        "Michigan",             "Minnesota",          
              "Mississippi",          "Missouri",             "Montana",              "Nebraska",             "Nevada",               "New Hampshire",       
              "New Jersey",           "New Mexico",           "New York",             "North Carolina",       "North Dakota",         "Ohio",                
              "Oklahoma",             "Oregon",               "Pennsylvania",         "Rhode Island",         "South Carolina",       "South Dakota",        
              "Tennessee",            "Texas",                "Utah",                 "Vermont",              "Virginia",             "Washington",          
              "West Virginia",        "Wisconsin",            "Wyoming" )

# mutation
all_mutation_df<-data.frame(Date    =NA,
                            State   =NA,
                            onlyBA1 =NA,
                            bothBA12=NA,
                            onlyBA2 =NA)


onlyBA1<-c(2,7,12)
bothBA12<-c(1,3:6,8:11,13:15)
onlyBA2<-c(16:19)

for (i in 1:51) {
  State<-State_name[i]
  load_file<-paste(".../sub_mut_freq_output_MA/",State,"_alg_sub_mut_freq.csv",sep = "",collapse="") 
  state_mutation<-read.csv(load_file)[,-1];ncol(state_mutation)
  colnames(state_mutation)<-c("G339D", "S371L", "S373P", "S375F", "K417N", "N440K", "G446S",
                              "S477N", "T478K", "E484A", "Q493R", "G496S", "Q498R", "N501Y",
                              "Y505H", "S371F", "T376A", "R408S", "D405N", "Date")
  
  if (i==42) {
    state_mutation$Date<-as.Date(state_mutation$Date,"%m/%d/%Y")
  } else state_mutation$Date<-as.Date(state_mutation$Date)
  
  state_mutation1<-state_mutation[which(state_mutation$Date>as.Date("2021-12-10") & state_mutation$Date<as.Date("2022-03-23")),]
  state_mutation2<-state_mutation1[,onlyBA1] 
  state_mutation2<-apply(state_mutation2,1,mean,na.rm=T)  
  
  state_mutation3<-state_mutation1[,bothBA12] 
  state_mutation3<-apply(state_mutation3,1,mean,na.rm=T)  
  
  state_mutation4<-state_mutation1[,onlyBA2] 
  state_mutation4<-apply(state_mutation4,1,mean,na.rm=T)  
  
  state_df<-data.frame(Date    =state_mutation1$Date,
                       State   =rep(State,102),
                       onlyBA1 =as.vector(state_mutation2),
                       bothBA12=as.vector(state_mutation3),
                       onlyBA2 =as.vector(state_mutation4))
  
  all_mutation_df<-rbind(all_mutation_df,state_df)
}
all_mutation_df$Date<-as.Date(all_mutation_df$Date,origin="1970-01-01")
extdata$Date<-as.Date(extdata$Date,"%Y/%m/%d")

df_mutation<-merge(extdata,all_mutation_df[-1,],by=c("Date","State"),all=T)
df_mutation<-df_mutation[order(df_mutation$State,df_mutation$Date),]

####---only BA1 model -----------------------------------------------
end_date<-as.Date("2022-02-01",origin="1970-01-01")
df_mutation0<-df_mutation[which(df_mutation$Date<end_date),]
df_mutation0$onlyBA1<-df_mutation0$onlyBA1*100
df_mutation0$Daily_hospital_Admission_pct1<-df_mutation0$Daily_hospital_Admission_pct1*100
mediation_model <- '
  # Direct effects
  onlyBA1 ~ a * Adjust_Doses_Vax_Pct_7
  Daily_hospital_Admission_pct1 ~ c * Adjust_Doses_Vax_Pct_7 + b * onlyBA1 + log_Cases_MA + inpatient_beds

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect
'

dim(mediation_model)

# Estimate the mediation model
mediation_results <- sem(mediation_model, data = df_mutation0)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)

####---bothBA12 model -----------------------------------------------
end_date<-as.Date("2022-02-01",origin="1970-01-01")
df_mutation2<-df_mutation[which(df_mutation$Date<end_date),]
df_mutation2$bothBA12<-df_mutation2$bothBA12*100
df_mutation2$Daily_hospital_Admission_pct1<-df_mutation2$Daily_hospital_Admission_pct1*100
mediation_model <- '
  # Direct effects
  bothBA12 ~ a * Adjust_Doses_Vax_Pct_7
  Daily_hospital_Admission_pct1 ~ c * Adjust_Doses_Vax_Pct_7 + b * bothBA12 + log_Cases_MA + inpatient_beds

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect
'

dim(mediation_model)

# Estimate the mediation model
mediation_results <- sem(mediation_model, data = df_mutation2)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)

####--- onlyB2 model-----------------------------------------------
df_mutation1<-df_mutation[which(df_mutation$Date>as.Date("2021-11-15")),]
df_mutation1$onlyBA2<-df_mutation1$onlyBA2*100
df_mutation1$Daily_hospital_Admission_pct1<-df_mutation1$Daily_hospital_Admission_pct1*100
#library(fastDummies)
#df_mutation12<-dummy_cols(df_mutation1, select_columns = "State")
#colnames(df_mutation12)[26:76]<-c("State_Alabama",                  
#                             "State_Alaska",                    "State_Arizona",                  
#                             "State_Arkansas",                  "State_California",               
#                             "State_Colorado",                  "State_Connecticut",              
#                             "State_Delaware",                  "State_District_of_Columbia",     
#                             "State_Florida",                   "State_Georgia",                  
#                             "State_Hawaii",                    "State_Idaho",                    
#                             "State_Illinois",                  "State_Indiana",                  
#                             "State_Iowa",                      "State_Kansas",                   
#                             "State_Kentucky",                  "State_Louisiana",                
#                             "State_Maine",                     "State_Maryland",                 
#                             "State_Massachusetts",             "State_Michigan",                 
#                             "State_Minnesota",                 "State_Mississippi",              
#                             "State_Missouri",                  "State_Montana",                  
#                             "State_Nebraska",                  "State_Nevada",                   
#                             "State_New_Hampshire",             "State_New_Jersey",               
#                             "State_New_Mexico",                "State_New_York",                 
#                             "State_North_Carolina",            "State_North_Dakota",             
#                             "State_Ohio",                      "State_Oklahoma",                 
#                             "State_Oregon",                    "State_Pennsylvania",             
#                             "State_Rhode_Island",              "State_South_Carolina",           
#                             "State_South_Dakota",              "State_Tennessee",                
#                             "State_Texas",                     "State_Utah",                     
#                             "State_Vermont",                   "State_Virginia",                 
#                             "State_Washington",                "State_West_Virginia",            
#                             "State_Wisconsin",                 "State_Wyoming" )

#mediation_model <- '
#  # Direct effects
#  onlyBA2 ~ a * Adjust_Doses_Vax_Pct_7
#  Daily_hospital_Admission_pct1 ~ c * Adjust_Doses_Vax_Pct_7 + b * onlyBA2 + 
#                                  State_Alabama + State_Alaska + State_Arizona + State_Arkansas + State_California + State_Colorado +
#                                  State_Connecticut + State_Delaware + State_District_of_Columbia + State_Florida + State_Georgia + State_Hawaii +
#                                  State_Idaho + State_Illinois + State_Indiana + State_Iowa + State_Kansas + State_Kentucky + State_Louisiana +
#                                  State_Maine + State_Maryland + State_Massachusetts + State_Michigan + State_Minnesota + State_Mississippi +
#                                  State_Missouri + State_Montana + State_Nebraska + State_Nevada + State_New_Hampshire + State_New_Jersey + State_New_Jersey +
#                                  State_New_Mexico + State_New_York + State_North_Carolina + State_North_Dakota + State_Ohio + State_Ohio + 
#                                  State_Oregon + State_Pennsylvania + State_Rhode_Island + State_South_Carolina + State_South_Dakota +
#                                  State_Tennessee + State_Texas + State_Utah + State_Vermont + State_Virginia + State_Washington + State_West_Virginia +
#                                  State_Wisconsin + State_Wyoming
#  # Indirect effect (a * b)
#  indirect := a * b

#  # Total effect (c + indirect)
#  total := c + indirect
#'

mediation_model <- '
  # Direct effects
  onlyBA2 ~ a * Adjust_Doses_Vax_Pct_7
  Daily_hospital_Admission_pct1 ~ c * Adjust_Doses_Vax_Pct_7 + b * onlyBA2 + log_Cases_MA + inpatient_beds
  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect
'

dim(mediation_model)

# Estimate the mediation model
mediation_results <- sem(mediation_model, data = df_mutation1)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)
