library(ggplot2)
library(USvac)
data(extdata)

ggplot(cases_mob_df_S,aes(x=transit_stations_percent_change_from_baseline,y=cases_per1000000))+
  geom_smooth(color="#6CADDF", method="lm",se=T, formula = y~x)+
  geom_point(size=2,color="#6CADDF",alpha=0.5)+
  labs(x="Mean mobility (%)", y="Cumulative reported cases per million",
       title="")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, size=12), axis.title = element_text(size=12))
