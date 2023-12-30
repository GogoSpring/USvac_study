# Fig S13A
library(USvac)
data(cases_chr_df1_S)

ggplot(cases_chr_df1_S,aes(x=log_mean_cases,y=mean_chr))+
  #geom_smooth(color="#F27077", method="lm",se=T, formula = y~x)+
  geom_point(size=2,color="#F27077",alpha=0.5)+
  labs(x="Daily average cases (logtransformed)", y="Daily average CHR (%)",
       title="")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, size=12), axis.title = element_text(size=12))
