setwd(".../US/")

# plot the map
library(usmap)
library(ggplot2)
library(scatterpie)
library(RColorBrewer)

# 1a. omicron map vaccine coverage -----------------------------------------------------------------------
df_omicron_extend_cum1<-read.csv("data/df_omicron_extend_cum1.csv")
#figure_file<-paste("..../Figure 1a.pdf")
#pdf(figure_file, height =4 , width = 5*1.3)
p <- plot_usmap(data=df_omicron_extend_cum1,
                values ="vac_coverage",
                labels=T,
                label_color = "black") +
  scale_fill_stepsn(breaks=c(as.numeric(quantile(df_omicron_extend_cum1$vac_coverage,seq(.25,1,.25)))),
                    colors=c("white","#99CC00","#339965","#013300")) + 
  labs(title = "COVID-19 vaccine coverage, United States",
       subtitle = "Omicron wave (Dec 20,2021 - Mar 22,2022)") 

p$layers[[2]]$aes_params$size <- 2.5
print(p)
#dev.off()

# 1b. omicron map hospitalization -----------------------------------------------------------------------
#figure_file<-paste(".../Figure 1b.pdf")
#pdf(figure_file, height =4 , width = 5*1.3)
p <- plot_usmap(data=df_omicron_extend_cum1,
                values ="hos_per1000000",
                labels=T,
                label_color = "black") +
  scale_fill_stepsn(breaks=c(as.numeric(quantile(df_omicron_extend_cum1$hos_per1000000,seq(.25,1,.25)))),
                    colors=c("white","#F0F9E8","#3684C0","#075099")) + 
  labs(title = "Cumulative COVID-19 hospital admissions, United States",
       subtitle = "Omicron wave (Dec 20,2021 - Mar 22,2022)") 

p$layers[[2]]$aes_params$size <- 2.5
print(p)
#dev.off()

# 1c. lineage proportion -----------------------------------------------------------------------
lineage_df_omicron<-read.csv("data/Figure 1c.csv")
lineage_df_omicron$order_variant <- as.character(lineage_df_omicron$order_variant)
#figure_file<-paste(".../Lineage.pdf")
#pdf(figure_file, height =5 , width = 5*1.3)
ggplot(lineage_df_omicron, aes(x = week_ending, y = share, fill = order_variant)) +
  geom_col(position = 'fill')+
  
  scale_fill_manual(values = c("#AE76B2", "#6CADDF", "#F27077", "#9DD374",
                               "#FAB26A"),
                    labels = c("B.1.617.2", "BA.1", "BA.1.1", "BA.2",
                               "Others")) +
  
  labs(x = "",y = "% Virual Lineages Among Infections")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  
  scale_x_continuous(breaks=lineage_df_omicron$week_ending) +
  theme(panel.background=element_rect(fill="white",colour="black",size=0.25), 
        
        axis.line=element_line(colour="black",size=0.25), 
        
        axis.title=element_text(size=13,color="black"), 
        
        axis.text = element_text(size=12,color="black"), 
        
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5)
  )
#dev.off()
