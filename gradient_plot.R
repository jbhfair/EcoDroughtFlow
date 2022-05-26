#load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dataRetrieval)
library(imputeTS)
library(tibbletime)
library(readr)
library(data.table)
library(tidyverse)
library(dataRetrieval)
library(matrixStats)
library(ggpubr)
library(readxl)
library(weathermetrics)
library(raster)
library(grid)
library(gridSVG)
library(ggplot2)
library(reshape2)
library(ggforce)

wdDataOut <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataOut")  
setwd(wdDataOut)

yield_data <- read_csv("WestBrook_monthly_yield_ft.csv", 
              col_types = cols(ymdate = col_date(format = "%Y-%m")))

yield_data$ymdate <- substr(yield_data$ymdate,1,7) 


# create curves for max and min
{  
  # find max and min of monthly yields
  
  
  yield_dt <- data.table(yield_data)
  
  yield_max_min<- 
    yield_dt %>%
    group_by(ymdate) %>%
    summarize(across(
      #.cols = mydata_yield$runoff_ft,na.rm = TRUE,
      .cols = where(is.numeric),
      .fns = list(max = max, min = min), na.rm = TRUE,
      #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  
  yield_max_min %>% filter(ymdate > "2020-02") 
  keep <-c("ymdate","runoff_ft_max","runoff_ft_min")
  yield_max_min <- yield_max_min[keep]
  yield_WB0 <- yield_data %>% filter(Site_Name == "West Brook 0")
  yield_range <- left_join(yield_max_min,yield_WB0,by="ymdate")
  names(yield_range)[names(yield_range) == 'runoff_ft'] <- 'runoff_ft_WBO'
  keep <-c("ymdate","runoff_ft_max","runoff_ft_min","runoff_ft_WBO")
  yield_range <- yield_range[keep]
  
  #add all sites
  
  
  
}
# ribbon plot


yield_range_long <- melt(yield_range, id.vars="ymdate")

ggplot(data = yield_range_long, aes(x=ymdate,y=value, group=variable)) + 
  geom_line()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())

yield_plot <- yield_range %>% filter(ymdate > "2020-02")  


yield_plot%>%
  ggplot(aes(ymdate,runoff_ft_WBO))+
  geom_ribbon(aes(ymin=runoff_ft_min, ymax=runoff_ft_WBO),group=1,fill="red",alpha=0.4)+
  geom_ribbon(aes(ymin=runoff_ft_WBO, ymax=runoff_ft_max),group=1,fill="blue",alpha=0.4)+
  geom_line(aes(x=ymdate, y=runoff_ft_WBO), group=1, color = "black", size = 1) + 
  geom_line(aes(x=ymdate, y=runoff_ft_min), group=1, color = "red", size = 0.1) +
  geom_line(aes(x=ymdate, y=runoff_ft_max), group=1, color = "blue", size = 0.1) +
  scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  ylab('runoff,ft, log scale')
  
  set.seed(123)
  pal <- c("red","blue")
  yield_plot2<-yield_data %>% filter(ymdate > "2020-02")
  yield_plot3 <- yield_plot2 %>% filter(Site_Name == "West Whately Brook" | Site_Name == "Jimmy Brook" | Site_Name == "Sanderson Brook")
  yield_plot4 <- yield_plot2 %>% filter(Site_Name == "West Brook Reservoir" | Site_Name == "West Brook Upper")
  
  
  #option1
  ggplot(data=NULL,aes(ymdate,runoff_ft_WBO))+
  geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_min, ymax=runoff_ft_WBO),group=1,fill="grey",alpha=0.3)+
  geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_WBO, ymax=runoff_ft_max),group=1,fill="grey",alpha=0.3)+
  
  geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_min), group=1, color = "grey", size = 0.1) +
  geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_max), group=1, color = "grey", size = 0.1) +
  scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  ylab('runoff,ft, log scale')+
  
  geom_line(data=yield_plot3 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="blue"))+
  geom_point(data=yield_plot3 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="blue"))+
  geom_line(data=yield_plot4 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="red"))+
  geom_point(data=yield_plot4 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="red"))+
  geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_WBO), group=1, color = "black", size = 1) + 
  
  #geom_link2(data=yield_plot2,aes(x=ymdate,y=runoff_ft, group=Site_Name, colour=after_stat(y<0.10)))+
  scale_colour_manual(values=c("blue","red","green"))+
  theme(legend.position = "none")
  
  #option2
  offset = 0.0001
  offset2 = 0.01
  ggplot(data=NULL,aes(ymdate,runoff_ft_WBO))+
    geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_min-offset, ymax=runoff_ft_WBO),group=1,fill="red",alpha=0.2)+
    geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_WBO, ymax=runoff_ft_max+offset2),group=1,fill="blue",alpha=0.2)+
    
   scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
    ylab('runoff,ft')+
    
    geom_line(data=yield_plot3 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="gray48"))+
    geom_point(data=yield_plot3 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="gray48"))+
    geom_line(data=yield_plot4 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="gray48"))+
    geom_point(data=yield_plot4 %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color="gray48"))+
    scale_colour_manual(values=c("gray48","gray48","green"))+
    
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_min-offset), group=1, color = "red", size = 0.1) +
    geom_point(data=yield_plot,aes(x=ymdate, y=runoff_ft_min-offset), group=1, color = "red") +
    
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_max+offset2), group=1, color = "blue", size = 0.1) +
    geom_point(data=yield_plot,aes(x=ymdate, y=runoff_ft_max+offset2), group=1, color = "blue") +
    
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_WBO), group=1, color = "black", size = 1) + 
    
    #geom_link2(data=yield_plot2,aes(x=ymdate,y=runoff_ft, group=Site_Name, colour=after_stat(y<0.10)))+
    
    theme(legend.position = "none")
  
  
  
  
  #another try
  
  ggplot(data=NULL,aes(ymdate,runoff_ft_WBO))+
    geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_min, ymax=runoff_ft_WBO),group=1,fill="grey",alpha=0.3)+
    geom_ribbon(data=yield_plot,aes(ymin=runoff_ft_WBO, ymax=runoff_ft_max),group=1,fill="grey",alpha=0.3)+
    
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_min), group=1, color = "grey", size = 0.1) +
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_max), group=1, color = "grey", size = 0.1) +
    scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
    ylab('runoff,ft, log scale')+
    
    #geom_line(data=yield_data %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color=ifelse(runoff_ft<yield_plot$runoff_ft_WBO,"blue","red")))+
    #geom_point(data=yield_data %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color=ifelse(runoff_ft<yield_plot$runoff_ft_WBO,"blue","red")))+
    geom_line(data=yield_plot,aes(x=ymdate, y=runoff_ft_WBO), group=1, color = "black", size = 1) + 
    
    ggplot(data=yield_plot2)+
    geom_link2(aes(x=ymdate,y=runoff_ft, group=Site_Name, color=after_stat(y<yield_plot2$runoff_ft[yield_plot2$Site_Name == "West Brook 0"])))+
    scale_colour_manual(values=c("red","blue","green"))+
    theme(legend.position = "none")

  

  
  

  
  
  
  
  
