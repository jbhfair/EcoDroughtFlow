## code to compare headwater Q with downstream Q
# for EcoDrought DataViz dashboard

# Jenn Fair
# lasted edited: Feb 2022

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

# set working directory
wdDataIn <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataIn")
setwd(wdDataIn)

# load EcoD continuous data from data release (draft 2022-02-02)

mydata <- read_csv("EcoDrought Continuous_MA_20200101_to_20210930.csv", 
                           col_types = cols(AirPressure_PSI = col_double(), 
                                            AirTemperature_HOBO_degF = col_double(), 
                                            DateTime_EST = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                            Station_No = col_integer()))


#load drainage area data
site_info <- read_excel("MA_site_info_20220210.xlsx", 
                                col_types = c("numeric", "text", "text", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))
site_info$DA_sqft <- site_info$Drainage_Area_sqmi*2.788e+7 # convert square miles to square feet
#create DA rank
site_info$DArank <- as.integer(rank(site_info$DA_sqft, ties.method="first"))

wdDataOut <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataOut")  
setwd(wdDataOut)

# compute monthly means
mydata$ymdate=ym(substr(mydata$DateTime_EST,1,7))
df=data.table(mydata)

mydata_stats<- 
  df %>%
  group_by(Station_No,ymdate) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = list(mean = mean), na.rm = TRUE,
    #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
mydata_stats$ymdate <- substr(mydata_stats$ymdate,1,7)
mydata_stats <-mydata_stats[,1:5]
mydata_stats %>% add_column(c(DArank = NA,Site_Name=NA))


for (i in 1:length(mydata_stats$Station_No)){
mydata_stats$DArank[i] <- site_info$DArank[which(site_info$Station_No == mydata_stats$Station_No[i])]
mydata_stats$Site_Name[i] <- site_info$Site_Name[which(site_info$DArank == mydata_stats$DArank[i])]
}
mydata_stats <-
  mydata_stats %>%
  arrange(desc(DArank),ymdate)

mydata_stats_cms <-data.frame(matrix(NA,nrow=nrow(mydata_stats),ncol = ncol(mydata_stats))) 

mydata_stats_cms <- mydata_stats
cfs_to_cms <- data.frame(matrix(0.0283,nrow=nrow(mydata_stats), ncol=1))
ft_to_m <- data.frame(matrix(0.3048,nrow=nrow(mydata_stats), ncol=1))
mydata_stats_cms[,3] <- mydata_stats[,3]*ft_to_m
mydata_stats_cms[,4] <- mydata_stats[,4]*cfs_to_cms
mydata_stats_cms[,5] <- fahrenheit.to.celsius(mydata_stats[,5], round = 2)
colnames(mydata_stats_cms)[4] <- "Discharge_HOBO_cms_mean"
colnames(mydata_stats_cms)[5] <- "WaterTemperature_HOBO_degC_mean"

write.csv(mydata_stats,file = "WestBrook_monthly_mean_cfs.csv", row.names = FALSE)
write.csv(mydata_stats_cms,file = "WestBrook_monthly_mean_cms.csv", row.names = FALSE)

### plot Qcfs

mydata_stats$Site_Name <- as.factor(mydata_stats$Site_Name)
mydata_stats$Site_Name <- factor(mydata_stats$Site_Name, levels= c("West Brook 0","West Brook Lower","West Brook Upper","West Brook Reservoir","Avery Brook","Sanderson Brook","Jimmy Brook","West Whately Brook","Obear Brook Lower","Mitchell Brook"))

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#490092","#999999")

month_mean_cfs_plot <- ggplot(data=mydata_stats %>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean, group=Site_Name, color=Site_Name))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="West Brook Nested Sites")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  geom_point(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean,shape="8"),show.legend = F)+
  geom_line(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean),size=1.25,show.legend = F)+
  scale_shape_manual(values = 8)+
  ylab('Mean Monthly Discharge, cfs')

month_mean_cfs_plot_log <- ggplot(data=mydata_stats %>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean, group=Site_Name, color=Site_Name))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="West Brook Nested Sites")+
  #scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  geom_point(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean,shape="8"),show.legend = F)+
  geom_line(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean),size=1.25,show.legend = F)+
  scale_shape_manual(values = 8)+
  ylab('Mean Monthly Discharge, cfs, log scale')+
  scale_y_log10()


#monthly variance
{
# ###  compute monthly variance
# mydata$ymdate=ym(substr(mydata$TimeStamp,1,7))
# df=data.table(mydata)
# 
# mydata_stats_var<- 
#   df %>%
#   group_by(Station_No,ymdate) %>%
#   summarize(across(
#     .cols = where(is.numeric),
#     .fns = list(variance = var), na.rm = TRUE,
#     #.fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
#     .names = "{col}_{fn}"
#   ))
# mydata_stats_var$ymdate <- substr(mydata_stats_var$ymdate,1,7)
# mydata_stats_var <-mydata_stats_var[,1:5]
# mydata_stats_var %>% add_column(c(DArank = NA,Site_Name=NA))
# 
# 
# for (i in 1:length(mydata_stats_var$Station_No)){
#   mydata_stats_var$DArank[i] <- site_info$DArank[which(site_info$Station_No == mydata_stats_var$Station_No[i])]
#   mydata_stats_var$Site_Name[i] <- site_info$Site_Name[which(site_info$DArank == mydata_stats_var$DArank[i])]
# }
# mydata_stats_var <-
#   mydata_stats_var %>%
#   arrange(desc(DArank),ymdate)
# 
# write.csv(mydata_stats_var,file = "WestBrook_monthly_mean_var.csv", row.names = FALSE)
# 
# ### plot variance, not meaningful since not normalized
# cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#           "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#490092","#999999")
# 
# month_mean_var_plot <- ggplot(data=mydata_stats_var,aes(x=ymdate,y=Discharge_Hobo_cfs_variance, group=Site_Name, color=Site_Name))+
#   geom_point()+
#   geom_line()+
#   scale_colour_manual(values=cbp2)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### compute yields
  }

### YIELD (flow normalized by drainage area) ###
{
df2 <- subset(df, select=c('Station_No','DateTime_EST','Discharge_Hobo_cfs','ymdate'))
  ## convert from instantaneous to ft^3/15min, so you can sum to get cumulative flow
df2$Q_vol_ft3=df2$Discharge_Hobo_cfs*15*60

df2 <- subset(df2,select = c('Station_No','DateTime_EST','ymdate','Q_vol_ft3'))
df2=data.table(df2)

mydata_yield<- 
  df2 %>%
  group_by(Station_No,ymdate) %>% # this sums by site by month
  summarize(across(
    .cols = where(is.numeric),
    .fns = sum, na.rm = TRUE,
    #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
mydata_yield$ymdate <- substr(mydata_yield$ymdate,1,7)



for (i in 1:length(mydata_yield$Station_No)){
  mydata_yield$DA_sqft[i] <- site_info$DA_sqft[which(site_info$Station_No == mydata_yield$Station_No[i])]
  mydata_yield$Site_Name[i] <- site_info$Site_Name[which(site_info$Station_No == mydata_yield$Station_No[i])]
}

mydata_yield$runoff_ft <-mydata_yield$Q_vol_ft3_1/mydata_yield$DA_sqft

write.csv(mydata_yield,file = "WestBrook_monthly_yield_ft.csv", row.names = FALSE)
}
### plot normalized yield

mydata_yield$Site_Name <- as.factor(mydata_yield$Site_Name)
mydata_yield$Site_Name <- factor(mydata_yield$Site_Name, levels= c("West Brook 0","West Brook Lower","West Brook Upper","West Brook Reservoir","Avery Brook","Sanderson Brook","Jimmy Brook","West Whately Brook","Obear Brook Lower","Mitchell Brook"))

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#490092","#999999")

month_yield_ft_plot <- ggplot(data=mydata_yield %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=runoff_ft, group=Site_Name, color=Site_Name))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="West Brook Nested Stites")+
  theme_bw()+
  ylab("Runoff, ft")+
  geom_point(data=mydata_yield %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=runoff_ft,shape="8"),show.legend = F)+
  geom_line(data=mydata_yield %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=runoff_ft),size=1.25,show.legend = F)+
  scale_shape_manual(values = 8)+
  theme(axis.text.x =element_blank(),
        axis.title.x=element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_blank())+
  theme(legend.position="none")+
  scale_y_log10()



# compute coefficient of variation of yield across sites per month

mydata_cv <-
  mydata_yield %>%
  group_by(ymdate) %>%
  summarise(monthly_cv = cv(runoff_ft))
#mydata_cv$ymdate <- substr(mydata_cv$ymdate,1,7)



month_cv_plot <- ggplot(data=mydata_cv%>% filter(ymdate > "2020-02"),aes(x=ymdate,y=monthly_cv,group=1))+
  geom_point()+
  geom_line()+
  #scale_colour_manual(values=cbp2)+
  theme_bw()+
  ylab('Coefficient of Variation, %')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_blank())


yield_cv_plot <- ggarrange(month_yield_ft_plot,month_cv_plot, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 2, legend="none",align = "v")

annotate_figure(yield_cv_plot, top = text_grob("West Brook", 
                                               color = "black", face = "bold", size = 14))

#z-score
{
  mydata_zscore <-
    mydata_yield %>%
    group_by(ymdate) %>%
    mutate(zscore = (runoff_ft-mean(runoff_ft))/sd(runoff_ft))
  
  month_zscore <- ggplot(data=mydata_zscore %>% filter(ymdate > "2020-02"),aes(x=ymdate,y=zscore, group=Site_Name, color=Site_Name))+
    geom_point()+
    geom_line()+
    scale_colour_manual(values=cbp2,name="West Brook Nested Sites")+
    theme_bw()+
    ylab("z-score")+
    geom_point(data=mydata_zscore %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=zscore,shape="8"),show.legend = F)+
    geom_line(data=mydata_zscore %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=zscore),size=1.25,show.legend = F)+
    scale_shape_manual(values = 8)+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank())+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x=element_blank())+
    theme(legend.position="none")

  annotate_figure(month_zscore, top = text_grob("West Brook", 
                                                 color = "black", face = "bold", size = 14))
  
}

