## code to compare headwater Q with downstream Q
# for EcoDrought DataViz dashboard

# Jenn Fair
# lasted edited: March 2022

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

# read in continuous data from pressure gages
wdDataIn <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataRelease/data/finishedData/20210930/VA/continuous")
setwd(wdDataIn)

mydata <- read_csv("EcoDrought Continuous_VA_20181022_to_20211027.csv")

#load UVA data

setwd("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataIn")
# Ref_gage <- read_csv("PA_10FL_hourly_flow.csv", 
#                                 col_types = cols(datetime = col_datetime(format = "%m/%d/%Y %H:%M"), 
#                                                  year = col_double()))

Ref_gage <- read_csv("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataIn/SR_10FL_hourly_flow.csv", 
                                col_types = cols(datetime = col_datetime(format = "%m/%d/%Y %H:%M")))



keepme<- c("StationID","cfs","datetime")
Ref_gage <- Ref_gage[keepme]
names(Ref_gage)[1:3] <- c("Site_ID","Discharge_Hobo_cfs","DateTime_EST")
Ref_gage$Station_No <- NA
Ref_gage$GageHeight_Hobo_ft <- NA
Ref_gage$WaterTemperature_HOBO_DegF <- NA
Ref_gage$AirPressure_PSI <- NA
Ref_gage$AirTemperature_HOBO_degF <- NA
#PA_10FL$DateTime_EST <- as.Date(PA_10FL$DateTime_EST, origin = '2020-02-01 00:00:00')

mydata <- rbind(mydata,Ref_gage)



# load site data 

wdDataIn <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataRelease/data/finishedData/20210930/VA/siteID")
setwd(wdDataIn)
                              
#load drainage area data
site_info <- read_excel("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataRelease/data/finishedData/20210930/VA/siteID/VA_site_info.xlsx")

site_info$DA_sqft <- site_info$Drainage_Area_sqmi*2.788e+7 # convert square miles to square feet
#create DA rank
site_info$DArank <- as.integer(rank(site_info$DA_sqft, ties.method="first"))

# include only Paine and/or Staunton focal sites

#sites2keep <- c("PA_01FL","PA_02FL","PA_06FL","PA_07FL","PA_10FL")
sites2keep <- c("SR_02FL","SR_03FL","SR_06FL","SR_07FL","SR_09FL","SR_10FL")
mydata = subset(mydata, Site_ID %in% sites2keep)
site_info = subset(site_info, Site_ID %in% sites2keep)

# save output directory

wdDataOut <- c("C:/Users/jfair/DOI/GS-LSC_EcoDrought - Documents/General/DataViz/Plots_EcoD_dataviz/DataOut")  
setwd(wdDataOut)

# compute monthly means

mydata$ymdate=ym(substr(mydata$DateTime_EST,1,7))
df=data.table(mydata)

mydata_stats<- 
  df %>%
  group_by(Site_ID,ymdate) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = list(mean = mean), na.rm = TRUE,
    #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
mydata_stats$ymdate <- substr(mydata_stats$ymdate,1,7)
mydata_stats <-mydata_stats[,1:5]
mydata_stats %>% add_column(DArank = NA)


for (i in 1:length(mydata_stats$Station_No)){
  mydata_stats$DArank[i] <- site_info$DArank[which(site_info$Station_No == mydata_stats$Station_No[i])]
  
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

write.csv(mydata_stats,file = "ShenPaine_monthly_mean_cfs.csv", row.names = FALSE)
write.csv(mydata_stats_cms,file = "ShenPaine_monthly_mean_cms.csv", row.names = FALSE)

### plot Qcfs

mydata_stats$Site_ID <- as.factor(mydata_stats$Site_ID)
#mydata_stats$Site_ID <- factor(mydata_stats$Site_ID, levels= c("PA_01FL","PA_02FL","PA_06FL","PA_07FL","PA_08FL","SR_02FL","SR_03FL","SR_06FL","SR_07FL","SR_09FL"))
#mydata_stats$Site_ID <- factor(mydata_stats$Site_ID, levels= c("PA_01FL","PA_02FL","PA_06FL","PA_07FL","PA_10FL"))
mydata_stats$Site_ID <- factor(mydata_stats$Site_ID, levels= c("SR_02FL","SR_03FL","SR_06FL","SR_07FL","SR_09FL","SR_10FL"))

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#490092","#999999")

month_mean_cfs_plot <- ggplot(data=mydata_stats %>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean, group=Site_ID, color=Site_ID))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="Shenandoah NP Nested Sites")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  #geom_point(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean,shape="8"),show.legend = F)+
  #geom_line(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean),size=1.25,show.legend = F)+
  scale_shape_manual(values = 8)+
  ylab('Mean Monthly Discharge, cfs')

month_mean_cfs_plot_log <- ggplot(data=mydata_stats %>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean, group=Site_ID, color=Site_ID))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="Shendandoah NP Nested Sites")+
  #scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x= element_blank())+
  #geom_point(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02"),aes(x=ymdate,y=Discharge_Hobo_cfs_mean,shape="8"),show.legend = F)+
  #geom_line(data=mydata_stats %>% filter(Site_Name == "West Brook 0" & ymdate > "2020-02" ),aes(x=ymdate,y=Discharge_Hobo_cfs_mean),size=1.25,show.legend = F)+
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
  df2 <- subset(df, select=c('Site_ID','DateTime_EST','Discharge_Hobo_cfs','ymdate'))
  df2$Q_vol_ft3=df2$Discharge_Hobo_cfs*30*60
  df2$Q_vol_ft3[df2$Site_ID == "SR_10FL"] <- (df2$Q_vol_ft3[df2$Site_ID == "SR_10FL"])*2
  df2 <- subset(df2,select = c('Site_ID','DateTime_EST','ymdate','Q_vol_ft3'))
  df2=data.table(df2)
  
  mydata_yield<- 
    df2 %>%
    group_by(Site_ID,ymdate) %>%
    summarize(across(
      .cols = where(is.numeric),
      .fns = sum, na.rm = TRUE,
      #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  mydata_yield$ymdate <- substr(mydata_yield$ymdate,1,7)
  
  
  
  for (i in 1:length(mydata_yield$Site_ID)){
    mydata_yield$DA_sqft[i] <- site_info$DA_sqft[which(site_info$Site_ID == mydata_yield$Site_ID[i])]
    
  }
  
  mydata_yield$runoff_ft <-mydata_yield$Q_vol_ft3_1/mydata_yield$DA_sqft
  
  write.csv(mydata_yield,file = "Shen_monthly_yield_ft.csv", row.names = FALSE)
}
### plot normalized yield

mydata_yield$Site_ID <- as.factor(mydata_yield$Site_ID)
#mydata_stats$Site_ID <- factor(mydata_stats$Site_ID, levels= c("PA_01FL","PA_02FL","PA_06FL","PA_07FL","PA_08FL","SR_02FL","SR_03FL","SR_06FL","SR_07FL","SR_09FL"))
mydata_stats$Site_ID <- factor(mydata_stats$Site_ID, levels= c("SR_02FL","SR_03FL","SR_06FL","SR_07FL","SR_09FL","SR_10FL"))

cbp2 <- c( "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#000000","#D55E00", "#CC79A7","#490092","#999999")

month_yield_ft_plot <- ggplot(data=mydata_yield %>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=runoff_ft, group=Site_ID, color=Site_ID))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbp2,name="Shenandoah NP Nested Stites")+
  theme_bw()+
  ylab("Runoff, ft, log scale")+
  geom_point(data=mydata_yield %>% filter(Site_ID == "SR_10FL" & ymdate > "2020-02" & ymdate < "2021-10" ),aes(x=ymdate,y=runoff_ft,shape="8"),show.legend = F)+
  geom_line(data=mydata_yield %>% filter(Site_ID == "SR_10FL" & ymdate > "2020-02" & ymdate < "2021-10"  ),aes(x=ymdate,y=runoff_ft),size=1.25,show.legend = F)+
  scale_shape_manual(values = 8)+
  theme(axis.text.x =element_blank(),
        axis.title.x=element_blank())+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #       axis.title.x=element_blank())+
  scale_y_log10()



# compute coefficient of variation of yield across sites per month

mydata_cv <-
  mydata_yield %>%
  group_by(ymdate) %>%
  summarise(monthly_cv = cv(runoff_ft))
#mydata_cv$ymdate <- substr(mydata_cv$ymdate,1,7)

month_cv_plot <- ggplot(data=mydata_cv%>% filter(ymdate > "2020-02" & ymdate <"2021-10"),aes(x=ymdate,y=monthly_cv,group=1))+
  geom_point()+
  geom_line()+
  #scale_colour_manual(values=cbp2)+
  theme_bw()+
  ylab('Coefficient of Variation, %')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_blank())


yield_cv_plot <- ggarrange(month_yield_ft_plot,month_cv_plot, 
                           #labels = c("A", "B", "C"),
                           ncol = 1, nrow = 2, legend="none",align = "v")
annotate_figure(yield_cv_plot, top = text_grob("Shenandoah NP, Staunton Catchment", 
                                      color = "black", face = "bold", size = 14))

yield_cv_plot

#z-score
{
  mydata_zscore <-
    mydata_yield %>%
    group_by(ymdate) %>%
    mutate(zscore = (runoff_ft-mean(runoff_ft))/sd(runoff_ft))
  
  month_zscore <- ggplot(data=mydata_zscore %>% filter(ymdate > "2020-02" & ymdate < "2021-10"),aes(x=ymdate,y=zscore, group=Site_ID, color=Site_ID))+
    geom_point()+
    geom_line()+
    scale_colour_manual(values=cbp2,name="Shen Staunton nested Sites")+
    theme_bw()+
    ylab("z-score")+
    geom_point(data=mydata_zscore %>% filter(Site_ID == "SR_10FL" & ymdate > "2020-02" & ymdate < "2021-10"),aes(x=ymdate,y=zscore,shape="8"),show.legend = F)+
    geom_line(data=mydata_zscore %>% filter(Site_ID == "SR_10FL" & ymdate > "2020-02" & ymdate < "2021-10" ),aes(x=ymdate,y=zscore),size=1.25,show.legend = F)+
    scale_shape_manual(values = 8)+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank())+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x=element_blank())+
    theme(legend.position="none")
  
  annotate_figure(month_zscore, top = text_grob("Shen-Staunton", 
                                                color = "black", face = "bold", size = 14))
  
}
