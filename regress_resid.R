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
library(matrixStats)
library(ggpubr)
library(readxl)
library(weathermetrics)
library(raster)

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

mydata$ymday=ymd(substr(mydata$DateTime_EST,1,10))
df=data.table(mydata)

## runoff calcs
df2 <- subset(df, select=c('Station_No','DateTime_EST','Discharge_Hobo_cfs','ymday'))
df2$Q_vol_ft3=df2$Discharge_Hobo_cfs*15*60
df2 <- subset(df2,select = c('Station_No','DateTime_EST','ymday','Q_vol_ft3'))
df2=data.table(df2)

mydata_daily<- 
  df2 %>%
  group_by(Station_No,ymday) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = sum, na.rm = TRUE,
    #.fns = list(mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

for (i in 1:length(mydata_daily$Station_No)){
  mydata_daily$DA_sqft[i] <- site_info$DA_sqft[which(site_info$Station_No == mydata_daily$Station_No[i])]
  mydata_daily$Site_Name[i] <- site_info$Site_Name[which(site_info$Station_No == mydata_daily$Station_No[i])]
}

mydata_daily$runoff_in <- (mydata_daily$Q_vol_ft3_1/mydata_daily$DA_sqft)*12

mydata_daily$Site_Name <- as.factor(mydata_daily$Site_Name)
mydata_daily$Site_Name <- factor(mydata_daily$Site_Name, levels= c("West Brook 0","West Brook Lower","West Brook Upper","West Brook Reservoir","Avery Brook","Sanderson Brook","Jimmy Brook","West Whately Brook","Obear Brook Lower","Mitchell Brook"))


## plot regression

mydata_daily$better_names <- make.names(mydata_daily$Site_Name)

mydata_daily$month_only <- month(mydata_daily$ymday)

#mydata_daily <- mydata_daily %>% filter(ymday > "2020-02-29",month_only==c(6,7,8,9,10))
mydata_daily <- mydata_daily %>% filter(ymday > "2020-02-29")
                                        
mydata_daily_test <- subset(mydata_daily,select=c('better_names','runoff_in'))
mydata_daily_test$better_names[mydata_daily_test$better_names == 'West_Brook 0'] <- 'West_Brook_0'

mydata_daily_wide2 <- 
  mydata_daily_test %>% 
  group_by(better_names) %>%
  mutate(row=row_number()) %>%
  pivot_wider(names_from = c(better_names),values_from=runoff_in,values_fn=list) 

mydata_daily_wide <- unnest(mydata_daily_wide2,cols = c(Avery.Brook, West.Whately.Brook, Sanderson.Brook, West.Brook.Reservoir, 
                                                        West.Brook.Upper, Jimmy.Brook, Obear.Brook.Lower, Mitchell.Brook, 
                                                        West.Brook.Lower, West.Brook.0))


WB0_WB1_regress <- ggplot(data=mydata_daily_wide,aes(x=West.Brook.0,y=West.Brook.Lower))+
                  geom_point()+
                  geom_smooth(method='lm')+
                  scale_y_log10()+
                  scale_x_log10()+
                  geom_abline(intercept=0,slope=1,size=1.0)+
                  stat_regline_equation(label.y = 0.001, aes(label = ..rr.label..))+
                  theme_bw()

WB0_WB2_regress <- ggplot(data=mydata_daily_wide,aes(x=West.Brook.0,y=West.Brook.Upper))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_log10()+
  scale_x_log10()+
  geom_abline(intercept=0,slope=1,size=1.0)+
  stat_regline_equation(label.y = 0.001, aes(label = ..rr.label..))+
  theme_bw()

options(scipen=1000)
WB0_WB3_regress <- ggplot(data=mydata_daily_wide,aes(x=West.Brook.0,y=West.Brook.Reservoir))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_log10()+
  scale_x_log10()+
  geom_abline(intercept=0,slope=1,size=1.0)+
  stat_regline_equation(label.y = 0.00001, aes(label = ..rr.label..))+
  theme_bw()

WB0_Avery_regress <- ggplot(data=mydata_daily_wide,aes(x=West.Brook.0,y=Avery.Brook))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_log10()+
  scale_x_log10()+
  geom_abline(intercept=0,slope=1,size=1.0)+
  stat_regline_equation(label.y = 0.001, aes(label = ..rr.label..))+
  theme_bw()




regress_plot <- ggarrange(WB0_WB1_regress,WB0_WB2_regress,WB0_WB3_regress,WB0_Avery_regress, 
                           labels = c("A", "B", "C","D"),
                           ncol = 2, nrow = 2, legend="none",align = "v")

regress_plot




