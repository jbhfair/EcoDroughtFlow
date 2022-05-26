## code to compare headwater Q with downstream Q
# for EcoDrought DataViz dashboard

# Jenn Fair
# lasted edited: Mar 2022

# daily values

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