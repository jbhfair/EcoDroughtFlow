library(dataRetrieval)

#siteListQT <- whatNWISsites(stateCd="MA",siteStatus= "active",siteType = "ST",parameterCd=c("00060","00010"),service="dv")
siteListQT <- whatNWISsites(stateCd="MA",siteStatus= "active",siteType = "ST",parameterCd="00010",service="dv")

thisSite = "01171005" # 
INFO = readNWISsite(thisSite) #site metadata
endDate = "2022-01-31"
startDate = "2015-11-01"



#this reads in the daily mean streamflow, cfs, and daily mean temp, deg C 
DF = readNWISdv(thisSite,c("00060","00010"),statCd = "00003", startDate= startDate,endDate=endDate)
#DF = readNWISdv(thisSite,c("00060","00010"),statCd = "00003", startDate= startDate,endDate=endDate)



DFnames <- c("agency","site_no","date_YYYY-MM-dd","temp_degC","temp_qualifier","discharge_cfs","discharge_qualifier")
names(DF)<-DFnames