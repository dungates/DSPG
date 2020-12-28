library(lubridate)
library(readr)
#dataframe for fishdata
fishdata <- read_csv("./data/AllFishData.csv")
fishdata$month_numeric <- month(fishdata$Date_time)
fishdata_SF <- read_csv("./data/AllSteelheadData.csv")

#dataframe for locations
locations_ref <- data.frame(
  c("Culver","Madras","Biggs","The Dalles","Dufur","Mt Wilson","Pelton"),
  c(1,2,3,4,5,6,7),
  c("14076500","14092500","14103000","14105700",NA,NA,NA),
  c(NA,NA,NA,"USC00358407","USC00352440","USR0000OMTW","USC00356532")
)
colnames(locations_ref) <- c("location","num","USGSref","NOAAref")

#dataframe for indicators 
indicators_ref <- data.frame(c("Temperature (C)" , "Discharge (cubic ft/s)", "Gage Height (feet)"), c("00010","00060","00065"))  
colnames(indicators_ref) <- c("indicator","ref")





