library(plyr)
library(ggpmisc)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(gridExtra)
library(gtable)
library(grid)
library(lubridate)
library(readxl)
library(broom)
library(hydrostats)
setwd("~/DSPG")

# Hourly PGE data
HourlyPGEData <- read_csv("newpgeData.csv", col_types = cols(Season = col_factor()))

# Daily USGS data
USGSData <- read_csv("USGSData.csv", col_types = cols(Season = col_factor()))
MadrasData <- USGSData %>% filter(Location == "Madras") %>% select(-`Discharge (cfs)`)
MoodyData <- USGSData %>% filter(Location == "Moody") %>% select(-`Discharge (cfs)`)

# ODFW Fish Count data (Monthly and Yearly)
ODFWDataMonthly <- read_csv("ODFWData.csv", col_types = cols(Season = col_factor()))
ODFWDataYearly <- read_csv("ODFWDataYearly.csv") # Actual is total directly from ODFW, Total is from sum of monthly provided data

# PGE Fish Count data (Daily 2014-2020)
PGEFishData <- read_csv("PGEFishData.csv", 
                        col_names = c("Date_time", "Hatchery Summer Steelhead", "Summer Steelhead","Summer Steelhead RM", 
                                      "Summer Steelhead LM", "Hatchery Spring Chinook", "Wild Spring Chinook","Spring Chinook RM", 
                                      "Spring Chinook LM", "No Mark Sockeye", "Sockeye RM", "Sockeye LM", "Fall Chinook","Bull Trout", 
                                      "Rainbow Trout", "Total", "Year", "Season", "Month"), 
                        col_types = cols(Season = col_factor(), Date_time = col_datetime()))[2:959,]
PGEFishData$Date_time <- ymd(PGEFishData$Date_time)

# ODEQ Water Quality paramaters data
ODEQData <- read_csv("ODEQData.csv", col_types = cols(Season = col_factor()))






# John Day Data
JDReddsCountData <- read_csv("JohnDayReddCounts.csv")[1:13,1:17]

JohnDayBargeData <- read_csv("JohnDayBargeRates.csv", skip = 1)[,1:14]
colnames(JohnDayBargeData) <- c("Year","W_Observed","H_Observed","pHOSObserved","W_Captured","H_Captured","%H_Captured","NOSA",
                            "W_JDD","H_JDD","%H_JDD","PercentWBarged","PercentHBarged","Num_H","")


# Bonneville Dam Data
BonnevilleData <- read_csv("BonnevilleDamData.csv")
















### ANALYSIS













### PLOTS










