library(tidyverse)
library(lubridate)

setwd("~/Documents/DSPG")


# Biggs USGS Data
BiggsData <- read.table("BiggsData2.txt", header = T, fill = T, sep = "\t")
colnames(BiggsData) <- c("Agency", "Site", "Date_time", "tz_cd", "Temperature", "Temperature_qualification", "Discharge", 
                         "Discharge_qualification", "Gage_height", "Gage_height_qualification")

# Subsetting by specific year if comparisons wanted
# Biggs2011 <- BiggsData[grep("/11 ", BiggsData$Date_time), ]
# Biggs2011$Date_time <- mdy_hm(Biggs2011$Date_time)
# Biggs2012 <- BiggsData[grep("/12 ", BiggsData$Date_time), ]
# Biggs2012$Date_time <- mdy_hm(Biggs2012$Date_time)
# Biggs2013 <- BiggsData[grep("/13 ", BiggsData$Date_time), ]
# Biggs2013$Date_time <- mdy_hm(Biggs2013$Date_time)
# Biggs2014 <- BiggsData[grep("/14 ", BiggsData$Date_time), ]
# Biggs2014$Date_time <- mdy_hm(Biggs2014$Date_time)
# Biggs2015 <- BiggsData[grep("/15 ", BiggsData$Date_time), ]
# Biggs2015$Date_time <- mdy_hm(Biggs2015$Date_time)
# Biggs2016 <- BiggsData[grep("/16 ", BiggsData$Date_time), ]
# Biggs2016$Date_time <- mdy_hm(Biggs2016$Date_time)
# Biggs2017 <- BiggsData[grep("/17 ", BiggsData$Date_time), ]
# Biggs2017$Date_time <- mdy_hm(Biggs2017$Date_time)
# Biggs2018 <- BiggsData[grep("/18 ", BiggsData$Date_time), ]
# Biggs2018$Date_time <- mdy_hm(Biggs2018$Date_time)
# Biggs2019 <- BiggsData[grep("/19 ", BiggsData$Date_time), ]
# Biggs2019$Date_time <- mdy_hm(Biggs2019$Date_time)
# Biggs2020 <- BiggsData[grep("/20 ", BiggsData$Date_time), ]
# Biggs2020$Date_time <- mdy_hm(Biggs2020$Date_time)
# 
# Biggs2011 <- Biggs2011 %>% mutate(Julian = yday(Date_time))
# Biggs2012 <- Biggs2012 %>% mutate(Julian = yday(Date_time))
# Biggs2013 <- Biggs2013 %>% mutate(Julian = yday(Date_time))
# Biggs2014 <- Biggs2014 %>% mutate(Julian = yday(Date_time))
# Biggs2015 <- Biggs2015 %>% mutate(Julian = yday(Date_time))
# Biggs2016 <- Biggs2016 %>% mutate(Julian = yday(Date_time))
# Biggs2017 <- Biggs2017 %>% mutate(Julian = yday(Date_time))
# Biggs2018 <- Biggs2018 %>% mutate(Julian = yday(Date_time))
# Biggs2019 <- Biggs2019 %>% mutate(Julian = yday(Date_time))
# Biggs2020 <- Biggs2020 %>% mutate(Julian = yday(Date_time))

# Biggs2011plot <- ggplot(data = Biggs2011, aes(x = Date_time, y = Temperature)) + geom_line() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# Biggs2012plot <- ggplot(data = Biggs2012, aes(x = Date_time, y = Temperature)) + geom_line() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# Biggs2019plot <- ggplot(data = Biggs2019, aes(x = Date_time, y = Temperature)) + geom_line() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# Biggs2020plot <- ggplot(data = Biggs2020, aes(x = Date_time, y = Temperature)) + geom_line() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot of just a couple years, beaver themed
ggplot() + geom_line(data = Biggs2012, aes(x = Julian, y = Temperature, color = "2012")) +
  geom_line(data = Biggs2019, aes(x = Julian, y = Temperature, color = "2019")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme_light() + scale_color_manual(values=c("darkorange", "gray1")) +
  annotate("text", x = 200, y = 10, label = "Go Beavs")



# Changing year names to be more interpretable
BiggsData <- BiggsData %>% mutate(Year = case_when(grepl("/11 ", Date_time) ~ 2011, grepl("/12 ", Date_time) ~ 2012,
                                                                grepl("/13 ", Date_time) ~ 2013, grepl("/14 ", Date_time) ~ 2014,
                                                                grepl("/15 ", Date_time) ~ 2015, grepl("/16 ", Date_time) ~ 2016,
                                                                grepl("/17 ", Date_time) ~ 2017, grepl("/18 ", Date_time) ~ 2018,
                                                                grepl("/19 ", Date_time) ~ 2019, grepl("/20 ", Date_time) ~ 2020))

# Subsetting out extraneous observations
BiggsData2 <- subset(BiggsData, !is.na(Year))
BiggsData2 <- subset(BiggsData2, !is.na(Temperature))
BiggsData2$Date_time <- mdy_hm(BiggsData2$Date_time)

# Using lubridate to standardize Date_time
BiggsData2 <- BiggsData2 %>% mutate(Julian = yday(Date_time))

# Plot of temperature data in Biggs by year - only goes back to 2011 
# Consider doing by month as well
ggplot(data = BiggsData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() + 
  facet_wrap( ~ Year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Temperature Data from Biggs, OR", x = "Year", color = "Year")

#### CULVER DATA

CulverData <- read.table("CulverData.txt", header = T, fill = T, sep = "\t")
colnames(CulverData) <- c("Agency", "Site", "Date_time", "tz_cd", "Temperature", "Temperature_qualification", "Discharge", 
                         "Discharge_qualification", "Gage_height", "Gage_height_qualification")

CulverData <- CulverData %>% mutate(Year = case_when(grepl("/07 ", Date_time) ~ 2007, grepl("/08 ", Date_time) ~ 2008,
                                                     grepl("/09 ", Date_time) ~ 2009, grepl("/10 ", Date_time) ~ 2010,
                                                     grepl("/11 ", Date_time) ~ 2011, grepl("/12 ", Date_time) ~ 2012,
                                                   grepl("/13 ", Date_time) ~ 2013, grepl("/14 ", Date_time) ~ 2014,
                                                   grepl("/15 ", Date_time) ~ 2015, grepl("/16 ", Date_time) ~ 2016,
                                                   grepl("/17 ", Date_time) ~ 2017, grepl("/18 ", Date_time) ~ 2018,
                                                   grepl("/19 ", Date_time) ~ 2019, grepl("/20 ", Date_time) ~ 2020))

# Just making sure data is saved in preserved in earlier versions in case different uses come up
CulverData2 <- CulverData

# Using lubridate again
CulverData2$Date_time <- mdy_hm(CulverData2$Date_time)

CulverData2 <- CulverData2 %>% mutate(Julian = yday(Date_time))

# Plot of Culver USGS temperature data by year
# Definitely do a summer based monthly comparison of temperature increase between years of 2008-2016
ggplot(data = CulverData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() + 
  facet_wrap( ~ Year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Temperature Data from Culver, OR", x = "Year", color = "Year")


#### MADRAS DATA

MadrasData <- read.table("MadrasData.txt", header = T, fill = T, sep = "\t")
colnames(MadrasData) <- c("Agency", "Site", "Date_time", "tz_cd", "Temperature", "Temperature_qualification", "Discharge", 
                          "Discharge_qualification", "Gage_height", "Gage_height_qualification")

MadrasData <- MadrasData %>% mutate(Year = case_when(grepl("/07 ", Date_time) ~ 2007, grepl("/08 ", Date_time) ~ 2008,
                                                     grepl("/09 ", Date_time) ~ 2009, grepl("/10 ", Date_time) ~ 2010,
                                                     grepl("/11 ", Date_time) ~ 2011, grepl("/12 ", Date_time) ~ 2012,
                                                     grepl("/13 ", Date_time) ~ 2013, grepl("/14 ", Date_time) ~ 2014,
                                                     grepl("/15 ", Date_time) ~ 2015, grepl("/16 ", Date_time) ~ 2016,
                                                     grepl("/17 ", Date_time) ~ 2017, grepl("/18 ", Date_time) ~ 2018,
                                                     grepl("/19 ", Date_time) ~ 2019, grepl("/20 ", Date_time) ~ 2020))

MadrasData2 <- MadrasData

MadrasData2$Date_time <- mdy_hm(MadrasData2$Date_time)

MadrasData2 <- MadrasData2 %>% mutate(Julian = yday(Date_time))
view(MadrasData2)

# Plot of Madras USGS temperature data by year
# Definitely do a summer based monthly comparison of temperature increase between years of 2008-2016
ggplot(data = MadrasData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() + 
  facet_wrap( ~ Year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Temperature Data from Madras, OR", x = "Year", color = "Year")

## MERGING DATA

BiggsData2 <- BiggsData2 %>% mutate(Location = paste("Biggs"))
MadrasData2 <- MadrasData2 %>% mutate(Location = paste("Madras"))
CulverData2 <- CulverData2 %>% mutate(Location = paste("Culver"))

### MERGING USGS DATA
partusgsData <- rbind(BiggsData2, MadrasData2)
allusgsData <- rbind(partusgsData, CulverData2)

### Merging PGE data
mergeCols <- c("Temperature","Location","Date_time")
rivertempbigData <- merge(allusgsData, df5, by = mergeCols, all = TRUE)


## Taking in ODEQ data and then merging also mutatubg location names
odeqData <- read_excel("ODEQ_data_datetime_revised.xlsx")

odeqData$Date <- NULL
odeqData$Time <- NULL
colnames(odeqData) <- c("Location", "Temperature", "Date_time")
rivertempbigData <- merge(rivertempbigData, odeqData, by = mergeCols, all = TRUE)

rivertempbigData <- rivertempbigData %>% mutate(Location = if_else(Location == "10511-ORDEQ", "Mirror Pond", Location)) %>%
  mutate(Location = if_else(Location == "10508-ORDEQ", "Lower Bridge", Location)) %>%
  mutate(Location = if_else(Location == "10506-ORDEQ", "Warm Springs", Location)) %>%
  mutate(Location = if_else(Location == "10411-ORDEQ", "Deschutes River Park", Location))

## REMOVING USELESS COLUMNS
colnames(rivertempbigData)
rivertempbigData$Agency <- NULL
rivertempbigData$Site <- NULL
rivertempbigData$tz_cd <- NULL
rivertempbigData$Agency <- NULL
rivertempbigData$Temperature_qualification <- NULL
rivertempbigData$Discharge_qualification <- NULL
rivertempbigData$Gage_height_qualification <- NULL


## FILLING IN DATA WHERE POSSIBLE, Specifically Year and Julian
rivertempbigData <- rivertempbigData %>% mutate(Julian = yday(Date_time))
rivertempbigData <- rivertempbigData %>% mutate(Year = year(Date_time))



### MERGED DATA GRAPHS, find out how to smooth with so many observations

ggplot() + geom_line(data = BiggsData2, aes(x = Julian, y = Temperature, color = "Biggs")) +
  geom_line(data = CulverData2, aes(x = Julian, y = Temperature, color = "Culver")) +
  geom_line(data = MadrasData2, aes(x = Julian, y = Temperature, color = "Madras")) + 
  facet_wrap( ~ Year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Temperature Data from Madras, Biggs, and Culver", x = "Day of Year", color = "Location")

# Graph of all temperature data from all years available, needs data smoothing and transparency
ggplot() + geom_line(data = BiggsData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year))) +
  geom_line(data = CulverData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year))) +
  geom_line(data = MadrasData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year)))


# Going just by the year 2012 which is extremely arbitrary
Biggs2012 <- BiggsData2 %>% filter(Year == 2012)
Madras2012 <- MadrasData2 %>% filter(Year == 2012)
Culver2012 <- CulverData2 %>% filter(Year == 2012)

ggplot() + geom_line(data = Biggs2012, aes(x = Date_time, y = Temperature, color = "Biggs")) + 
  geom_line(data = Culver2012, aes(x = Date_time, y = Temperature, color = "Culver")) + 
  geom_line(data = Madras2012, aes(x = Date_time, y = Temperature, color = "Madras"))


#### TIME SERIES CORRELATIONS
#Try acf and pacf, test different sites data against each other, read studies, also make an overlapping plot with fish capture



# plotter <- rivertempbigData %>% group_by(Location)

# PGE data has unclear location labels, this comparison makes it clear that River Mouth and Biggs data are the same locations
riverMouth1 <- rivertempbigData %>% filter(Location == "River Mouth" | Location == "Biggs")
ggplot(riverMouth1, aes(x = Date_time, y = Temperature, color = Location)) + 
  geom_line() + scale_color_manual(values=c("darkorange", "gray1"))



## AIR TEMPERATURE DATA

airtempData <- read.csv("2207755.csv")

# Converting data to better formats

airtempData$DATE <- ymd(airtempData$DATE)
airtempData$NAME <- as.character(airtempData$NAME)

# Adding a column to airtempData for celsius observations
airtempData <- airtempData %>% mutate(cTAVG = (TAVG -32)/1.8)

unique(airtempData$NAME)
view(airtempData)

airtempData2 <- airtempData %>% mutate(NAME = if_else(NAME == "PELTON DAM, OR US", "Pelton Dam", NAME)) %>%
  mutate(NAME = if_else(NAME == "POLLYWOG OREGON, OR US" | "MAUPIN 10.0 SSE, OR US" | "MOUNT WILSON OREGON, OR US"| "WAMIC MILL OREGON, OR US" | "NORTH POLE RIDGE OREGON, OR US" | "WASCO BUTTE OREGON, OR US", "Wasco County", NAME)) %>%
  mutate(NAME = if_else(NAME == "THE DALLES, OR US" | "CITY OF THE DALLES 3.0 W, OR US" | "MOSIER 3.8 E, OR US" | "DUFUR, OR US" | "", "The Dalles", NAME)) %>%
  mutate(NAME = if_else(NAME == "ANTELOPE 6 SSW, OR US" | "MADRAS, OR US" | "CLEAR LAKE, OR US" | "HEHE 1 OREGON, OR US" | "MUTTON MOUNTAIN OREGON, OR US", "Madras", NAME))

rivertempbigData <- rivertempbigData %>% mutate(Location = if_else(Location == "10511-ORDEQ", "Mirror Pond", Location)) %>%
  mutate(Location = if_else(Location == "10508-ORDEQ", "Lower Bridge", Location)) %>%
  mutate(Location = if_else(Location == "10506-ORDEQ", "Warm Springs", Location)) %>%
  mutate(Location = if_else(Location == "10411-ORDEQ", "Deschutes River Park", Location))




# Using PGE river temperature data from river mouth (RM 0), Reregulating DAM (RM 100), Kloan Rapids (RM 6.8), and Lower Wapinitia (RM 55.2)
test <- rivertempbigData %>% 
  filter(Location == "River Mouth" | Location == "Reregulating Dam" | Location == "Kloan Rapids" | Location == "Lower Wapinitia") %>% 
  select(Temperature, Date_time, Location) %>% arrange(Date_time)

# Creating air temperature average by max and min then converting to Celsius and plotting over PGE river temperature data
ggplot() + geom_line(data = test, aes(x = Date_time, y = Temperature, color = Location)) +
  geom_line(data = airplot, aes(x = as.POSIXct(DATE), y = (((TMAX+TMIN)/2)-32)/1.8))

airplot <- airtempData %>% filter(NAME == "MAUPIN 10.0 SSE, OR US") %>% select(cTAVG, DATE, TMAX, TMIN)

airplot$YEAR <- substr(airplot$DATE, 1,4)
date1 <- as.Date("2015-01-01")
date2 <- as.Date("2017-08-30")
airplot <- airplot[airplot$DATE >= date1 & airplot$DATE <= date2,]

ggplot() + geom_line(data = airplot, aes(x = DATE, y = cTAVG))
view(airplot)
