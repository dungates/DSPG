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
# ggplot() + geom_line(data = Biggs2012, aes(x = Julian, y = Temperature, color = "2012")) +
#   geom_line(data = Biggs2019, aes(x = Julian, y = Temperature, color = "2019")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme_light() + scale_color_manual(values=c("darkorange", "gray1")) +
#   annotate("text", x = 200, y = 10, label = "Go Beavs")



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
# ggplot(data = BiggsData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() + 
#   facet_wrap( ~ Year) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "Temperature Data from Biggs, OR", x = "Year", color = "Year")

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
# ggplot(data = CulverData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() + 
#   facet_wrap( ~ Year) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "Temperature Data from Culver, OR", x = "Year", color = "Year")


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


# Plot of Madras USGS temperature data by year
# Definitely do a summer based monthly comparison of temperature increase between years of 2008-2016
# ggplot(data = MadrasData2, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() +
#   facet_wrap( ~ Year) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "Temperature Data from Madras, OR", x = "Year", color = "Year")
#This is May 15 to August 15 graph of just Madras
# justsummerdata <- MadrasData2 %>% filter(Julian > 135 & Julian < 227)
# ggplot(data = justsummerdata, aes(x = Julian, y = Temperature, color = factor(Year))) + geom_line() +
#   geom_hline(aes(yintercept = 10)) +
#   facet_wrap( ~ Year) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "Temperature Data from Madras, OR", x = "Year", color = "Year")

# Graphing before and after SWW Tower for Madras, gridded
# Madras2009 <- MadrasData2 %>% filter(Year == 2009)
# Madras2019 <- MadrasData2 %>% filter(Year == 2019)
# Madras2019 <- Madras2019 %>% mutate(Year = month(Julian))
# 
# ggplot() + geom_line(data = Madras2009, aes(x = Julian, y = Temperature, color = "2009")) +
#   geom_line(data = Madras2019, aes(x = Julian, y = Temperature, color = "2019")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme_light() + scale_color_manual(values=c("darkorange", "gray1")) +
#   labs(y = "Temperature °C", x = "Day of Year") + ggtitle("Madras Temperature Daily") +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name = "Year")




## MERGING DATA

BiggsData2 <- BiggsData2 %>% mutate(Location = paste("Biggs"))
MadrasData2 <- MadrasData2 %>% mutate(Location = paste("Madras"))
CulverData2 <- CulverData2 %>% mutate(Location = paste("Culver"))

### MERGING USGS DATA
partusgsData <- rbind(BiggsData2, MadrasData2)
allusgsData <- rbind(partusgsData, CulverData2)

### Merging PGE data
mergeCols <- c("Temperature","Location","Date_time")
# Note that df5 is from the PGE data file

df <- read_excel("pge-water-chemistry-2015-2017.xlsx")

# Using spread to make columns for different Parameters which are listed initially under Temperature column
# df2 <- spread(data = df, Parameter, Value)
# str(df2)

# Making a new column that is composed of the units and parameters from data
df3 <- df %>% mutate(new = paste(Parameter, "in", Units))

# Deleting columns that were just pasted
df3 <- subset(df3, select = -c(Units, Parameter))

# Try later, not useful right now
# df4 <- df3 %>% gather()

# Subsetting out what appears to be additional measurements that are similar enough to be irrelevant
df4 <- df3[-c(2525,2524,2253,2254,1982,1983,1711,1712,1441,1442),]

# Spreading data by Value and then renaming the columns, deleting station ID
df5 <- spread(df4, new, Value)
df5$`Station ID` <- NULL
colnames(df5)[7] <- c("Temperature")
colnames(df5)[1] <- c("Location")
colnames(df5)[2] <- c("Date_time")




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

# df13 <- rivertempbigData %>% filter(!is.na(`Conductivity in μS/cm`))
# CONDUCTIVITY DATA GRAPHED
# ggplot(df13, aes(x = Date_time, y = `Conductivity in μS/cm`, color = Location)) + 
#   geom_line() + theme(legend.position = "none") + 
#   geom_dl(aes(label = Location), method = list(dl.combine("last.points")), cex = 0.8)

## REMOVING USELESS COLUMNS
colnames(rivertempbigData)
rivertempbigData$Agency <- NULL
rivertempbigData$Site <- NULL
rivertempbigData$tz_cd <- NULL
rivertempbigData$Agency <- NULL
rivertempbigData$Temperature_qualification <- NULL
rivertempbigData$Discharge_qualification <- NULL
rivertempbigData$Gage_height_qualification <- NULL


## FILLING IN DATA WHERE POSSIBLE, Specifically Year and Julian, adding season and a additional date format for air temperature merging
# Creating a function for seasons
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

rivertempbigData <- rivertempbigData %>% mutate(Julian = yday(Date_time))
rivertempbigData <- rivertempbigData %>% mutate(Year = year(Date_time))
rivertempbigData <- rivertempbigData %>% mutate(mergeDate = ymd(as.Date(Date_time)))
rivertempbigData <- rivertempbigData %>% mutate(Season = getSeason(Date_time))

## RUN UP TO HERE FOR  FIRST DATAFRAME

### MERGED DATA GRAPHS, find out how to smooth with so many observations

# ggplot() + geom_line(data = BiggsData2, aes(x = Julian, y = Temperature, color = "Biggs")) +
#   geom_line(data = CulverData2, aes(x = Julian, y = Temperature, color = "Culver")) +
#   geom_line(data = MadrasData2, aes(x = Julian, y = Temperature, color = "Madras")) + 
#   facet_wrap( ~ Year) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "Temperature Data from Madras, Biggs, and Culver", x = "Day of Year", color = "Location")

# Graph of all temperature data from all years available, needs data smoothing and transparency
# ggplot() + geom_line(data = BiggsData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year))) +
#   geom_line(data = CulverData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year))) +
#   geom_line(data = MadrasData2, aes(x = Julian, y = Temperature, group = Year, color = as.factor(Year)))


# Going just by the year 2012 which is extremely arbitrary
Biggs2012 <- BiggsData2 %>% filter(Year == 2012)
Madras2012 <- MadrasData2 %>% filter(Year == 2012)
Culver2012 <- CulverData2 %>% filter(Year == 2012)

# ggplot() + geom_line(data = Biggs2012, aes(x = Date_time, y = Temperature, color = "Biggs")) + 
#   geom_line(data = Culver2012, aes(x = Date_time, y = Temperature, color = "Culver")) + 
#   geom_line(data = Madras2012, aes(x = Date_time, y = Temperature, color = "Madras"))


#### TIME SERIES CORRELATIONS
#Try acf and pacf, test different sites data against each other, read studies, also make an overlapping plot with fish capture



plotter <- rivertempbigData %>% group_by(Location)

# PGE data has unclear location labels, this comparison makes it clear that River Mouth and Biggs data are the same locations
# riverMouth1 <- rivertempbigData %>% filter(Location == "River Mouth" | Location == "Biggs")
# ggplot(riverMouth1, aes(x = Date_time, y = Temperature, color = Location)) + 
#   geom_line() + scale_color_manual(values=c("darkorange", "gray1"))



## AIR TEMPERATURE DATA

airtempData <- read.csv("2207755.csv")

# Converting data to better formats

airtempData$DATE <- ymd(airtempData$DATE)
airtempData$NAME <- as.character(airtempData$NAME)

# Adding a column to airtempData first for average temperature then for celsius observations
airtempData <- airtempData %>% mutate(TAVG = (TMAX+TMIN)/2)
airtempData <- airtempData %>% mutate(cTAVG = (TAVG -32)/1.8)

airtempData2 <- airtempData %>% mutate(NAME = if_else(NAME == "PELTON DAM, OR US", "Madras", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "POLLYWOG OREGON, OR US", "Maupin", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "MAUPIN 10.0 SSE, OR US", "Maupin", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "MOUNT WILSON OREGON, OR US", "Maupin", NAME))
airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "WAMIC MILL OREGON, OR US", "Maupin", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "NORTH POLE RIDGE OREGON, OR US", "Maupin", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "WASCO BUTTE OREGON, OR US", "Maupin", NAME))

airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "THE DALLES, OR US", "Biggs", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "CITY OF THE DALLES 3.0 W, OR US", "Biggs", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "MOSIER 3.8 E, OR US", "Biggs", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "DUFUR, OR US", "Biggs", NAME))

# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "ANTELOPE 6 SSW, OR US", "Madras", NAME))
airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "MADRAS, OR US", "Culver", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "CLEAR LAKE, OR US", "Madras", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "HEHE 1 OREGON, OR US", "Madras", NAME))
# airtempData2 <- airtempData2 %>% mutate(NAME = if_else(NAME == "MUTTON MOUNTAIN OREGON, OR US", "Madras", NAME))

#Adding a year column
airtempData2 <- airtempData2 %>% mutate(Year = year(DATE))

#Testing plot
#ggplot(airtempData2, aes(x = DATE, y = cTAVG, color = NAME)) + geom_line(linetype = "dotted")

#START HERE FOR DATA MERGE, ORGANIZE CODE ABOVE, PUT GRAPHS BELOW
airtempData2$STATION <- NULL
colnames(airtempData2) <- c("Location", "Latitude", "Longitude", "Elevation", "mergeDate", "Tavg", "Tmax", "Tmin", "Tobs", "cTAVG", "Year")
airtempData3 <- airtempData2 %>% filter(Location == "Maupin" | Location == "Madras" | Location == "Biggs" | Location == "Culver")
airtempmerge <- airtempData3 %>% filter(Year > 2006) %>% select(cTAVG, Year, Location, mergeDate) %>% group_by(Location, mergeDate)
airandwatertempdf <- rivertempbigData %>% left_join(airtempmerge, by = c("mergeDate","Year","Location")) #Note that this currently excludes ODEQ AND PGE Data, also that Maupin air temp data won't merge
airandwatertempdf <- airandwatertempdf %>% relocate(cTAVG, .after = Temperature)
# Subsets out additional observations
airandwatertempdf <- airandwatertempdf %>% distinct(mergeDate, Location, .keep_all = T) %>% arrange(mergeDate)

summary(lm(Temperature ~ cTAVG + as.factor(Year), data = airandwatertempdf))

MadrasSeasonYearlyRegression <- airandwatertempdf %>% filter(Location == "Madras") %>% 
  group_by(Season, Year) %>% do(model = lm(Temperature ~ cTAVG, data = .)) %>% tidy(model) %>% filter(term == "cTAVG") # try augmenting model
BiggsSeasonYearlyRegression <- airandwatertempdf %>% filter(Location == "Biggs") %>% 
  group_by(Season, Year) %>% do(tidy(lm(Temperature ~ cTAVG, data = .))) %>% filter(term == "cTAVG")
CulverSeasonYearlyRegression <- airandwatertempdf %>% filter(Location == "Culver" & Year > 2007) %>% 
  group_by(Season, Year) %>% do(tidy(lm(Temperature ~ cTAVG, data = .))) %>% filter(term == "cTAVG")

#Table of regression coefficients
allregressioncoefficients <- MadrasSeasonYearlyRegression %>% 
  left_join(select(BiggsSeasonYearlyRegression, estimate), by = c("Year","Season")) %>%
  left_join(select(CulverSeasonYearlyRegression, estimate), by = c("Year","Season"))
allregressioncoefficients <- allregressioncoefficients[, -c(5,6,7)]
colnames(allregressioncoefficients) <- c("Season", "Year", "Explanatory Variable", 
                                         "MadrasEstimate", "BiggsEstimate", "CulverEstimate")

# Just checking the correlation coefficients of each season, spring is by far the greatest corresponding increase, fall is close second interestingly, check stargazer for more info
testplot <- airandwatertempdf %>% filter(Location == "Madras" & Season == "Spring")
testplot2 <- airandwatertempdf %>% filter(Location == "Madras" & Season == "Summer")
testplot3 <- airandwatertempdf %>% filter(Location == "Madras" & Season == "Fall")
testplot4 <- airandwatertempdf %>% filter(Location == "Madras" & Season == "Winter")
# ggplot() + geom_line(data = testplot, aes(x = Date_time, y = Temperature), color = "blue") + 
#   geom_line(data = testplot, aes(x = Date_time, y = cTAVG), linetype = "dashed", color = "red") + facet_wrap( ~ Year)



# Table of regression coefficients by season, all time
ols1 <- lm(Temperature ~ cTAVG, data = testplot)
ols2 <- lm(Temperature ~ cTAVG, data = testplot2)
ols3 <- lm(Temperature ~ cTAVG, data = testplot3)
ols4 <- lm(Temperature ~ cTAVG, data = testplot4)
stargazer(ols1, ols2, ols3, ols4, title = "Results", align = T, type = "text")


# Using PGE river temperature data from river mouth (RM 0), Reregulating DAM (RM 100), Kloan Rapids (RM 6.8), and Lower Wapinitia (RM 55.2)
test <- rivertempbigData %>% 
  filter(Location == "River Mouth" | Location == "Reregulating Dam" | Location == "Kloan Rapids" | Location == "Lower Wapinitia") %>% 
  select(Temperature, Date_time, Location) %>% arrange(Date_time)
riverMouthFilter <- rivertempbigData %>% filter(Location == "River Mouth")
reregDamFilter <- rivertempbigData %>% filter(Location == "Reregulating Dam")
kloanRapidsFilter <- rivertempbigData %>% filter(Location == "Kloan Rapids")
lowerWapFilter <- rivertempbigData %>% filter(Location == "Lower Wapinitia")

# Creating air temperature average by max and min then converting to Celsius and plotting over PGE river temperature data, some data
# conversion necessary
airplot <- airtempData2 %>% filter(Location == "Madras") %>% select(cTAVG, mergeDate, Tmax, Tmin)
date1 <- as.Date("2015-01-01")
date2 <- as.Date("2017-08-30")
airplot <- airplot[airplot$mergeDate >= date1 & airplot$mergeDate <= date2,]

linecolor <- c("red", "blue", "forestgreen", "purple", "bisque2")
# ggplot() + geom_line(data = test, aes(x = Date_time, y = Temperature, color = Location)) +
#   geom_line(data = airplot, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Pelton Dam Air Temperature"), linetype = "dashed") +
#   scale_color_manual(values = linecolor)

# Same thing but for USGS data and plotted at specific locations
# Here is for Biggs and the Dalles
airplot2 <- airtempData2 %>% filter(Location == "Biggs") %>% select(cTAVG, mergeDate, Tmax, Tmin)
date3 <- as.Date("2010-01-01")
date4 <- as.Date("2020-07-01")
airplot2 <- airplot2[airplot2$mergeDate >= date3 & airplot2$mergeDate <= date4,]

BiggsAirandWater <- airandwatertempdf %>% filter(Location == "Biggs")
MadrasAirandWater <- airandwatertempdf %>% filter(Location == "Madras")
CulverAirandWater <- airandwatertempdf %>% filter(Location == "Culver" & Year > 2007)
# Graph of Biggs air vs water temp
# ggplot() + geom_line(data = BiggsData2, aes(x = Date_time, y = Temperature, color = Location)) +
#   geom_line(data = airplot2, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Air Temperature in The Dalles"), linetype = "dashed") +
#   scale_color_brewer(palette = "Paired")




### FACET WRAPS OF BIGGS, MADRAS, AND CULVER FOR COMPARISON, add titles later
lm_eqn <- function(BiggsAirandWater) {
  m = lm(Temperature ~ cTAVG, BiggsAirandWater);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(as.numeric(coef(m)[1]), digits = 2), 
                        b = format(as.numeric(coef(m)[2]), digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));  
}

eq <- ddply(BiggsAirandWater,.(Year, Season),lm_eqn)

# ggplot(data = BiggsAirandWater, aes(cTAVG, Temperature)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F) + geom_text(data = eq, aes(x = -1, y = 18, label = V1), parse = T, cex = 2, inherit.aes = F) +
#   facet_wrap(Year ~ Season, ncol = 4)

# Coef stuff for Biggs
# fitmodels <- BiggsAirandWater %>% group_by(Year, Season) %>% do(model = cor.test( ~ Temperature + cTAVG, data = .))
# 
# for (i in 1:37) {
#   pval[i] <- fitmodels$model[[i]]$p.value
#   coef[i] <- fitmodels$model[[i]]$estimate
#   year[i] <- fitmodels$Year[i]
#   season[i] <- fitmodels$Season[i]
# }
# 
# df19 <- data.frame(pval = c(pval), coef = c(coef), year = c(year), season = c(season))
# df19$year <- as.Date(paste0(year, '-01-01'))
# ggplot(df19, aes(x = year, y = coef, color = season)) + geom_point()
# 
# 
# # Coef stuff for Madras
# 
# fitmodels1 <- MadrasAirandWater %>% group_by(Year, Season) %>% do(model = cor.test( ~ Temperature + cTAVG, data = .))
# 
# for (i in 1:52) {
#   pval[i] <- fitmodels1$model[[i]]$p.value
#   coef[i] <- fitmodels1$model[[i]]$estimate
#   year[i] <- fitmodels1$Year[i]
#   season[i] <- fitmodels1$Season[i]
# }
# 
# df20 <- data.frame(pval = c(pval), coef = c(coef), year = c(year), season = c(season))
# df20 <- df20 %>% mutate(Season = case_when(season == "1" ~ "Winter", season == "2" ~ "Spring", 
#                                   season == "3" ~ "Summer", season == "4" ~ "Fall"))
# 
# df20$year <- as.Date(paste0(year, '-01-01'))
# ggplot(df20, aes(x = Season, y = coef, color = pval)) + geom_point() + facet_wrap( ~ year)


# MADRAS
lm_eqn1 <- function(MadrasAirandWater) {
  m1 = lm(Temperature ~ cTAVG, MadrasAirandWater);
  eq1 <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(as.numeric(coef(m1)[1]), digits = 2), 
                        b = format(as.numeric(coef(m1)[2]), digits = 2), 
                        r2 = format(summary(m1)$r.squared, digits = 3)))
  as.character(as.expression(eq1));  
}

eq1 <- ddply(MadrasAirandWater,.(Year, Season),lm_eqn1)

# ggplot(data = MadrasAirandWater, aes(cTAVG, Temperature)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F) + geom_text(data = eq1, aes(x = -1, y = 18, label = V1), parse = T, cex = 2, inherit.aes = F) +
#   facet_wrap(Year ~ Season, ncol = 4)

# CULVER
lm_eqn2 <- function(CulverAirandWater) {
  m2 = lm(Temperature ~ cTAVG, CulverAirandWater);
  eq2 <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(as.numeric(coef(m2)[1]), digits = 2), 
                        b = format(as.numeric(coef(m2)[2]), digits = 2), 
                        r2 = format(summary(m2)$r.squared, digits = 3)))
  as.character(as.expression(eq2));  
}

eq2 <- ddply(CulverAirandWater,.(Year, Season),lm_eqn2)

# ggplot(data = CulverAirandWater, aes(cTAVG, Temperature)) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = F) + geom_text(data = eq2, aes(x = -1, y = 18, label = V1), parse = T, cex = 2, inherit.aes = F) +
#   facet_wrap(Year ~ Season, ncol = 4)


# Here is for Madras (USGS) and Madras (NOAA)
airplot3 <- airtempData2 %>% filter(Location == "Madras") %>% select(cTAVG, mergeDate, Tmax, Tmin, Year)
date5 <- as.Date("2007-01-01")
date6 <- as.Date("2020-07-01")
airplot3 <- airplot3[airplot3$mergeDate >= date5 & airplot3$mergeDate <= date6,]

idklol <- airplot3 %>% filter(Year == 2008)
idklol2 <- airplot3 %>% filter(Year == 2012)
idklol4 <- airplot3 %>% filter(Year == 2018)
idklol3 <- airplot3 %>% filter(Year == 2019)
Madras2008 <- MadrasData2 %>% filter(Year == 2008)
Madras2012 <- MadrasData2 %>% filter(Year == 2012)
Madras2018 <- MadrasData2 %>% filter(Year == 2018)
Madras2019 <- MadrasData2 %>% filter(Year == 2019)

# Graphs of 2008 and 2012
plot1 <- ggplot() + geom_line(data = Madras2008, aes(x = Date_time, y = Temperature, color = Location)) +
  geom_line(data = idklol, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Madras Air Temperature"), linetype = "dashed") +
  scale_color_brewer(palette = "Paired") + theme(legend.position='none') + xlab("Date")
plot2 <- ggplot() + geom_line(data = Madras2012, aes(x = Date_time, y = Temperature, color = Location)) +
  geom_line(data = idklol2, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Madras Air Temperature"), linetype = "dashed") +
  scale_color_brewer(palette = "Paired") + ylim(-15, 30) + theme(legend.position='none') + xlab("Date")
plot3 <- ggplot() + geom_line(data = Madras2019, aes(x = Date_time, y = Temperature, color = Location)) +
  geom_line(data = idklol3, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Madras Air Temperature"), linetype = "dashed") +
  scale_color_brewer(palette = "Paired") + ylim(-15, 30) + theme(legend.position='none') + xlab("Date")
plot4 <- ggplot() + geom_line(data = Madras2018, aes(x = Date_time, y = Temperature, color = Location)) +
  geom_line(data = idklol4, aes(x = as.POSIXct(mergeDate), y = cTAVG, color = "Madras Air Temperature"), linetype = "dashed") +
  scale_color_brewer(palette = "Paired") + ylim(-15, 30) + theme(legend.position='none') + xlab("Date")
#This is a solid graph run it if you don't remember
# grid.arrange(plot1, plot2, plot4, plot3, ncol = 2)

# Facet wrapped graph
# ggplot() + geom_line(data = MadrasData2, aes(x = Date_time, y = Temperature, color = Location)) +
#   geom_line(data = airplot3, aes(x = as.POSIXct(DATE), y = cTAVG, color = "Madras Air Temperature"), linetype = "dashed") +
#   scale_color_brewer(palette = "Dark2") + facet_wrap( ~ Year)


# Compare river temperature, air temperature data ratio to determine impact above and below the dam



# Some pH graphing
df10 <- rivertempbigData %>% 
  filter(!is.na(`pH in NA`)) %>% arrange(Location, desc(Date_time))
min <- as.Date("2015-01-01")
max <- as.Date("2017-01-01")
# ggplot(df10, aes(as.Date(Date_time), y = `pH in NA`, color = Location, group = Location)) + geom_line() + scale_x_date(limits = c(min, max)) +
#   geom_hline(yintercept = 8.5, linetype = "dashed") +
#   geom_dl(aes(label = Location), method = list(dl.combine("last.points")), cex = 0.8) + theme(legend.position = "none") +
#   labs(x = "Date", y = "pH") + annotate("text", x = as.Date("2015-02-15"), y = 8.6, label = "ODEQ Standard (8.5)")
# 
# df11 <- df10 %>% filter(Location == "Whitehorse" | Location == "Shitike Creek" | Location == "River Mouth" | Location == "Reregulating Dam")
# ggplot(df11, aes(Date_time, `pH in NA`, color = Location)) + geom_line()



# Overlaid plot of just Madras, Biggs, Culver, plots used in midterm presentation
# usgsdata1 <- rivertempbigData %>% filter(Location == "Biggs" | Location == "Culver" | Location == "Madras") %>% filter(Year == 2008)
# plt2008ylims <- layer_scales(plot2015)$y$range$range
# plot2008 <- ggplot(usgsdata1, aes(x = Date_time, y = Temperature, color = Location)) + geom_line() +
#   scale_color_manual(values = c("red", "blue")) +
#   geom_hline(yintercept = 16, linetype = "dashed") + ylim(plt2008ylims) + theme(axis.title.x = element_blank(),
#                                                                                 axis.title.y = element_blank())
# usgsdata2 <- rivertempbigData %>% filter(Location == "Biggs" | Location == "Culver" | Location == "Madras") %>% filter(Year == 2015)
# plot2015 <- ggplot(usgsdata2, aes(x = Date_time, y = Temperature, color = Location)) + geom_line() +
#   scale_color_manual(values = c("forestgreen", "red", "blue")) +
#   geom_hline(yintercept = 16, linetype = "dashed") + theme(axis.title.x = element_blank(),
#                                                            axis.title.y = element_blank())
# legend1 <- gtable_filter(ggplotGrob(plot2015), "guide-box")
# 
# 
# # Plot years 2008, 2012, 2019
# 
# grid.arrange(arrangeGrob(plot2008 + theme(legend.position = "none"), 
#                          plot2015 + theme(legend.position = "none"),
#                          nrow = 1,
#                          top = textGrob("Water Temperature before and After SWW Activation", vjust = 1, 
#                                         gp = gpar(fontface = "bold", cex = 1.5, col = "goldenrod4")),
#                          left = textGrob("Temperature (°C)", rot = 90, vjust = 1)),
#                          bottom = textGrob("Date", hjust = 1),
#                          legend1,
#                          widths = unit.c(unit(1, "npc") - legend1$width, legend1$width), ncol = 2)
# 
pgeLocations <- unique(df5$Location)
# pgedata2015 <- rivertempbigData %>% filter(Location %in% pgeLocations) %>% filter(Year == 2015)
# odeqdata2015 <- rivertempbigData %>% filter(Location == "Mirror Pond" | 
#                                               Location == "Lower Bridge" | Location == "Warm Springs" | 
#                                               Location == "Deschutes River Park") %>% filter(Year == 2015)
# 
# pgeandodeq <- rivertempbigData %>% filter(Location != "Biggs" & Location != "Culver" & Location != "Madras")
# fakeplot <- ggplot(pgeandodeq, aes(x = Date_time, y = Temperature, color = Location)) + geom_line()
# legend2 <- gtable_filter(ggplotGrob(fakeplot), "guide-box")
# 
# pgeplot2015 <- ggplot(pgedata2015, aes(Date_time, Temperature, color = Location)) + geom_line() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_hline(yintercept = 16, linetype = "dashed")
# odeqplot2015 <- ggplot(odeqdata2015, aes(Date_time, Temperature, color = Location)) + geom_line() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_hline(yintercept = 16, linetype = "dashed")
# #Update legend
# grid.arrange(arrangeGrob(pgeplot2015 + theme(legend.position = "none"),
#              odeqplot2015 + theme(legend.position = "none"),
#              nrow = 2,
#              top = textGrob("PGE and ODEQ Water Temperature Data", vjust = 1,
#                             gp = gpar(fontface = "bold", cex = 1.5, col = "goldenrod4")),
#              left = textGrob("Temperature (°C)", rot = 90, vjust = 1)),
#              bottom = textGrob("Date", hjust = 1),
#              legend2,
#              widths = unit.c(unit(1, "npc") - legend2$width, legend2$width), ncol = 2)


# Make sure to have upstream and downstream of dam graph somewhere in here


# Trying to replicate the PGE graph of figure 6-4 here
# rivertempbigData %>% filter(Location == "Reregulating Dam") %>% 
#   ggplot(aes(x = as.Date(Julian, origin = as.Date("2015-01-01")), y = Temperature)) + geom_line() + facet_wrap( ~ Year) +
#   scale_x_date(date_labels = "%b")

# Attempting to replicate 24 degrees celsius observation of river mouth - max is 23.3 from what I see
# rivertempbigData %>% filter(Location == "Biggs") %>% ggplot(aes(Date_time, Temperature, color = Location)) + geom_line() + 
#   stat_peaks(aes(label = stat(y.label)), geom = "label", color = "red", hjust = -0.1)

# Replicating figure 6-5

# rivertempbigData %>% filter(Date_time >= "2015-01-01" & Date_time <= "2017-01-01") %>% 
#   filter(Location == "Madras" | Location == "Biggs") %>% ggplot(aes(Date_time, Temperature, color = Location)) + geom_line()


### NEW USGS DATA READIN
MadrasGageData <- read.table("MadrasTemperatureData.txt", header = T, fill = T, sep = "\t")
MoodyGageData <- read.table("MoodyTemperatureData.txt", header = T, fill = T, sep = "\t")

MadrasGageData <- MadrasGageData %>% mutate(Location = "Madras")
MadrasGageData$X113433_00010_00001_cd <- NULL
MadrasGageData$X113434_00010_00002_cd <- NULL
MadrasGageData$X113435_00010_00003_cd <- NULL # Subsetting out approval columns since data is already quality controlled
MadrasGageData$X113436_00060_00003_cd <- NULL
colnames(MadrasGageData) <- c("Agency", "Site", "Date_time", "Max Temperature", "Min Temperature", "Mean Temperature", 
                             "Discharge (cfs)", "Location")
MadrasGageData <- MadrasGageData %>% mutate(`Mean Temperature` = case_when(is.na(`Mean Temperature`) ~ 
                                                                         (`Max Temperature` + `Min Temperature`) / 2,
                                                                         !is.na(`Mean Temperature`) ~ `Mean Temperature`))

MoodyGageData <- MoodyGageData %>% mutate(Location = "Moody")
MoodyGageData$X113455_00010_00001_cd <- NULL
MoodyGageData$X113456_00010_00002_cd <- NULL
MoodyGageData$X113457_00010_00003_cd <- NULL
MoodyGageData$X113458_00060_00003_cd <- NULL
MoodyGageData$X265533_00010_00011_cd <- NULL
colnames(MoodyGageData) <- c("Agency", "Site", "Date_time", "Max Temperature", "Min Temperature", "Mean Temperature", 
                             "Discharge (cfs)", "Instantaneous Temperature", "Location")
MoodyGageData <- MoodyGageData %>% mutate(`Mean Temperature` = coalesce(`Instantaneous Temperature`, `Mean Temperature`))
MoodyGageData$`Instantaneous Temperature` <- NULL
MoodyGageData <- MoodyGageData %>% mutate(`Mean Temperature` = case_when(is.na(`Mean Temperature`) ~ 
                                                                             (`Max Temperature` + `Min Temperature`) / 2,
                                                                           !is.na(`Mean Temperature`) ~ `Mean Temperature`))

allusgsdata2 <- rbind(MadrasGageData, MoodyGageData)
allusgsdata2$Year <- four.digit.year(as.POSIXct(allusgsdata2$Date_time, format = "%m/%d/%y"), year = 1951) #lubridate is set in 1970 gotta transform data
allusgsdata2$Date_time <- mdy(allusgsdata2$Date_time)
allusgsdata2 <- allusgsdata2 %>% 
  mutate(Date_time = case_when(year(Date_time) > 2021 ~ 'year<-'(Date_time, Year), TRUE ~ Date_time))
allusgsdata2 <- allusgsdata2 %>% mutate(Season = getSeason(Date_time)) %>% mutate(Julian = yday(Date_time))
allusgsdata2 <- allusgsdata2 %>% mutate(lat = case_when(Location == "Madras" ~ c(45.62222222), Location == "Moody" ~ c(44.72611111))) %>%
  mutate(long = case_when(Location == "Madras" ~ c(-120.90444444), Location == "Moody" ~ c(-121.24583333)))

# allusgsdata2 <- allusgsdata2 %>% mutate(Yearagain = paste(Date_time[1:4], "/", Year, sep = "")) this doesn't work
MadrasDataMedians <- allusgsdata2 %>% filter(Location == "Madras") %>% group_by(Year) %>% 
  summarize(median = median(`Mean Temperature`, na.rm = T), medianDate = ) %>% 
  filter(Year == 1953 | Year == 1956 | Year == 2008 | Year == 2009 | Year == 2016 | Year == 2019) %>% 
  mutate(lab = paste("Median = ", median))
MadrasDataMedians$x = as.Date(c("1953-06-15",
                                 "1956-06-15",
                                 "2008-06-15",
                                 "2009-06-15",
                                 "2016-06-15",
                                 "2019-06-15"))
MadrasDataMedians$Julian <- c("183", "183","183", "183","183", "183")


longtermtempplot <- allusgsdata2 %>% filter(Location == "Madras" & Year == 2008 | Year == 2009 & Location == "Madras"| Year == 1953 & Location == "Madras" |
                          Year == 1956 & Location == "Madras" | Year == 2016 & Location == "Madras" | Year == 2019 & Location == "Madras") %>%
  ggplot(aes(x = as.Date(Julian, origin = "1952-01-01"), y = `Mean Temperature`, color = Year)) + geom_line(show.legend = F) + 
  facet_wrap( ~ as.factor(Year), ncol = 2) + theme_bw() +
  scale_x_date(date_labels = "%b") + ggtitle("Temperature Before and After Dam Installation") + labs(x = "Date") + 
  theme(axis.title.y = element_text(color = temperatureColor, size = 13), 
        axis.title.x = element_text(color = fishColor, size = 13), 
        plot.title = element_text(hjust = 0.5))
colorset = c('1953' = "red", '1956' = "red", '2008' = "goldenrod", '2009' = "goldenrod", '2016' = "forestgreen", '2019' = "forestgreen")
longtermtempplot + scale_fill_manual(values = colorset)

longtermtempplot + geom_text(
  data = MadrasDataMedians,
  mapping = aes(x = yday(x), y = 10, label = lab),
  show.legend = F
)

# Working here

# ggplot(allusgsdata2, aes(Date_time, `Mean Temperature`, color = Location)) + geom_line()












### FISH DATA
fishCountsSteelheadEstimated <- read.csv("EstimatedSteelheadODFW.csv")
fishCounts <- read.csv("adult counts deschutes PGE 2014-2020.csv")
fishCounts2 <- read.csv("RM43-steelhead-counts.csv")
fishCountsSteelhead <- fishCounts2 %>% select("BeginDate","EndDate","CountValue","TrendCom") %>% arrange(EndDate) # Actual captured rate



fishCounts3 <- read.csv("FallChinookODFW.csv")[1:43,1:7]
fishCounts3 <- gather(fishCounts3, Month, Count, June, July, August, September, October, -Total)
fishCounts3 <- mutate(fishCounts3, monthNum = case_when(grepl("June", Month) ~ "-06", grepl("July", Month) ~ "-07", 
                                                        grepl("August", Month) ~ "-08", 
                                                        grepl("September", Month) ~ "-09",  grepl("October", Month) ~ "-10"))
fishCounts3 <- fishCounts3 %>% mutate(Date_time = paste0(Year, monthNum))
fishCounts3$Date_time <- parse_date_time(fishCounts3$Date_time, orders = c("Y-m"))
fishCounts3$Date_time <- ymd(fishCounts3$Date_time)
fishCounts3$Total <- NULL
fishCounts3$monthNum <- NULL
colnames(fishCounts3)[3] <- "Fall Chinook"
fishCounts3$`Fall Chinook` <- as.numeric(fishCounts3$`Fall Chinook`)

fishCounts4 <- read.csv("HatcherySteelhead.csv")[1:43,1:7]
odfwmergedata <- fishCounts4 %>% select("Year", "Total")
fishCounts4 <- gather(fishCounts4, Month, Count, June, July, August, September, October, -Total)
fishCounts4 <- mutate(fishCounts4, monthNum = case_when(grepl("June", Month) ~ "-06", grepl("July", Month) ~ "-07", 
                                                        grepl("August", Month) ~ "-08", 
                                                        grepl("September", Month) ~ "-09",  grepl("October", Month) ~ "-10"))
fishCounts4 <- fishCounts4 %>% mutate(Date_time = paste0(Year, monthNum))
fishCounts4$Date_time <- parse_date_time(fishCounts4$Date_time, orders = c("Y-m"))
fishCounts4$Date_time <- ymd(fishCounts4$Date_time)
fishCounts4$Total <- NULL
fishCounts4$monthNum <- NULL
colnames(fishCounts4)[3] <- "Hatchery Summer Steelhead"
fishCounts4$`Hatchery Summer Steelhead` <- as.numeric(fishCounts4$`Hatchery Summer Steelhead`)

fishCounts5 <- read.csv("WildSteelhead.csv")[1:43,1:7]
odfwmergedata <- odfwmergedata %>% left_join(select(fishCounts5, Total, Year), by = "Year")
colnames(odfwmergedata) <- c("Year","Total Number of Captured Hatchery Summer Steelhead", "Number of Captured Wild Summer Steelhead")
odfwmergedata <- odfwmergedata %>% mutate_all(funs(as.numeric(gsub(",", "", .))))
fishCounts5 <- gather(fishCounts5, Month, Count, June, July, August, September, October, -Total)
fishCounts5 <- mutate(fishCounts5, monthNum = case_when(grepl("June", Month) ~ "-06", grepl("July", Month) ~ "-07", 
                                                        grepl("August", Month) ~ "-08", 
                                                        grepl("September", Month) ~ "-09",  grepl("October", Month) ~ "-10"))
fishCounts5 <- fishCounts5 %>% mutate(Date_time = paste0(Year, monthNum))
fishCounts5$Date_time <- parse_date_time(fishCounts5$Date_time, orders = c("Y-m"))
fishCounts5$Date_time <- ymd(fishCounts5$Date_time)
fishCounts5$Total <- NULL
fishCounts5$monthNum <- NULL
colnames(fishCounts5)[3] <- "Wild Summer Steelhead"
fishCounts5$`Wild Summer Steelhead` <- as.numeric(fishCounts5$`Wild Summer Steelhead`)

ODFWData <- fishCounts3 %>% left_join(fishCounts4) %>% left_join(fishCounts5) %>% arrange(Date_time)
ODFWData <- ODFWData %>% mutate(Season = getSeason(Date_time))
ODFWData <- ODFWData[,c(4,1,2,7,3,5,6)] #Reordering data, note that overwhelming majority of fish occurrence is in summer, about a third as much in fall - basically none in spring



#Plot by season and species of ODFW Data before merge
ODFWData %>% gather(Variable, Value, -c("Date_time", "Year","Season","Month")) %>%
  ggplot(aes(Date_time, Value, color = Variable)) + geom_line() + facet_wrap(Variable ~ Season) + theme_bw() +
  ggtitle("Monthly ODFW Data at Sherar Falls") + labs(x = "Date", y = "Fish Count", color = "Species") + 
  theme(axis.title.y = element_text(color = temperatureColor, size = 13), 
        axis.title.y.right = element_text(color = fishColor, size = 13), 
        plot.title = element_text(hjust = 0.5))


fishCounts$Date <- mdy(fishCounts$Date)
fishCounts$Year <- year(fishCounts$Date)
fishCounts$Season <- getSeason(fishCounts$Date)
fishCounts <- fishCounts %>% mutate(Month = month(Date, label = T, abbr = F))

fishCounts %>% gather(Variable, Value, -Date, -Year, -Season) %>% filter(Variable != "Total") %>% 
  ggplot(aes(Date, Value, color = Variable)) + geom_point() + geom_line() + facet_grid(Year ~ Variable)


#ANOVA TEST
prob <- fishCounts %>% gather(Variable, Count, - Date, -Year, -Season) %>% aov(Count ~ Variable, .)
summary(prob)

fishandtempdfMadras <- allusgsdata2 %>% filter(Location == "Madras") %>%
  inner_join(fishCounts, by = c("Date_time" = "Date", "Season", "Year"))
fishandtempdfMoody <- allusgsdata2 %>% filter(Location == "Moody") %>% 
  inner_join(fishCounts, by = c("Date_time" = "Date", "Season", "Year"))
fishandtempdfMadras <- fishandtempdfMadras %>% 
  select(-c("Agency", "Site", "Discharge (cfs)", "lat", "long", "Max Temperature", "Min Temperature", "Julian", "Location"))
colnames(fishandtempdfMadras) <- c("Date_time","Mean Temperature","Year","Season","Hatchery Summer Steelhead","Wild Summer Steelhead",
                                   "Summer Steelhead RM", "Summer Steelhead LM", "Hatchery Spring Chinook", "Wild Spring Chinook",
                                   "Spring Chinook RM", "Spring Chinook LM","No Mark Sockeye", "Sockeye RM", "Sockeye LM", "Fall Chinook",
                                   "Bull Trout", "Rainbow Trout", "Total")
fishandtempdfMadras <- fishandtempdfMadras %>% mutate(Month = month(Date_time, label = T, abbr = F))

ODFWData1 <- ODFWData %>% filter(Year < 2014) %>% mutate(Source = "ODFW")
fishCounts <- fishCounts %>% mutate(Source = "PGE")
fullfishandtemp <- fishCounts %>% 
  full_join(ODFWData1, by = c("Date" = "Date_time","Season","Year","Month","Source",
                              "SUMMER.STEELHEAD_Hatchery" = "Hatchery Summer Steelhead", "SUMMER.STEELHEAD_Wild" = "Wild Summer Steelhead",
                              "Fall.Chinook" = "Fall Chinook")) %>% arrange(Date)
colnames(fullfishandtemp) <- c("Date_time","Hatchery Summer Steelhead","Wild Summer Steelhead",
                                   "Summer Steelhead RM", "Summer Steelhead LM", "Hatchery Spring Chinook", "Wild Spring Chinook",
                                   "Spring Chinook RM", "Spring Chinook LM","No Mark Sockeye", "Sockeye RM", "Sockeye LM", "Fall Chinook",
                                   "Bull Trout", "Rainbow Trout", "Total", "Year", "Season", "Month", "Source")

allusgsdata3 <- allusgsdata2 %>% filter(Location == "Madras") %>% select("Date_time","Mean Temperature") 
fullfishandtemp1 <- fullfishandtemp %>% left_join(allusgsdata3, by = "Date_time") %>% arrange(Date_time)
test1 <- fullfishandtemp1 %>% group_by(Date_time) %>% filter(n() > 1) %>% summarize(n = n())

fullfishandtemp1 %>% gather(Variable, Value, -c("Date_time", "Year","Season","Month")) %>% filter(Variable == "Fall Chinook" | 
                                                                                                    Variable == "Hatchery Summer Steelhead" |
                                                                                                    Variable == "Wild Summer Steelhead") %>%
  ggplot(aes(Date_time, Value, color = Variable)) + geom_line() + geom_smooth(se = F, method = "lm",
                                                                              formula = y ~ x + I(x^2) + I(x^3)) + facet_wrap( ~ Variable) + theme_bw() +
  ggtitle("Fish Count data by Species") + labs(x = "Date", y = "Fish Count", color = "Species") + 
  theme(axis.title.y = element_text(color = temperatureColor, size = 13), 
        axis.title.y.right = element_text(color = fishColor, size = 13), 
        plot.title = element_text(hjust = 0.5)) + stat_poly_eq(formula = y ~ x + I(x^2) + I(x^3),aes(label = ..adj.rr.label..), parse = T)

summary(lm(`Hatchery Summer Steelhead` ~ poly(Year,2), data = fullfishandtemp1))
summary(lm(`Hatchery Summer Steelhead` ~ poly(Year,3), data = fullfishandtemp1))


fishandtempdfMoody <- fishandtempdfMoody %>% 
  select(-c("Agency", "Site", "Discharge (cfs)", "lat", "long", "Max Temperature", "Min Temperature", "Julian", "Location"))

fishandtempdfMadras %>% mutate(lFishCount = log(Total)) %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
  filter(Variable == "lFishCount" | Variable == "Mean Temperature") %>%
  ggplot(aes(Date_time, Value, color = Variable)) + geom_line() + facet_wrap( ~ Year, scales = "free") + 
  scale_color_manual(values = c("red","blue")) + labs(x = "Date", y = "Temperature")
coeff <- max(fishandtempdfMadras$Total, na.rm = T)/max(fishandtempdfMadras$`Mean Temperature`)
temperatureColor <- "#C92A2A"
fishColor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(data = fishandtempdfMadras, aes(x = Date_time)) + geom_line(aes(y = `Mean Temperature`), color = temperatureColor) +
  geom_line(aes(y = Total / coeff), color = fishColor) + scale_y_continuous(name = "Temperature (Celsius °)", 
                                                         sec.axis = sec_axis(~.*coeff, name = "Total Fish Count")) + theme_bw() +
  ggtitle("Temperature vs. Fish Count") + theme(axis.title.y = element_text(color = temperatureColor, size = 13),
                                               axis.title.y.right = element_text(color = fishColor, size = 13),
                                               plot.title = element_text(hjust = 0.5)) + xlab("Date")


# Graph with bar and line different axes and then different fish species
coeff2 <- fishandtempdfMadras %>% gather(Variable, Value, -Date_time, -Year, -Season, -`Mean Temperature`, -Month) %>% 
  group_by(Variable) %>%
  summarise(coeff = max(as.numeric(Value), na.rm = T)/15.4)
fishandtempdfMadras %>% gather(Variable, Value, -Date_time, -Year, -Season, -`Mean Temperature`, -Month) %>% 
  ggplot(aes(x = Date_time)) + geom_col(aes(y = Value, fill = Variable), show.legend = F) + facet_wrap( ~ Variable) +
  geom_line(aes(y = `Mean Temperature`), color = temperatureColor) +
  scale_y_continuous(name = "Temperature (Celsius °)", sec.axis = sec_axis(~.*10, name = "Fish Count")) +
  theme_bw() + labs(x = "Date") + ggtitle("Temperature and Fish Count by Species (PGE Data 2014-2020)") +
  theme(axis.title.y = element_text(color = temperatureColor, size = 13),
        axis.title.y.right = element_text(color = fishColor, size = 13),
        plot.title = element_text(hjust = 0.5))
# justforplotting <- fishandtempdfMadras %>% gather(Variable, Value, -Date_time, -Year, -Season, -`Mean Temperature`, -Month)
# justforplotting$Variable <- factor(justforplotting$Variable)
# ggplot(data = justforplotting, aes(x = Date_time, y = Value)) + geom_bar(stat = "identity") + 
#   facet_wrap( ~ Variable, scales = "free") + theme_bw()

# Linear models for Madras and Moody
fishtempmodelMadras <- lm(log(Total) ~ `Mean Temperature`, data = fishandtempdfMadras)
fishtempmodelMoody <- lm(Total ~ `Mean Temperature`, data = fishandtempdfMoody)
summary(fishtempmodelMadras) # For every 2.53 degree celsius decrease in temperature there is a corresponding increase of 1 total fish count
summary(fishtempmodelMoody) # For every 1.9 degree celsius decrease in temperature there is a corresponding increase of 1 total fish count

formula <- log(y) ~ poly(x, raw = T)
ggplot(data = fishandtempdfMadras, aes(x = `Mean Temperature`, y = Total)) + geom_point() + geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..adj.rr.label..), formula = formula, parse = T)


totalsumoffish <- fishandtempdfMadras %>% group_by(Year) %>% select(-Date_time, -Season) %>% replace(is.na(.), 0) %>% summarise_all(funs(sum))
sumsoftotalfishsums <- totalsumoffish %>% summarise_all(funs(sum))
totalsumoffish <- rbind(totalsumoffish, sumsoftotalfishsums)
# Only significant numbers of fish rainbow trout, hatchery steelhead, hatchery spring chinook, fall chinook

fishCountsSteelhead <- fishCountsSteelhead %>% mutate(Year = year(BeginDate)) %>% select("Year","TrendCom","CountValue")
fishCountsSteelhead <- spread(fishCountsSteelhead, TrendCom, CountValue)
fishCountsSteelheadEstimated <- fishCountsSteelheadEstimated %>% mutate(Year = substr(Year, start = 1, stop = 4))
fishCountsSteelheadEstimated$Year <- as.numeric(fishCountsSteelheadEstimated$Year)
SteelheadODFWDF <- fishCountsSteelhead %>% right_join(fishCountsSteelheadEstimated, by = c("Year"))
SteelheadODFWDF <- SteelheadODFWDF %>% mutate_all(funs(as.numeric(gsub(",", "", .))))
colnames(SteelheadODFWDF) <- c("Year", "Number of Captured Wild Summer Steelhead",
                               "Number of Captured Round Butte Hatchery Summer Steelhead",
                               "Number of Captured Stray Hatchery Summer Steelhead",
                               "Total Number of Captured Hatchery Summer Steelhead",
                               "Estimated Wild Summer Steelhead", "Estimated Round Butte Hatchery Summer Steelhead",
                               "Estimated Stray Hatchery Summer Steelhead", "Estimated Total Hatchery Summer Steelhead")
odfwmergedata <- odfwmergedata %>% filter(Year > 2006)
# SteelheadODFWDF <- SteelheadODFWDF %>% replace(is.na(.), 0)
# SteelheadODFWDF <- SteelheadODFWDF %>% mutate_all(na_if, 0) go back and forth between na and 0

testdf <- full_join(SteelheadODFWDF, odfwmergedata, 
                    by = c("Year"))
testdf$`Number of Captured Wild Summer Steelhead.x`[is.na(testdf$`Number of Captured Wild Summer Steelhead.x`)] <- 
  testdf$`Number of Captured Wild Summer Steelhead.y`[!is.na(testdf$`Number of Captured Wild Summer Steelhead.y`)]
testdf$`Total Number of Captured Hatchery Summer Steelhead.x`[is.na(testdf$`Total Number of Captured Hatchery Summer Steelhead.x`)] <-
  testdf$`Total Number of Captured Hatchery Summer Steelhead.y`[!is.na(testdf$`Total Number of Captured Hatchery Summer Steelhead.y`)]
testdf$`Number of Captured Wild Summer Steelhead.y` <- NULL
testdf$`Total Number of Captured Hatchery Summer Steelhead.y` <- NULL
steelheadFinaldf <- testdf
colnames(steelheadFinaldf) <- c("Year", "Number of Captured Wild Summer Steelhead",
                               "Number of Captured Round Butte Hatchery Summer Steelhead",
                               "Number of Captured Stray Hatchery Summer Steelhead",
                               "Total Number of Captured Hatchery Summer Steelhead",
                               "Estimated Wild Summer Steelhead", "Estimated Round Butte Hatchery Summer Steelhead",
                               "Estimated Stray Hatchery Summer Steelhead", "Estimated Total Hatchery Summer Steelhead")




# Working here

#INITIAL STEELHEAD DATA PLOT
steelheadListInitial <- c("Year", "Proportion of Estimated to Captured Total Hatchery", "Proportion of Estimated to Captured Stray Hatchery",
                    "Proportion of Estimated to Captured Round Butte Hatchery", "Proportion of Estimated to Captured Wild")
steelheadListProp <- c("Year",
                       "Number of Captured Wild Summer Steelhead",
                       "Number of Captured Round Butte Hatchery Summer Steelhead",
                       "Number of Captured Stray Hatchery Summer Steelhead",
                       "Total Number of Captured Hatchery Summer Steelhead",
                       "Estimated Wild Summer Steelhead",
                       "Estimated Round Butte Hatchery Summer Steelhead",
                       "Estimated Stray Hatchery Summer Steelhead",
                       "Estimated Total Hatchery Summer Steelhead")


steelheadFinaldf %>% gather(Variable, Value, -steelheadListInitial) %>% 
  mutate(Estimates = case_when(grepl("Wild", Variable) ~ "Wild", grepl("Round Butte", Variable) ~ "Round Butte Hatchery",
         grepl("Total", Variable) ~ "Total Hatchery", grepl("Stray", Variable) ~ "Stray Hatchery")) %>% 
  ggplot(aes(x = Year, y = Value, color = Variable)) + facet_wrap( ~ Estimates) +
  geom_line() + geom_point() +
  ggtitle("Sherar Falls Trap Data") + theme_bw() + labs(y = "Fish Count") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
  
steelheadFinaldf <- steelheadFinaldf %>% mutate(`Proportion of Estimated to Captured Round Butte Hatchery` = `Number of Captured Round Butte Hatchery Summer Steelhead` / `Estimated Round Butte Hatchery Summer Steelhead`,
                           `Proportion of Estimated to Captured Total Hatchery` = `Total Number of Captured Hatchery Summer Steelhead` / `Estimated Total Hatchery Summer Steelhead`,
                           `Proportion of Estimated to Captured Stray Hatchery` = `Number of Captured Stray Hatchery Summer Steelhead` / `Estimated Stray Hatchery Summer Steelhead`,
                           `Proportion of Estimated to Captured Wild` = `Number of Captured Wild Summer Steelhead` / `Estimated Wild Summer Steelhead`)

steelheadFinaldf %>% gather(Variable, Value, -steelheadListProp) %>% ggplot(aes(x = Year, y = Value, color = Variable)) + 
  geom_line(show.legend = F) + 
  geom_point(show.legend = F) + facet_wrap( ~ Variable) +
  ggtitle("Sherar Falls Trap Data by Proportions")

### READING IN ODEQ Data - mostly useless
# allodeqData <- read.csv("Standard Export 11365.csv")
# 
# allodeqData <- allodeqData %>% select("Result.Value", "Result.Unit", "Characteristic.Name","Monitoring.Location.Name",
#                                       "Monitoring.Location.Latitude",	"Monitoring.Location.Longitude","Activity.Start.Date")
# allodeqData <- allodeqData %>% mutate(new = paste(Characteristic.Name, "in", Result.Unit))
# allodeqData <- subset(allodeqData, select = -c(Characteristic.Name, Result.Unit))
# allodeqData <- as.data.frame(sapply(allodeqData, gsub, pattern = "<|>", replacement = ""))
# allodeqData$Result.Value <- as.numeric(as.character(allodeqData$Result.Value))
# allodeqData1 <- pivot_wider(allodeqData, names_from = new, values_from = Result.Value, values_fn = max)
# colnames(allodeqData1) <- c("Location","Lat","Long","Date_time","pH","Dissolved Oxygen % Saturation","Temperature","Dissolved Oxygen mg/l",
#                             "Biochemical Oxygen Demand", "Total Coliform", "Total Solids", "Ammonia", "Nitrate + Nitrite",
#                             "Escherichiac in cfu/100ml", "Escherichia in MPN/100ml")
# allodeqData1$Date_time <- mdy(allodeqData1$Date_time)
# allodeqData1 <- allodeqData1 %>% mutate(Year = year(Date_time))
# 
# allodeqData1 %>% filter(Location == "Deschutes River at Deschutes River Park" | Location == "John Day River at Hwy 206" | 
#                           Location == "Deschutes River at Maupin") %>%
#   ggplot(aes(x = Date_time, y = Temperature, color = Location)) + geom_point(show.legend = F) +
#   geom_line(show.legend = F) + facet_wrap( ~ Location, ncol = 1)


### Reading in new PGE data
path <- "Rereg Sonde Data 2004-2006_ 2010-2017.xlsx"
mad <- path %>% excel_sheets() %>% set_names() %>% map_df(read_excel, path = path)
mad$Time <- as.character(mad$Time)
mad <- mad %>% mutate(Time = sub(".* ", "", Time))


# test <- mad[is.na(mad$Time), ] 2010-07-31,2011-05-08,2015-05-01,2015-11-02 don't have times
newpgeData <- mad %>% mutate(Date_time = paste(Date, Time))
newpgeData$Date_time <- ymd_hms(newpgeData$Date_time)
newpgeData <- subset(newpgeData, select = -c(Date, Time))
colnames(newpgeData) <- c("Temperature", "Dissolved Oxygen mg/l", "Dissolved Oxygen % Saturation", "pH", "Date_time")
ggplot(newpgeData, aes(x = Date_time, y = Temperature)) + geom_line(color = "blue") + stat_peaks(color = "red", ignore_threshold = 0.6) + 
  annotate(geom = "point", x = as.POSIXct("2008-04-01"), y = 9, size = 50, shape = 21, fill = "transparent") +
  annotate(geom = "text", x = as.POSIXct("2008-04-01"), y = 13, label = "No Data Collected") + 
  stat_peaks(geom = "text", color = "red", vjust = -1, span = 25, ignore_threshold = 0.58, angle = 20, hjust = 1) + theme_bw() + 
  labs(y = "Temperature (Celsius °)", x = "Date") +
  ggtitle("PGE Data at Pelton Round Butte Dam on Temperature") + theme(plot.title = element_text(hjust = 0.5)) # Dates highlighted
ggplot(newpgeData, aes(x = Date_time, y = Temperature)) + geom_line(color = "blue") + stat_peaks(color = "red", ignore_threshold = 0.6) + 
  annotate(geom = "point", x = as.POSIXct("2008-04-01"), y = 9, size = 50, shape = 21, fill = "transparent") +
  annotate(geom = "text", x = as.POSIXct("2008-04-01"), y = 13, label = "No Data Collected") +
  stat_peaks(geom = "text", color = "red", vjust = -0.5, span = 25, ignore_threshold = 0.58, y.label.fm = "%f°", angle = 20, hjust = 1,
             aes(label = paste(..y.label..))) + 
  theme_bw() + 
  labs(y = "Temperature (Celsius °)", x = "Date") +
  ggtitle("PGE Data at Pelton Round Butte Dam on Temperature") + theme(plot.title = element_text(hjust = 0.5)) # Temperatures highlighted, measured by 
# difference, ignore_threshold at 0.6 meaning if a peak is within 60% of the data on either side it will be ignored

# df6 <- df5 %>% filter(Location == "Reregulating Dam")
# testforsimilarity <- allusgsdata2 %>% filter(Location == "Madras", Year > 2003) 
# ggplot() + geom_line(data = newpgeData, aes(x = Date_time, y = Temperature), color = "forestgreen") +
#   geom_line(data = df6, aes(x = Date_time, y = Temperature), color = "red") + 
#   geom_line(data = testforsimilarity, aes(x = as.POSIXct(Date_time), y = `Mean Temperature`), color = "blue")
# Showing that PGE data is essentially the same from 2015-2017 and new data and USGS data









