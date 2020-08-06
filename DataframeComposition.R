### List of data sets
## Water Temperature
# PGE Data from Erica - 2004-2006 and 2010-2017 observations every hour (includes pH and dissolved oxygen)
# PGE Data from website - 2015-2017
# USGS Data from Madras, Culver, and Moody by hour, USGS data is similar but not the exact same as PGE
# ODEQ Data from 1962-2020 on Deschutes River at Deschutes River Park, 1967-1975 at Maupin, and 1973-2020 at John Day River
## Air Temperature
# Data from NOAA from 2000-2020 at Pelton Dam, Moody, Madras, and Maupin
## Fish Counts
# Data from PGE 2014-2020 at Pelton Dam
# Data from ODFW on hatchery and wild steelhead as well as fall chinook,
# Data from that one website at Sherar Falls that is also ODFW data, collected by Jason and Rod

## PGE Data from Erica
path <- "Rereg Sonde Data 2004-2006_ 2010-2017.xlsx"
mad <- path %>% excel_sheets() %>% set_names() %>% map_df(read_excel, path = path)
mad$Time <- as.character(mad$Time)
mad <- mad %>% mutate(Time = sub(".* ", "", Time))
# test <- mad[is.na(mad$Time), ] 2010-07-31,2011-05-08,2015-05-01,2015-11-02 don't have times
newpgeData <- mad %>% mutate(Date_time = paste(Date, Time))
newpgeData$Date_time <- ymd_hms(newpgeData$Date_time)
newpgeData <- subset(newpgeData, select = -c(Date, Time))
colnames(newpgeData) <- c("Temperature", "Dissolved Oxygen mg/l", "Dissolved Oxygen % Saturation", "pH", "Date_time")
newpgeData <- newpgeData %>% mutate(Season = getSeason(Date_time)) %>% mutate(Julian = yday(Date_time)) %>% mutate(Year = year(Date_time))

## PGE Data from website
df <- read_excel("pge-water-chemistry-2015-2017.xlsx")
df2 <- df %>% mutate(new = paste(Parameter, "in", Units))
# Deleting columns that were just pasted
df3 <- subset(df2, select = -c(Units, Parameter))
# Subsetting out what appears to be additional measurements that are similar enough to be irrelevant
df4 <- df3[-c(2525,2524,2253,2254,1982,1983,1711,1712,1441,1442),]

# Spreading data by Value and then renaming the columns, deleting station ID
oldpgeData <- spread(df4, new, Value)
oldpgeData$`Station ID` <- NULL
colnames(oldpgeData)[7] <- c("Temperature")
colnames(oldpgeData)[1] <- c("Location")
colnames(oldpgeData)[2] <- c("Date_time")


## USGS Data
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
allusgsdata2 <- subset(allusgsdata2, select = -c(Agency, Site, `Min Temperature`, `Max Temperature`)) # Run if you don't want min or max temp
colnames(allusgsdata2)[2] = "Temperature"
MadrasMergeData <- allusgsdata2 %>% filter(Location == "Madras") %>% select(-`Discharge (cfs)`)



## ODEQ Data
allodeqData <- read.csv("Standard Export 11365.csv")

allodeqData <- allodeqData %>% select("Result.Value", "Result.Unit", "Characteristic.Name","Monitoring.Location.Name",
                                      "Monitoring.Location.Latitude",	"Monitoring.Location.Longitude","Activity.Start.Date")
allodeqData <- allodeqData %>% mutate(new = paste(Characteristic.Name, "in", Result.Unit))
allodeqData <- subset(allodeqData, select = -c(Characteristic.Name, Result.Unit))
allodeqData <- as.data.frame(sapply(allodeqData, gsub, pattern = "<|>", replacement = ""))
allodeqData$Result.Value <- as.numeric(as.character(allodeqData$Result.Value))
allodeqData1 <- pivot_wider(allodeqData, names_from = new, values_from = Result.Value, values_fn = max)
colnames(allodeqData1) <- c("Location","Lat","Long","Date_time","pH","Dissolved Oxygen % Saturation","Temperature","Dissolved Oxygen mg/l",
                            "Biochemical Oxygen Demand", "Total Coliform", "Total Solids", "Ammonia", "Nitrate + Nitrite",
                            "Escherichiac in cfu/100ml", "Escherichia in MPN/100ml")
allodeqData1$Date_time <- mdy(allodeqData1$Date_time)
allodeqData1 <- allodeqData1 %>% mutate(Year = year(Date_time))
allodeqDataFinal <- allodeqData1 %>% select(-c(Lat, Long)) %>% mutate(Season = getSeason(Date_time), Julian = yday(Date_time))

## ODFW Data


