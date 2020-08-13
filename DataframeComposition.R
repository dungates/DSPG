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
# path <- "Rereg Sonde Data 2004-2006_ 2010-2017.xlsx"
# mad <- path %>% excel_sheets() %>% set_names() %>% map_df(read_excel, path = path)
# mad$Time <- as.character(mad$Time)
# mad <- mad %>% mutate(Time = sub(".* ", "", Time))
# # test <- mad[is.na(mad$Time), ] 2010-07-31,2011-05-08,2015-05-01,2015-11-02 don't have times
# newpgeData <- mad %>% mutate(Date_time = paste(Date, Time))
# newpgeData$Date_time <- ymd_hms(newpgeData$Date_time)
# newpgeData <- subset(newpgeData, select = -c(Date, Time))
# colnames(newpgeData) <- c("Temperature", "Dissolved Oxygen mg/l", "Dissolved Oxygen % Saturation", "pH", "Date_time")
# newpgeData <- newpgeData %>% mutate(Season = getSeason(Date_time)) %>% mutate(Julian = yday(Date_time)) %>% mutate(Year = year(Date_time))

newpgeData <- read_csv("newpgeData.csv", col_types = cols(Season = col_factor()))


## PGE Data from website
# df <- read_excel("pge-water-chemistry-2015-2017.xlsx")
# df2 <- df %>% mutate(new = paste(Parameter, "in", Units))
# # Deleting columns that were just pasted
# df3 <- subset(df2, select = -c(Units, Parameter))
# # Subsetting out what appears to be additional measurements that are similar enough to be irrelevant
# df4 <- df3[-c(2525,2524,2253,2254,1982,1983,1711,1712,1441,1442),]

# Spreading data by Value and then renaming the columns, deleting station ID
# oldpgeData <- spread(df4, new, Value)
# oldpgeData$`Station ID` <- NULL
# colnames(oldpgeData)[7] <- c("Temperature")
# colnames(oldpgeData)[1] <- c("Location")
# colnames(oldpgeData)[2] <- c("Date_time")

oldpgeData <- read_csv("oldpgeData.csv")
oldpgeData$Date_time <- ymd(oldpgeData$Date_time)

## USGS Data
# MadrasGageData <- read.table("MadrasTemperatureData.txt", header = T, fill = T, sep = "\t")
# MoodyGageData <- read.table("MoodyTemperatureData.txt", header = T, fill = T, sep = "\t")
# 
# MadrasGageData <- MadrasGageData %>% mutate(Location = "Madras")
# MadrasGageData$X113433_00010_00001_cd <- NULL
# MadrasGageData$X113434_00010_00002_cd <- NULL
# MadrasGageData$X113435_00010_00003_cd <- NULL # Subsetting out approval columns since data is already quality controlled
# MadrasGageData$X113436_00060_00003_cd <- NULL
# colnames(MadrasGageData) <- c("Agency", "Site", "Date_time", "Max Temperature", "Min Temperature", "Mean Temperature", 
#                               "Discharge (cfs)", "Location")
# MadrasGageData <- MadrasGageData %>% mutate(`Mean Temperature` = case_when(is.na(`Mean Temperature`) ~ 
#                                                                              (`Max Temperature` + `Min Temperature`) / 2,
#                                                                            !is.na(`Mean Temperature`) ~ `Mean Temperature`))
# 
# MoodyGageData <- MoodyGageData %>% mutate(Location = "Moody")
# MoodyGageData$X113455_00010_00001_cd <- NULL
# MoodyGageData$X113456_00010_00002_cd <- NULL
# MoodyGageData$X113457_00010_00003_cd <- NULL
# MoodyGageData$X113458_00060_00003_cd <- NULL
# MoodyGageData$X265533_00010_00011_cd <- NULL
# colnames(MoodyGageData) <- c("Agency", "Site", "Date_time", "Max Temperature", "Min Temperature", "Mean Temperature", 
#                              "Discharge (cfs)", "Instantaneous Temperature", "Location")
# MoodyGageData <- MoodyGageData %>% mutate(`Mean Temperature` = coalesce(`Instantaneous Temperature`, `Mean Temperature`))
# MoodyGageData$`Instantaneous Temperature` <- NULL
# MoodyGageData <- MoodyGageData %>% mutate(`Mean Temperature` = case_when(is.na(`Mean Temperature`) ~ 
#                                                                            (`Max Temperature` + `Min Temperature`) / 2,
#                                                                          !is.na(`Mean Temperature`) ~ `Mean Temperature`))
# 
# allusgsdata2 <- rbind(MadrasGageData, MoodyGageData)
# allusgsdata2$Year <- four.digit.year(as.POSIXct(allusgsdata2$Date_time, format = "%m/%d/%y"), year = 1951) #lubridate is set in 1970 gotta transform data
# allusgsdata2$Date_time <- mdy(allusgsdata2$Date_time)
# allusgsdata2 <- allusgsdata2 %>% 
#   mutate(Date_time = case_when(year(Date_time) > 2021 ~ 'year<-'(Date_time, Year), TRUE ~ Date_time))
# allusgsdata2 <- allusgsdata2 %>% mutate(Season = getSeason(Date_time)) %>% mutate(Julian = yday(Date_time))
# allusgsdata2 <- subset(allusgsdata2, select = -c(Agency, Site, `Min Temperature`, `Max Temperature`)) # Run if you don't want min or max temp
# colnames(allusgsdata2)[2] = "Temperature"

USGSData <- read_csv("USGSData.csv", col_types = cols(Season = col_factor()))
# Just Madras temperature data which is largely what we use
MadrasMergeData <- USGSData %>% filter(Location == "Madras") %>% select(-`Discharge (cfs)`)
MoodyMergeData <- USGSData %>% filter(Location == "Moody") %>% select(-`Discharge (cfs)`)
MoodyMergeData <- MoodyMergeData %>% filter(!is.na(Temperature))
MadrasMergeData <- MadrasMergeData %>% filter(!is.na(Temperature))
#Could add PGE year 2004 here for slightly larger dataframe, below illustrates years of temperature data availability at Madras and Moody respectively
# ggplot(data = MadrasMergeData, aes(x = Date_time, y = Temperature)) + geom_line()
# ggplot(data = MoodyMergeData, aes(x = Date_time, y = Temperature)) + geom_line() 
## ODEQ Data
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
# allodeqDataFinal <- allodeqData1 %>% select(-c(Lat, Long)) %>% mutate(Season = getSeason(Date_time), Julian = yday(Date_time))
ODEQData <- read_csv("ODEQData.csv", col_types = cols(Season = col_factor()))


## ODFW Data
ODFWData <- read_csv("ODFWData.csv", col_types = cols(Season = col_factor()))

## PGE Fish Data
PGEFishData <- read_csv("PGEFishData.csv", 
                 col_names = c("Date_time", "Hatchery Summer Steelhead", "Summer Steelhead","Summer Steelhead RM", 
                               "Summer Steelhead LM", "Hatchery Spring Chinook", "Wild Spring Chinook","Spring Chinook RM", 
                               "Spring Chinook LM", "No Mark Sockeye", "Sockeye RM", "Sockeye LM", "Fall Chinook","Bull Trout", 
                               "Rainbow Trout", "Total", "Year", "Season", "Month"), 
                 col_types = cols(Season = col_factor(), Date_time = col_datetime()))[2:959,]
PGEFishData$Date_time <- ymd(PGEFishData$Date_time)

### ODFW John Day
JohnDayData <- read_csv("JohnDayReddCounts.csv")[1:13,1:17]
# For data merge purposes
JohnDayData <- JohnDayData %>% gather(Year, Value, -`Stream Name`, -`Site ID`)
#Yearly total
JohnDayData <- JohnDayData %>% group_by(Year) %>% mutate(Total = sum(Value, na.rm = T))
ggplot(data = JohnDayData, aes(x = as.Date(paste0(Year, "-10-01")), y = Total, color = `Stream Name`)) + geom_line() + geom_point()


### Ian Tatam John Day ODFW Data
JohnDayData2 <- read_csv("JohnDayBargeRates.csv", skip = 1)[,1:14]
colnames(JohnDayData2) <- c("Year","W_Observed","H_Observed","pHOSObserved","W_Captured","H_Captured","%H_Captured","NOSA",
                            "W_JDD","H_JDD","%H_JDD","PercentWBarged","PercentHBarged","Num_H","")
JohnDayData2 <- JohnDayData2 %>% mutate(Line = Num_H / 10)
JohnDayData3 <- JohnDayData2 %>% gather(Measure, Value, -Year)
JohnDayData3 %>% filter(Measure != "#H", Measure != "Num_H") %>%
  ggplot(aes(x = as.numeric(Year), y = Value, color = Measure)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(aes(label = Measure),
            data = JohnDayData3 %>% filter(as.numeric(Year) == 2018 & Measure != "Num_H"),
            color = "black",
            hjust = 0,
            size = 3,
            nudge_x = 0.5) + guides(color = "none") + coord_cartesian(xlim = c(2003, 2022))




## Merged fish data with temperature
MergedFishData <- read.csv("AllFishData.csv")
MergedFishData$X <- NULL
MergedFishData$Total <- rowSums(MergedFishData[,2:16], na.rm = T)

# Run a regression that compares fish data pre-sww to post-sww from ODFWData
# MadrasOLS <- MadrasMergeData %>% group_by(Year, Season) %>% summarize(`Temperature` = median(Temperature, na.rm = T))
# MoodyOLS <- MoodyMergeData %>% group_by(Year, Season) %>% summarize(`Median Seasonal Temperature` = median(Temperature))
# ols2data <- ODFWData %>% group_by(Year, Season) %>% summarize(`Fall Chinook` = sum(`Fall Chinook`),
#                                                               `Hatchery Summer Steelhead` = sum(`Hatchery Summer Steelhead`),
#                                                               `Wild Summer Steelhead` = sum(`Wild Summer Steelhead`))
# lmdata <- MadrasOLS %>% left_join(ols2data, by = c("Year","Season")) %>% filter(Year > 1976 & Season != "Winter" & Year != 2017 &
#                                                                                   Year != 2020)
# lmdata2 <- MoodyOLS %>% left_join(ols2data, by = c("Year","Season")) %>% filter(Year > 1976)
# lmdata$Total <- rowSums(lmdata[,4:6], na.rm = T)
# lmdata2$Total <- rowSums(lmdata[,4:6], na.rm = T)
# formula <- y ~ x



# basiclm <- lm(Total ~ `Temperature`*as.factor(Season), data = lmdata)
# summary(basiclm)
# # 
# fixed <- plm(Total ~ Temperature,
#              data = lmdata, index = c("Season", "Year"), model = "within")
# fixed.time <- plm(Total ~ Temperature + I(Temperature^2) + factor(Year) - 1,
#                   data = lmdata, index = c("Season", "Year"), model = "within")
# summary(fixed.time)
# pFtest(fixed.time, fixed)
# plmtest(fixed, c("time"), type = "bp")




# Curious about relationship between water temperature and other variables like pH and Dissolved Oxygen
# summary(lm(Temperature ~ pH + `Dissolved Oxygen % Saturation`, data = ODEQData))
# ggplot(data = ODEQData, aes(x = pH, y = Temperature)) + geom_point() + geom_smooth(method = "lm", formula = formula) + 
#   stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T)




#Plots rainbow trout, hatchery steelhead, hatchery spring chinook, fall chinook for PGE Data by Season and Year
# PGEFishDataGathered <- PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month)
# PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
#   filter(Variable == c("Hatchery Summer Steelhead","Hatchery Spring Chinook", "Fall Chinook", "Rainbow Trout")) %>%
#   ggplot(aes(Season, as.numeric(Value), color = Variable, fill = Variable)) + geom_col() + facet_grid(Variable ~ Year) +
#   theme_bw() + ggtitle("PGE Fish Count Data") + labs(y = "Number of Fish Captured") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
#         plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank())

#Overplot of all variables by Season and Year from PGE data
# PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
#   ggplot(aes(Season, as.numeric(Value), color = Variable, fill = Variable)) + geom_col() + facet_grid(Variable ~ Year) +
#   theme_bw() + ggtitle("PGE Fish Count Data") + labs(y = "Number of Fish Captured") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
#         plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank())


# Plot of Fall and Summer ODFW Fish Data by Column
# formula <- y ~ x + I(x^2)
# MergedFishData %>% filter(Year < 2014 & Season != "Spring") %>% group_by(Year) %>% ggplot(aes(x = Year, y = Total, fill = Season, color = Season)) +
#   geom_col(show.legend = F, position = "dodge") + geom_smooth(method = "lm", formula = formula, show.legend = F, color = "black") +
#   geom_vline(aes(xintercept = 2014), linetype = "dashed") +
#   facet_wrap( ~ Season) +
#   stat_poly_eq(aes(label = ..eq.label..), method = "lm", parse = T, formula = formula)

# Season interaction term plot shows the lack of data we are struggling with
# ggplot(data = lmdata, aes(x = Year, y = Total, color = Season)) + geom_point(aes(x = Year, y = Total)) +
#   geom_point(aes(x = Year, y = `Temperature`), color = "red", size = 3) +
#   geom_smooth(method = "lm", se = F, formula = formula) + facet_wrap( ~ Season) + geom_vline(aes(xintercept = 2010)) +
#   stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T, angle = -30)


# Structural break in a time series, test for break in progression that lines up with installation of SWW, allow for different intercept
# term after installation, dummy variable equal to 1 if after 2010, and 0 if before
# Put dummy variable in regression and see if statistically significanct and/or coefficient is bigger
# Says there is a different level in outcome of before and after controlling for quadratic trend
# Interact dummy variable with months, tells if difference in trend after installation

# allusgsdata3 <- allusgsdata2 %>% filter(Location == "Madras") %>% select("Date_time","Mean Temperature") 
# MergedFishData <- MergedFishData %>% left_join(allusgsdata3, by = "Date_time") %>% arrange(Date_time) Run if you want temperature data as well, left_join recommended





### Recreating Proportion slide 21 from presentation, regress with deschutes population
Year <- c(1997:2012)
Proportion <- c(275,364,353,341,414,254,277,406,383,239,115,120,145,90,156,31)
80/405
RealDF <- data.frame(Year, Proportion)
RealDF <- RealDF %>% mutate(Proportion = (Proportion * 0.1975309) + 16.642)
testdf <- RealDF %>% left_join(ODFWData, by = c("Year"))
testdf2 <- testdf %>% distinct(HSS, .keep_all = T)
summary(lm(HSS ~ I(Proportion^2), data = testdf2))
formula = y ~ I(x^2)
ggplot(testdf2, aes(Proportion, HSS)) + geom_point() + geom_smooth(method = "lm", formula = formula, se = F) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T)
w
lm1 <- lm(pHOSObserved ~ log(Num_H) + `PercentHBarged`, data = JohnDayData2)
lm2 <- lm(pHOSObserved ~ log(Num_H) + `PercentWBarged`, data = JohnDayData2)
lm3 <- lm(pHOSObserved ~ propTransported + log(num_H), data = JohnDayData2) # We still need data for proportion transported here
stargazer(lm1, lm2, type = "text") # Check slide 20 for source on regression
# Next going to use ODFWData and Summer Hatchery Steelhead correlated with proportion transported from McCann et al. 
# 
