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

# John Day Data analysis
lm1 <- lm(pHOSObserved ~ log(Num_H) + NOSA, data = JohnDayBargeData)
lm2 <- lm(pHOSObserved ~ `PercentHBarged` + NOSA, data = JohnDayBargeData) #Current model
stargazer(lm1, lm2, type = "text") # Check slide 20 for source on regression

# Testing for difference in season by Predam, PreSWW, PostSWW groupings
MadrasDataYearly <- MadrasData %>% group_by(Year, Season) %>% summarise(Temperature = mean(Temperature)) %>% 
  mutate(Group = case_when(Year <= 1968 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MadrasDataYearly$Group <- as.factor(MadrasDataYearly$Group)
MadrasDataYearly$Group <- factor(MadrasDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW"))

ggplot(data = MadrasDataYearly, aes(x = Year, y = Temperature)) + geom_smooth(method = "lm", formula = formula, se = F) +
  geom_line(aes(color = Season)) + facet_grid(Season ~ Group, scales = "free") + 
  stat_poly_eq(aes(label = paste(..rr.label..)), formula = formula, parse = T) #Redo with rolling 7 day average maximum START HERE

MadrasDataYearlyFall <- MadrasDataYearly %>% filter(Season == "Fall")
MadrasDataYearlyWinter <- MadrasDataYearly %>% filter(Season == "Winter")
MadrasDataYearlySpring <- MadrasDataYearly %>% filter(Season == "Spring")
MadrasDataYearlySummer <- MadrasDataYearly %>% filter(Season == "Summer")

# Summarized individually
summary(lm(Temperature ~ Group, data = MadrasDataYearlyFall))
summary(lm(Temperature ~ (Group), data = MadrasDataYearlyWinter))
summary(lm(Temperature ~ (Group), data = MadrasDataYearlySpring))
summary(lm(Temperature ~ (Group), data = MadrasDataYearlySummer))

Falllm <- lm(Temperature ~ Group, data = MadrasDataYearlyFall)
Winterlm <- lm(Temperature ~ (Group), data = MadrasDataYearlyWinter)
Springlm <- lm(Temperature ~ (Group), data = MadrasDataYearlySpring)
Summerlm <- lm(Temperature ~ (Group), data = MadrasDataYearlySummer)


# View all at once
stargazer(Falllm,Winterlm,Springlm,Summerlm, type = "text") # Order here is 1:Fall,2:Winter,3:Spring,4:Summer









### PLOTS

# Plots rainbow trout, hatchery steelhead, hatchery spring chinook, fall chinook for PGE Data by Season and Year
PGEFishDataGathered <- PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month)
PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
  filter(Variable == c("Hatchery Summer Steelhead","Hatchery Spring Chinook", "Fall Chinook", "Rainbow Trout")) %>%
  ggplot(aes(Season, as.numeric(Value), color = Variable, fill = Variable)) + geom_col() + facet_grid(Variable ~ Year) +
  theme_bw() + ggtitle("PGE Fish Count Data") + labs(y = "Number of Fish Captured") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

#Overplot of all variables by Season and Year from PGE data
PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
  ggplot(aes(Season, as.numeric(Value), color = Variable, fill = Variable)) + geom_col() + facet_grid(Variable ~ Year) +
  theme_bw() + ggtitle("PGE Fish Count Data") + labs(y = "Number of Fish Captured") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


# Plot of Fall and Summer ODFW Fish Data by Column
formula <- y ~ x + I(x^2)
MergedFishData %>% filter(Year < 2014 & Season != "Spring") %>% group_by(Year) %>% ggplot(aes(x = Year, y = Total, fill = Season, color = Season)) +
  geom_col(show.legend = F, position = "dodge") + geom_smooth(method = "lm", formula = formula, show.legend = F, color = "black") +
  geom_vline(aes(xintercept = 2014), linetype = "dashed") +
  facet_wrap( ~ Season) +
  stat_poly_eq(aes(label = ..eq.label..), method = "lm", parse = T, formula = formula)

# Season interaction term plot shows the lack of data we are struggling with
ggplot(data = lmdata, aes(x = Year, y = Total, color = Season)) + geom_point(aes(x = Year, y = Total)) +
  geom_point(aes(x = Year, y = `Temperature`), color = "red", size = 3) +
  geom_smooth(method = "lm", se = F, formula = formula) + facet_wrap( ~ Season) + geom_vline(aes(xintercept = 2010)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T, angle = -30)

# Plot of pHOSObserved vs. log of number of hatchery barged
formula = y ~ x + I(x^2)
ggplot(JohnDayBargeData, aes(log(Num_H), pHOSObserved)) + geom_point() + geom_smooth(method = "lm", formula = formula, se = F) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = formula, parse = T)

# Plot of proportion of fish barged vs HSS from ODFW Data Yearly
ggplot(testdf2, aes(Proportion, HSS)) + geom_point() + geom_smooth(method = "lm", formula = formula, se = F) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T) #Figure out how to rebuild testdf2

# Plot of ODFW HSS Yearly Numbers by Year vs Bonneville Barged Numbers
BonnevilleDatavsODFW <- BonnevilleData %>% left_join(ODFWDataYearly, by = c("Year"))
ggplot(data = BonnevilleDatavsODFW) + geom_line(aes(as.Date(paste0(Year, "-01-01")), ActualHSS), color = "red") + 
  geom_point(aes(as.Date(paste0(Year, "-01-01")),ActualHSS), color = "red") + 
  geom_point(aes(as.Date(paste0(Year, "-01-01")),Hatchery), color = "black") + 
  geom_line(aes(as.Date(paste0(Year, "-01-01")),Hatchery), color = "black") # ActualHSS is ODFW Count Hatchery is Bonneville Barged Numbers

# Plot of ODFW Yearly vs Hatchery counts from Bonneville data
formula = y ~ x + I(x^2)
ggplot(data = BonnevilleDatavsODFW, aes(Hatchery, ActualHSS)) + geom_point() + geom_smooth(method = "lm", se = F, formula = formula) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = formula, parse = T) 

# Plot of Hatchery vs Fall Chinook, negatively associated as expected
ggplot(data = BonnevilleDatavsODFW, aes(Hatchery, ActualFC)) + geom_point() + geom_smooth(method = "lm", se = F, formula = y ~ x) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), formula = y ~ x, parse = T)




