library(plyr)
library(tidyr)
library(naniar)
library(ggpmisc)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(gridExtra)
library(gtable)
library(grid)
library(lubridate)
library(readr)
library(broom)
library(hydrostats)
library(stargazer)
library(GGally)
library(zoo)
library(kableExtra)
library(knitr)
library(reactable)
library(htmlwidgets)
library(webshot)
library(htmltools)
library(formattable)
setwd("~/DSPG")

# Hourly PGE data
HourlyPGEData <- read_csv("Data/newpgeData.csv", col_types = cols(Season = col_factor()))

# Daily USGS data
USGSData <- read_csv("Data/AllUSGSData.csv", col_types = cols(Season = col_factor()))
MadrasData <- USGSData %>% filter(Location == "Madras") %>% select(-`Discharge (cfs)`)
MoodyData <- USGSData %>% filter(Location == "Moody") %>% select(-`Discharge (cfs)`)
CulverData <- USGSData %>% filter(Location == "Culver") %>% select(-`Discharge (cfs)`)

# ODFW Fish Count data (Monthly and Yearly)
ODFWDataMonthly <- read_csv("Data/ODFWData.csv", col_types = cols(Season = col_factor()))
ODFWDataYearly <- read_csv("Data/ODFWDataYearly.csv") # Actual is total directly from ODFW, Total is from sum of monthly provided data

# PGE Fish Count data (Daily 2014-2020)
PGEFishData <- read_csv("Data/PGEFishData.csv", 
                        col_names = c("Date_time", "Hatchery Summer Steelhead", "Summer Steelhead","Summer Steelhead RM", 
                                      "Summer Steelhead LM", "Hatchery Spring Chinook", "Wild Spring Chinook","Spring Chinook RM", 
                                      "Spring Chinook LM", "No Mark Sockeye", "Sockeye RM", "Sockeye LM", "Fall Chinook","Bull Trout", 
                                      "Rainbow Trout", "Total", "Year", "Season", "Month"), 
                        col_types = cols(Season = col_factor(), Date_time = col_datetime()))[2:959,]
PGEFishData$Date_time <- ymd(PGEFishData$Date_time)

# ODEQ Water Quality parameters data
ODEQData <- read_csv("Data/ODEQData.csv", col_types = cols(Season = col_factor()))






# John Day Data
JDReddsCountData <- read_csv("Data/JohnDayReddCounts.csv")[1:13,1:17]

JohnDayBargeData <- read_csv("Data/JohnDayBargeRates.csv", skip = 1)[,1:14]
colnames(JohnDayBargeData) <- c("Year","W_Observed","H_Observed","pHOSObserved","W_Captured","H_Captured","%H_Captured","NOSA",
                            "W_JDD","H_JDD","%H_JDD","PercentWBarged","PercentHBarged","Num_H","")

# Bonneville Dam Data
BonnevilleData <- read_csv("Data/BonnevilleDamData.csv")
BonnevilleData2 <- read_csv("Data/bonneville fish .csv", col_types = cols(Year = col_integer()))[1:82,]
















### ANALYSIS

# ODFW Fish Data Analysis
fishDataAnalysis <- JohnDayBargeData %>% left_join(ODFWDataYearly, by = c("Year"))
fishDataAnalysis <- fishDataAnalysis %>% mutate(SWW = case_when(Year >= 2010 ~ 1, Year < 2010 ~ 0))
ols1 <- lm(ActualWSS ~ SWW*Year + Num_H, data = fishDataAnalysis)
ols2 <- lm(ActualHSS ~ SWW*Year + Num_H, data = fishDataAnalysis)
ols3 <- lm(ActualHSS ~ SWW*Year, data = fishDataAnalysis)
stargazer(ols1,ols2,ols3, type = "text")
stargazer(ols1, ols2, type = "html", out = "Models2.htm", 
          covariate.labels = c("SWW","Year","Number of Hatchery Fish Barged","SWW * Year"))
summary(logistic.reg <- glm(pHOSObserved ~ Year*SWW, data = fishDataAnalysis, family = binomial))

ols2$resid <- ols2$residuals
ols2$fit <- ols2$fit

ggplot(data = ols2, aes(x = fit, y = resid)) + geom_point() + theme_linedraw() + 
  geom_hline(yintercept = 0, lty = 2, color = "red")
qplot(ols2$fit, ols2$resid)

MadrasDataYearly <- MadrasData %>% group_by(Year) %>% summarise(Temperature = mean(Temperature, na.rm = T)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MadrasDataYearly$Group <- as.factor(MadrasDataYearly$Group)
MadrasDataYearly$Group <- factor(MadrasDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW")) # 10.22426 is all time average temperature
MadrasDataYearly <- MadrasDataYearly %>% mutate(Temperature2 = Temperature - mean(Temperature, na.rm = T))
FishMadras <- MadrasDataYearly %>% left_join(ODFWDataYearly, by = "Year")
FishMadras <- FishMadras %>% mutate(SWW = case_when(Year >= 2010 ~ 1, Year < 2010 ~ 0))

ggplot(data = FishMadras, aes(x = Year, y = Temperature2, color = SWW)) + geom_point() + 
  geom_line(aes(x = Year, y = ActualHSS/1000)) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) + 
  stat_poly_eq(aes(label = paste(..rr.label..)), formula = formula, parse = T)

quadmodel <- lm(Temperature2 ~ Year + I(Year^2), data = MadrasDataYearly)
summary(quadmodel)

quadmodel_diag <- augment(quadmodel, data = MadrasDataYearly)
qplot(Year, .resid, data = quadmodel_diag) + geom_line()

with(quadmodel_diag, pacf(.resid, plot = T, cex.lab = 1.5))

with(quadmodel_diag, acf(.resid, lag.max = 1, type = "correlation", plot = F))






MoodyDataYearly <- MoodyData %>% group_by(Year) %>% summarise(Temperature = mean(Temperature, na.rm = T)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MoodyDataYearly$Group <- as.factor(MoodyDataYearly$Group)
MoodyDataYearly$Group <- factor(MoodyDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW")) # 10.22426 is all time average temperature
MoodyDataYearly <- MoodyDataYearly %>% mutate(Temperature2 = Temperature - mean(Temperature, na.rm = T))
FishMoody <- MoodyDataYearly %>% left_join(ODFWDataYearly, by = "Year")
FishMoody <- FishMoody %>% mutate(SWW = case_when(Year >= 2010 ~ 1, Year < 2010 ~ 0))

quadmodel2 <- lm(Temperature2 ~ Year + I(Year^2), data = MoodyDataYearly)
summary(quadmodel2)

quadmodel_diag2 <- augment(quadmodel2, data = MoodyDataYearly)
qplot(Year, .resid, data = quadmodel_diag2) + geom_line()

stargazer(quadmodel, quadmodel2, type = "latex", out = "Models3.htm", covariate.labels = c("Year","{Year}^2"))


## Seasonal and Yearly analysis 

MadrasOLS <- MadrasData %>% group_by(Year, Season) %>% summarize(`Temperature` = median(Temperature, na.rm = T))
MoodyOLS <- MoodyData %>% group_by(Year, Season) %>% summarize(`Median Seasonal Temperature` = median(Temperature))
ols2data <- ODFWData %>% group_by(Year, Season) %>% summarize(`Fall Chinook` = sum(`Fall Chinook`),
                                                              `Hatchery Summer Steelhead` = sum(`Hatchery Summer Steelhead`),
                                                              `Wild Summer Steelhead` = sum(`Wild Summer Steelhead`))
lmdata <- MadrasOLS %>% left_join(ols2data, by = c("Year","Season")) %>% filter(Year > 1976 & Season != "Winter" & Year != 2017 &
                                                                                  Year != 2020)
lmdata2 <- MoodyOLS %>% left_join(ols2data, by = c("Year","Season")) %>% filter(Year > 1976)
lmdata$Total <- rowSums(lmdata[,4:6], na.rm = T)
lmdata2$Total <- rowSums(lmdata2[,4:6], na.rm = T)

fixed <- plm(Total ~ Temperature,
             data = lmdata, index = c("Season", "Year"), model = "within")
fixed.time <- plm(Total ~ Temperature + I(Temperature^2) + factor(Year) - 1,
                  data = lmdata, index = c("Season", "Year"), model = "within")
summary(fixed.time)
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type = "bp")



# John Day Data analysis
lm1 <- lm(pHOSObserved ~ log(Num_H) + NOSA, data = JohnDayBargeData)
lm2 <- lm(pHOSObserved ~ `PercentHBarged` + NOSA, data = JohnDayBargeData) #Current model
stargazer(lm1, lm2, type = "text") # Check slide 20 for source on regression

# Statistical evidence 








# Testing for difference in season by Predam, PreSWW, PostSWW groupings at Madras
MadrasDataYearly <- MadrasData %>% group_by(Year, Season) %>% summarise(Temperature = mean(Temperature)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MadrasDataYearly$Group <- as.factor(MadrasDataYearly$Group)
MadrasDataYearly$Group <- factor(MadrasDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW"))

ggplot(data = MadrasDataYearly, aes(x = Year, y = Temperature)) + geom_smooth(method = "lm", formula = formula, se = F) +
  geom_line(aes(color = Season)) + facet_grid(Season ~ Group, scales = "free") + 
  stat_poly_eq(aes(label = paste(..rr.label..)), formula = formula, parse = T)

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
stargazer(Falllm,Winterlm,Springlm,Summerlm, type = "html") # Order here is 1:Fall,2:Winter,3:Spring,4:Summer


# Testing for difference in season by Predam, PreSWW, PostSWW groupings at Moody
MoodyDataYearly <- MoodyData %>% group_by(Year, Season) %>% summarise(Temperature = mean(Temperature, na.rm = T)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MoodyDataYearly$Group <- as.factor(MoodyDataYearly$Group)
MoodyDataYearly$Group <- factor(MoodyDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW"))

ggplot(data = MoodyDataYearly, aes(x = Year, y = Temperature)) + geom_smooth(method = "lm", formula = formula, se = F) +
  geom_line(aes(color = Season)) + facet_grid(Season ~ Group, scales = "free") + 
  stat_poly_eq(aes(label = paste(..rr.label..)), formula = formula, parse = T) 

MoodyDataYearlyFall <- MoodyDataYearly %>% filter(Season == "Fall")
MoodyDataYearlyWinter <- MoodyDataYearly %>% filter(Season == "Winter")
MoodyDataYearlySpring <- MoodyDataYearly %>% filter(Season == "Spring")
MoodyDataYearlySummer <- MoodyDataYearly %>% filter(Season == "Summer")

# Summarized individually
summary(lm(Temperature ~ Group, data = MoodyDataYearlyFall))
summary(lm(Temperature ~ (Group), data = MoodyDataYearlyWinter))
summary(lm(Temperature ~ (Group), data = MoodyDataYearlySpring))
summary(lm(Temperature ~ (Group), data = MoodyDataYearlySummer))

Falllm1 <- lm(Temperature ~ Group, data = MoodyDataYearlyFall)
Winterlm1 <- lm(Temperature ~ (Group), data = MoodyDataYearlyWinter)
Springlm1 <- lm(Temperature ~ (Group), data = MoodyDataYearlySpring)
Summerlm1 <- lm(Temperature ~ (Group), data = MoodyDataYearlySummer)


# View all at once
stargazer(Falllm1,Winterlm1,Springlm1,Summerlm1, type = "text") # Order here is 1:Fall,2:Winter,3:Spring,4:Summer
















### PLOTS

# Plots rainbow trout, hatchery steelhead, hatchery spring chinook, fall chinook for PGE Data by Season and Year
PGEFishDataGathered <- PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month)
PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>%
  filter(Variable == c("Hatchery Summer Steelhead","Hatchery Spring Chinook", "Fall Chinook", "Rainbow Trout")) %>%
  ggplot(aes(Season, as.numeric(Value), color = Variable, fill = Variable)) + geom_col() + facet_grid(Variable ~ Year) +
  theme_bw() + ggtitle("PGE Fish Count Data") + labs(y = "Number of Fish Captured") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# ODFW Yearly Fish Count Data
ODFWFishPlot <- ODFWDataYearly %>% select(ActualHSS, Year, ActualWSS, ActualFC) %>% pivot_longer(-Year, names_to = "Variable", 
                                                                                 values_to = "Count")
ggplot(data = ODFWFishPlot, aes(x = Year, y = Count, color = Variable)) + geom_line() + 
  labs(y = "Fish Count", x = "Date", title = "ODFW Fish Counts at Sherars Falls (RM 43)", color = "Fish") +
  geom_vline(xintercept = 2010) + annotate(geom = "text", x = 2015.5, y = 3000, label = "SWW Installation") + 
  scale_color_manual(labels = c("Fall Chinook","Hatchery Summer Steelhead","Wild Summer Steelhead"),
                     values = c("blue","red","green")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank())



# Overplot of all variables by Season and Year from PGE data, very ugly and most fish are not significant at all
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




# Plot of missing USGS data 
MissingDataPlot <- pivot_wider(USGSData, names_from = Location, values_from = Temperature, values_fn = max)
MissingDataPlot2 <- MissingDataPlot %>% group_by(Date_time) %>% summarise(Culver = mean(Culver, na.rm = T), 
                                                                          Moody = mean(Moody, na.rm = T),
                                                                         Madras = mean(Madras, na.rm = T))
MissingDataPlot2 <- MissingDataPlot2 %>% mutate(Year = year(Date_time), Season = getSeason(Date_time), Julian = yday(Date_time))
MissingDataPlot2 %>% select(Moody, Madras, Culver, Year) %>% gg_miss_fct(Year) + 
  labs(title = "Percent of Yearly Temperature Data Available", x = "Date", y = "Location", fill = "% Missing Yearly") + 
  scale_fill_gradient(high = "#132B43",
                      low = "#56B1F7") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot of missing PGE Fish Count Data
PGEFishDataGathered <- PGEFishData %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month)
`%notin%` <- Negate(`%in%`)
notfishList <- c("Season", "Year", "Total", "Month", "Date_time")
# Number of missing observations
PGEFishData %>% select(-notfishList) %>% gg_miss_var()

# Times where data is missing 
PGEFishData %>% select(-Total, -Season, -Month, -Date_time) %>% gg_miss_fct(Year) + 
  scale_fill_gradient2(low = "white", high = "black") + labs(title = "PGE Fish Count Data Availability", fill = "% Missing") +
  theme(axis.title.y = element_blank())

# Plot of missing ODFW Data Monthly
ODFWDataMonthly %>% select(-Season, -Month, -Date_time) %>% gg_miss_fct(Year) + 
  labs(title = "ODFW Fish Count Data Availability") + scale_fill_gradient2(low = "white", high = "black") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank(),
        legend.position = "none")

ODFWDataMonthly %>% gather(Variable, Value, -Date_time, -Year, -Season, -Month) %>% 
  ggplot(aes(x = Year, y = Value)) + geom_miss_point() + scale_color_manual(values = c("white", "black")) + theme_dark() + 
  labs(x = "Date", y = "Fish Count", color = "Missing Observations", title = "ODFW Fish Count Data Availability") + 
  theme(plot.title = element_text(hjust = 0.5))

# ODEQ missing data

ODEQMissingPlot <- ODEQData %>% gather(Variable, Value, -Location, -Date_time, -Year, -Season, -Julian)

ODEQData %>% select(-Date_time, -Season, -Julian, -Location) %>% gg_miss_fct(Year) + scale_fill_viridis_c() +
  labs(title = "ODEQ Water Quality Parameter Data Coverage", x = "Date", y = "Variable", fill = "% Missing Yearly") + 
  theme(plot.title = element_text(hjust = 0.5))


# PGE Missing Data

HourlyPGEData %>% select(-Date_time, -Season, -Julian) %>% gg_miss_fct(Year) + scale_fill_viridis_b() +
  labs(title = "PGE Water Quality Parameter Data Coverage", x = "Date", y = "Variable", fill = "% Missing Yearly") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Correlation matrix for season data
MadrasDataYearly <- MadrasData %>% group_by(Year, Season) %>% summarise(Temperature = mean(Temperature)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MadrasDataYearly$Group <- as.factor(MadrasDataYearly$Group)
MadrasDataYearly$Group <- factor(MadrasDataYearly$Group, levels = c("PreDam", "PreSWW", "PostSWW"))

CorrelogramData <- MadrasData %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 & Year > 1956 ~ "PreSWW", Year >= 2010 ~ "PostSWW")) %>% 
  mutate(Temperature2 = rollmax(Temperature, k = 7, fill = NA))
colnames(CorrelogramData) <- c("Date","Temp","Location","Year","Season","Julian","Period","Temperature")
CorrelogramData$Period <- factor(CorrelogramData$Period, levels = c("PreDam", "PreSWW", "PostSWW"))

CorrelogramData[,] %>% ggcorr(method = "pairwise") # Spread season to make work
?ggcorr
# Comparing Pre-Dam, Pre-SWW, Post-SWW at Madras
CorrelogramData <- CorrelogramData %>% group_by(Period) %>% mutate(Line = case_when(Period == "PreDam" ~ "1956-04-01", 
                                                                 Period == "PostSWW" ~ "2010-01-01"))
ggplot(data = CorrelogramData, aes(x = Date, y = Temperature)) +
  geom_line(color = "darkblue") + facet_grid( ~ Period, scales = "free_x", labeller = 
                                                as_labeller(c(`PreDam` = "Pre-Dam",
                                                              `PreSWW` = "Pre-SWW",
                                                              `PostSWW` = "Post-SWW"))) + 
  geom_vline(aes(xintercept = as.Date("1956-04-01"), color = "red4"), linetype = "longdash") + 
  geom_vline(aes(xintercept = as.Date("2010-01-01"), color = "royalblue4"), linetype = "dashed") +
  labs(y = "River Temperature (Celsius °)", title = "7 Day Rolling Max Temperature at Madras Gage") + theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


# Table of means and medians of Pre-Dam, Pre-SWW, Post-SWW at Madras
MadrasDataYearly <- MadrasData %>% group_by(Year, Season) %>%
  summarise(`Mean Temperature` = mean(Temperature, na.rm = T, trim = 2),
            `Median Temperature` = median(Temperature, na.rm = T, trim = 2)) %>%
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MadrasDataYearly <- MadrasDataYearly %>% drop_na() %>%
  pivot_wider(names_from = Season, values_from = c("Mean Temperature", "Median Temperature"))
colnames(MadrasDataYearly) <- c("Year", "Period", "Winter Mean Temperature", "Spring Mean Temperature",
                                "Summer Mean Temperature", "Fall Mean Temperature")
MadrasDataYearly <- MadrasDataYearly[,1:6]

# rtable <- reactable(MadrasDataYearly, defaultPageSize = 40)
# html <- "rtable.html"
# saveWidget(rtable,html)
# webshot(html, "Table1.png")

# Stats to back up previous table/chart
stargazer(Falllm,Winterlm,Springlm,Summerlm, type = "html", out = "Models.htm", covariate.labels = c("Pre-SWW","Post-SWW"))

# Table of means and medians of Pre-Dam, Pre-SWW, Post-SWW at Madras
MoodyDataYearly <- MoodyData %>% group_by(Year, Season) %>% 
  summarise(`Mean Temperature` = mean(Temperature, na.rm = T, trim = 2),
            `Median Temperature` = median(Temperature, na.rm = T, trim = 2)) %>% 
  mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
MoodyDataYearly <- MoodyDataYearly %>% drop_na() %>% 
  pivot_wider(names_from = Season, values_from = c("Mean Temperature", "Median Temperature"))
colnames(MoodyDataYearly) <- c("Year", "Period", "Winter Mean Temperature", "Spring Mean Temperature", 
                                "Summer Mean Temperature", "Fall Mean Temperature")
MoodyDataYearly <- MoodyDataYearly[,1:6]

customGreen = "#71CA97"
customRed = "#ff7f7f"

improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", color = 
                                                               ifelse(x - lag(x, default = first(x)) > 0, 
                                                               customGreen, 
                                                               ifelse(x - lag(x, default = first(x)) < 0, 
                                                               customRed, "black"))),
                                   x ~ icontext(icon = 
                                                ifelse(x - lag(x, default = first(x)) > 0, "arrow-up",
                                                ifelse(x - lag(x, default = first(x)) < 0, "arrow-down","circle")), 
                                                       x))

Table1 <- formattable(MadrasDataYearly, list(
            `Totals` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
            `Winter Mean Temperature` = improvement_formatter,
            `Spring Mean Temperature` = improvement_formatter,
            `Summer Mean Temperature` = improvement_formatter,
            `Fall Mean Temperature` = improvement_formatter
            ))

Table2 <- formattable(MoodyDataYearly, list(
  `Totals` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `Winter Mean Temperature` = improvement_formatter,
  `Spring Mean Temperature` = improvement_formatter,
  `Summer Mean Temperature` = improvement_formatter,
  `Fall Mean Temperature` = improvement_formatter
))


export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(Table1,"Plots/Table1.png")
export_formattable(Table2,"Plots/Table2.png")

# rtable1 <- reactable(MoodyDataYearly, defaultPageSize = 40, 
#                      defaultColDef = colDef(align = "center", minWidth = 150), 
#                      bordered = TRUE, highlight = TRUE, theme = reactableTheme(
#                        borderColor = "#dfe2e5",
#                        stripedColor = "#f6f8fa",
#                        highlightColor = "#f0f5f9",
#                        cellPadding = "8px 12px",
#                        headerStyle = list(color = "f7f7f8")))
# rtable1 <- htmlwidgets::prependContent(rtable1, htmltools::h2(class = "title", "Seasonal Mean Temperature at Madras by Year"))
# html1 <- "rtable1.html"
# saveWidget(rtable1,html1)
# webshot(html, "Plots/Table2.png")








stargazer(Falllm,Winterlm,Springlm,Summerlm, type = "html", out = "Models1.htm", covariate.labels = c("Pre-SWW","Post-SWW"))
stargazer(Falllm,Winterlm,Springlm,Summerlm, type = "text")

# Plots for Sophia
MadrasDataMedians <- MadrasData %>% group_by(Year, Season) %>% 
  summarize(median = median(`Temperature`, na.rm = T), mean = mean(`Temperature`, na.rm = T)) %>% 
  filter(Year == 1953 | Year == 1955 | Year == 2008 | Year == 2009 | Year == 2016 | Year == 2019)
# Seasonal Mean Temperature pre and post dam comparison at Madras
colorset3 <- c(`1953` = "firebrick3", `1955` = "firebrick3", `2008` = "royalblue4", `2009` = "royalblue4", 
               `2016` = "firebrick3", `2019` = "firebrick3")
MadrasDataMedians %>% ggplot(aes(Season, mean)) + geom_bar(aes(fill = as.factor(Year)), position = "dodge", stat = "identity") + 
  labs(y = "Mean River Temperature (Celsius °)", fill = "Year", title = "Madras") + scale_fill_brewer(palette = "Dark2") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, color = "firebrick4")) +
  scale_fill_manual(values = colorset3)

# Seasonal Mean Temperature pre and post dam comparison at Moody
MoodyDataMedians <- MoodyData %>% group_by(Year, Season) %>% 
  summarize(median = median(`Temperature`, na.rm = T), mean = mean(`Temperature`, na.rm = T)) %>% 
  filter(Year == 1953 | Year == 1955 | Year == 1974 | Year == 1976 | Year == 2016 | Year == 2019)
colorset4 <- c(`1953` = "firebrick3", `1955` = "firebrick3", `1974` = "royalblue4", `1976` = "royalblue4", 
               `2016` = "firebrick3", `2019` = "firebrick3")
MoodyDataMedians %>% ggplot(aes(Season, mean)) + geom_bar(aes(fill = as.factor(Year)), position = "dodge", stat = "identity") + 
  labs(y = "Mean River Temperature (Celsius °)", fill = "Year", title = "Moody") + scale_fill_brewer(palette = "Dark2") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, color = "dodgerblue4")) +
  scale_fill_manual(values = colorset4)


# Pre and post dam temperature comparison
temperatureColor <- "#C92A2A"
fishColor <- rgb(0.2, 0.6, 0.9, 1)
colorset = c(`1953` = "firebrick3", `1955` = "firebrick3", `2008` = "royalblue4", `2009` = "royalblue4", 
             `2016` = "firebrick3", `2019` = "firebrick3")
customcolors <- RColorBrewer::brewer.pal(6, "Spectral")
longtermtempplot <- MadrasData %>% filter(Year == 1953 | Year == 1955 | Year == 2008 | Year == 2009 | Year == 2016 | Year == 2019) %>%
  ggplot(aes(x = as.Date(Julian, origin = "1952-01-01"), y = Temperature, color = as.factor(Year))) + geom_line(show.legend = F) + 
  facet_wrap( ~ as.factor(Year), ncol = 2, labeller = as_labeller(c(`1953` = "1953: Pre-Dam",
                                                                        `1955` = "1955: Pre-Dam",
                                                                        `2008` = "2008: Pre-SWW",
                                                                        `2009` = "2009: Pre-SWW",
                                                                        `2016` = "2016: Post-SWW",
                                                                        `2019` = "2019: Post-SWW"))) +
  theme_bw() + 
  scale_x_date(date_labels = "%b") + ggtitle("Deschutes River Temperature at Madras by Period") + 
  labs(x = "Date", y = "River Temperature (Celsius °)") + 
  theme(axis.title.y = element_text(color = temperatureColor, size = 13), 
        axis.title.x = element_text(color = "black", size = 13), 
        plot.title = element_text(hjust = 0.5))
longtermtempplot + scale_color_manual(values = colorset)

colorset2 <- c(`1953` = "firebrick3", `1955` = "firebrick3", `1974` = "royalblue4", `1976` = "royalblue4", 
               `2016` = "firebrick3", `2019` = "firebrick3")
longtermtempplot2 <- MoodyData %>% filter(Year == 1953 | Year == 1955 | Year == 1974 | Year == 1976 | Year == 2016 | Year == 2019) %>%
  ggplot(aes(x = as.Date(Julian, origin = "1952-01-01"), y = Temperature, color = as.factor(Year))) + geom_line(show.legend = F) + 
  facet_wrap( ~ as.factor(Year), ncol = 2, labeller = as_labeller(c(`1953` = "1953: Pre-Dam",
                                                                    `1955` = "1955: Pre-Dam",
                                                                    `1974` = "1974: Pre-SWW",
                                                                    `1976` = "1976: Pre-SWW",
                                                                    `2016` = "2016: Post-SWW",
                                                                    `2019` = "2019: Post-SWW"))) +
  theme_bw() + 
  scale_x_date(date_labels = "%b") + ggtitle("Deschutes River Temperature at Moody by Period") + 
  labs(x = "Date", y = "River Temperature (Celsius °)") + 
  theme(axis.title.y = element_text(color = temperatureColor, size = 13), 
        axis.title.x = element_text(color = "black", size = 13), 
        plot.title = element_text(hjust = 0.5))
longtermtempplot2 + scale_color_manual(values = colorset2)

MoodyData %>% filter(Year == 1974) %>% ggplot(aes(x = Julian, y = Temperature)) + geom_line()


## John Day Barge Data pHOS vs Hatchery
ggplot(JohnDayBargeData) + geom_line(aes(x = Year, y = PercentHBarged), color = "black") + 
  geom_line(aes(x = Year, y = pHOSObserved*100), color = "red") + 
  labs(y = "Percentage of Hatchery Fish Barged", 
       title = "Barging Rate vs. pHOS (proportion of hatchery-origin spawners) in the John Day") + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "pHOS")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y.right = element_text(color = "red")) #+ 
  # annotate(x = 2017, y = 34, label = "Barging Rate", geom = "text") + 
  # annotate(x = 2017, y = 7, label = "pHOS", color = "red", geom = "text")

# Two axis plot temperature vs fish count
MadrasData
ggplot(data = fishandtempdfMadras, aes(x = Date_time)) + geom_line(aes(y = `Mean Temperature`), color = temperatureColor) +
  geom_line(aes(y = Total / coeff), color = fishColor) + 
  scale_y_continuous(name = "River Temperature (Celsius °)", sec.axis = sec_axis(~.*coeff, name = "Total Fish Count")) + 
  theme_bw() +
  ggtitle("Temperature vs. Fish Count") + theme(axis.title.y = element_text(color = temperatureColor, size = 13),
                                                axis.title.y.right = element_text(color = fishColor, size = 13),
                                                plot.title = element_text(hjust = 0.5)) + xlab("Date")


# Moody Plot by Season and Group
MoodyData <- MoodyData %>% mutate(Group = case_when(Year <= 1956 ~ "PreDam", Year <= 2009 ~ "PreSWW", Year >= 2010 ~ "PostSWW"))
ggplot(MoodyData, aes(x = Date_time, y = Temperature, color = Group)) + geom_line() + geom_smooth(method = "lm") +
  facet_wrap( ~ Season)

### Bonneville Dam Data from Ian Tattam
BonnevilleDatavsODFW <- BonnevilleData %>% left_join(ODFWDataYearly, by = c("Year"))
ggplot(data = BonnevilleDatavsODFW) + geom_line(aes(as.Date(paste0(Year, "-01-01")),ActualHSS), color = "red") + 
  labs(x = "Date", y = "Hatchery Summer Steelhead and Bonneville Hatchery Counts", title = "Bonneville and Deschutes Steelhead Presence") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(as.Date(paste0(Year, "-01-01")),ActualHSS), color = "red") + 
  geom_point(aes(as.Date(paste0(Year, "-01-01")),Hatchery), color = "black") + 
  geom_line(aes(as.Date(paste0(Year, "-01-01")),Hatchery), color = "black") + 
  annotate(x = as.Date("2015-01-01"), y = 1500, label = "Bonneville Hatchery Summer Steelhead", geom = "text", color = "red") + 
  annotate(x = as.Date("2016-01-01"), y = 210, geom = "text", label = "Sherars Falls Hatchery Summer Steelhead", color = "black") 

BonnevilleDatavsODFW2 <- BonnevilleData2 %>% left_join(ODFWDataYearly, by = c("Year"))
bonnevillecoeff <- max(BonnevilleDatavsODFW2$Steelhead)/max(BonnevilleDatavsODFW2$ActualHSS, na.rm = T)
ggplot(data = BonnevilleDatavsODFW2) + geom_line(aes(as.Date(paste0(Year, "-01-01")), ActualHSS), color = "red") + 
  labs(x = "Date", title = "Bonneville and Deschutes Steelhead Presence") + 
  scale_y_continuous(name = "Sherars Falls Hatchery Summer Steelhead", 
                     sec.axis = sec_axis(~ .*bonnevillecoeff, name = "Bonneville Hatchery Summer Steelhead",
                                         labels = scales::comma), position = "right") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                     axis.title.y = element_text(color = "black", size = 13),
                     axis.title.y.right = element_text(color = "red", size = 13)) +
  geom_point(aes(as.Date(paste0(Year, "-01-01")), ActualHSS), color = "red") + 
  geom_point(aes(as.Date(paste0(Year, "-01-01")), Steelhead / bonnevillecoeff), color = "black") + 
  geom_line(aes(as.Date(paste0(Year, "-01-01")), Steelhead / bonnevillecoeff), color = "black") +
  geom_line(aes(as.Date(paste0(Year, "-01-01")), ActualHSS), color = "red")


