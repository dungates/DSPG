library(tidyverse)
library(readxl)
library(lubridate)

setwd("~/DSPG")

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


# Reading in the second sheet
df6 <- read_excel("pge-water-chemistry-2015-2017.xlsx", sheet = 2)
df7 <- df6 %>% mutate(new = paste(Parameter, "in", Units))
df7 <- subset(df7, select = -c(Units, Parameter))

# Making a new index for spread to use
df7 <- df7 %>% 
  group_by(new) %>% 
  do(tibble::rowid_to_column(.))

df8 <- spread(df7, new, Value) %>% select(-grouped_id)

stupidf <- df8 %>% filter(Site == "Round Butte Forebay")
ggplot(stupidf, aes(x = Date, y = `Temperature in C`, color = Site)) + geom_line()
# This data is shit

# Another dataframe specifically to be used for PGE data, not useful since data merge

dfTemperature <- subset(df, Parameter == "Temperature")
view(dfTemperature)
dfTemperature %>% group_by(`Station Name`)

names(dfTemperature)[2] <- "StationName"
p <- ggplot(data = dfTemperature, aes(x = Date, y = Value, color = StationName)) + geom_line() + facet_wrap(vars(StationName)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none") + ggtitle("Try Mint Tea!")
p

# Subsetting to look at location specific data

unique(dfTemperature$StationName)
view(dfTemperature)
reregDam <- subset(dfTemperature, StationName == "Reregulating Dam")
dryCreek <- subset(dfTemperature, StationName == "Dry Creek")
dryCreekTrib <- subset(dfTemperature, StationName == "Dry Creek Tributary")
shitCreek <- subset(dfTemperature, StationName == "Shitike Creek")
troutCreek <- subset(dfTemperature, StationName == "Trout Creek")
southJunction <- subset(dfTemperature, StationName == "South Junction")
whiteHorse <- subset(dfTemperature, StationName == "Whitehorse")
warmSprings <- subset(dfTemperature, StationName == "Warm Springs")
ferry <- subset(dfTemperature, StationName == "Ferry")
lowerWap <- subset(dfTemperature, StationName == "Lower Wapinitia")
oakSprings <- subset(dfTemperature, StationName == "Oak Springs")
sandyBeach <- subset(dfTemperature, StationName == "Sandy Beach")
whiteRiver <- subset(dfTemperature, StationName == "White River")
kloanRapids <- subset(dfTemperature, StationName == "Kloan Rapids")
deschutesInf <- subset(dfTemperature, StationName == "Deschutes Inflow")
metoliusInf <- subset(dfTemperature, StationName == "Metolius Inflow")
wreck <- subset(dfTemperature, StationName == "Wreck")
rattleSnake <- subset(dfTemperature, StationName == "Rattlesnake") # Note that this could be rattlesnake canyon or rapids I am not sure
crookedInf <- subset(dfTemperature, StationName == "Crooked Inflow")
bullRun <- subset(dfTemperature, StationName == "Bull Run")
riverMouth <- subset(dfTemperature, StationName == "River Mouth")
willowCreek <- subset(dfTemperature, StationName == "Willow Creek")
whiteRivWRFSP <- subset(dfTemperature, StationName == "White River WRFSP")


ggplot() + geom_line(data = deschutesInf, aes(Date, Value, color = "Deschutes")) + 
  geom_line(data = metoliusInf, aes(Date, Value, color = "Metolius")) +
  geom_line(data = crookedInf, aes(Date, Value, color = "Crooked")) + 
  geom_line(data = Madrasdatasmall, aes(Date_time, Temperature, color = "Madras")) +
  geom_line(data = Biggdatasmall, aes(Date_time, Temperature, color = "Biggs")) +
  geom_line(data = warmSprings, aes(Date, Value, color = "Warm Springs"))


Madrasdatasmall <- MadrasData2[MadrasData2$Year>=2015 & MadrasData2$Year <=2016,]
Biggdatasmall <- BiggsData2[BiggsData2$Year>=2015 & BiggsData2$Year <=2016,]

