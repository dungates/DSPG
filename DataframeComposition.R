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