library(shinydashboard)
library(dataRetrieval)
library(shiny)
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
library(shinyWidgets)
#dataframe for fishdata

setwd("C:/Users/Sebastiano/Documents/DSPG Dash/Deschutes_Shiny")
fishdata <- read.csv("data/AllFishData.csv", header = T)
fishdata$month_numeric <- month(fishdata$Date_time)
fishdata_SF <- read.csv("data/AllSteelheadData.csv", header = T)

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



#function for USGS plotting
plot_USGS <- function(parameterCd,siteNumber,startdate,enddate,reporttype) {
  siteNumber_ref <- locations_ref$USGSref[locations_ref$num==siteNumber]
  sitename <- locations_ref$location[locations_ref$num==siteNumber]
  sitename <- paste("USGS - ", sitename)
  parametername <- indicators_ref$indicator[indicators_ref$ref==parameterCd]
  ChoptankInfo <- readNWISsite(siteNumber_ref)
  if (reporttype == "daily"){
    rawDailyData <- readNWISdv(siteNumber_ref,parameterCd, startdate, enddate)
    rawDailyData <- na.omit(rawDailyData)
    colnames(rawDailyData)[4] <- "Value"
    plot <- plotly::plot_ly(rawDailyData, type = "scatter", mode = "lines", x = ~Date, y = ~Value)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Date", titlefont = f)
    y <- list(title = parametername, titlefont = f)
    plot %>% layout(title = sitename, xaxis = x, yaxis = y)
    
  }
  else if (reporttype == "monthly"){
    startdate <- as.Date(startdate)
    startdate_reformat <- format(startdate, "%Y-%m")
    enddate <- as.Date(enddate)
    enddate_reformat <- format(enddate, "%Y-%m")
    rawDailyData <- readNWISstat(siteNumber_ref,parameterCd = parameterCd,statType = "mean",
                                 statReportType = "monthly", startDate = as.character(startdate_reformat), 
                                 endDate = as.character(enddate_reformat))
    colnames(rawDailyData)[8] <- "Value"
    colnames(rawDailyData)[6] <- "Year"
    #rawDailyData$YM <- paste(rawDailyData$Year, rawDailyData$month_nu, sep = "/")
    rawDailyData$YM <- with(rawDailyData, sprintf("%d-%02d", Year, month_nu))
    plot <- plotly::plot_ly(rawDailyData, type = "scatter", mode = "markers+lines", x = ~YM, y = ~Value)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Month", titlefont = f)
    y <- list(title = parametername, titlefont = f)
    plot %>% layout(title = sitename,xaxis = x, yaxis = y)
   
  }
  else if (reporttype == "annual"){
    startdate <- as.Date(startdate)
    startdate_reformat <- format(startdate, "%Y")
    enddate <- as.Date(enddate)
    enddate_reformat <- format(enddate, "%Y")
    rawDailyData <- readNWISstat(siteNumber_ref,parameterCd = parameterCd,statType = "mean",
                                 statReportType = "annual", startDate = as.character(startdate_reformat), 
                                 endDate = as.character(enddate_reformat))
    #rawDailyData <- na.omit(rawDailyData)
    colnames(rawDailyData)[7] <- "Value"
    colnames(rawDailyData)[6] <- "Year"
    plot <- plotly::plot_ly(rawDailyData, type = "scatter", mode = "markers+lines", x = ~Year, y = ~Value)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Year", titlefont = f)
    y <- list(title = parametername, titlefont = f)
    plot %>% layout(title = sitename, xaxis = x, yaxis = y)
  }
}

#plot_USGS("00010", "14092500", "2000-01-01", "2020-01-01", "annual")
#siteNumber <- "USR0000OMTW"#"USC00356532" ##pelton butte dam // USC00358407 the dalles // USC00352440 dufur // USR0000OMTW mt wilson
#startdate <- "2010-01-01"
#enddate <- "2020-01-01"

#function for NOAA plotting 
plot_NOAA <- function(siteNumber, startdate, enddate) {
  siteNumber_ref <- locations_ref$NOAAref[locations_ref$num==siteNumber]
  suppressMessages(temperatures <- ghcnd_search(siteNumber_ref, var = c("TMAX","TMIN"), refresh = F, date_min = startdate, date_max = enddate))
  temperatures$tmax$tmax_corr <- temperatures$tmax$tmax/10
  temperatures$tmin$tmin_corr <- temperatures$tmin$tmin/10
  temperatures$tmin$tAVG <- (temperatures$tmax$tmax_corr+temperatures$tmin$tmin_corr)/2
  plot <- plotly::plot_ly(temperatures$tmin, type = "scatter", mode = "markers+lines", x = ~date, y = ~tAVG)
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f")
  x <- list(title = "Date", titlefont = f)
  y <- list(title = "Air Temperature (C)", titlefont = f)
  plot %>% layout(title = paste("NOAA - ",locations_ref$location[locations_ref$num==siteNumber]), xaxis = x, yaxis = y)
}
#plot_NOAA(siteNumber, startdate, enddate)

#function for fishdata ##still to add filtering by date and remove low months 

plot_fish_RB <- function(vars, startdate, enddate, reporttype) {
  if (reporttype=="daily"){
    print("Daily fish data not available, please select monthly or yearly view instead")
  }
  else if (reporttype=="monthly"){
    fishdata_times <- fishdata[,(colnames(fishdata) == "Date_time" | colnames(fishdata) == "Season" | colnames(fishdata) == "Year"| colnames(fishdata) == "month_numeric")]
    fishdata_fish <- fishdata %>% select(all_of(vars))
    fishdata_all <- data.frame(fishdata_times, fishdata_fish)
    fishdata_gathered <- fishdata_all %>% gather(key = fishtype, value = count, vars[1]:tail(vars,1))
    fishdata_gathered$Yearmonth <- with(fishdata_gathered, sprintf("%d-%02d", Year, month_numeric))
    fishdata_gathered <- fishdata_gathered %>% group_by(Yearmonth, fishtype) %>% summarize(mean_count = mean(count, na.rm = TRUE)) %>% ungroup()
    plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", x = ~Yearmonth, y = ~mean_count, color = ~fishtype)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Date", titlefont = f)
    y <- list(title = "Fish #", titlefont = f)
    plt %>% layout(title = "ODFW - Fish Count", xaxis = x, yaxis = y)
     
  }
  else if (reporttype=="yearly"){
    fishdata_times <- fishdata[,(colnames(fishdata) == "Date_time" | colnames(fishdata) == "Season" | colnames(fishdata) == "Year"| colnames(fishdata) == "Month")]
    fishdata_fish <- fishdata %>% select(all_of(vars))
    fishdata_all <- data.frame(fishdata_times, fishdata_fish)
    fishdata_gathered <- fishdata_all %>% gather(key = fishtype, value = count, vars[1]:tail(vars,1))
    fishdata_gathered <- fishdata_gathered %>% group_by(Year, fishtype) %>% summarize(mean_count = mean(count, na.rm = TRUE)) %>% ungroup()
    fishdata_gathered$Yearnum <- as.numeric(fishdata_gathered$Year)
    plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", x = ~Yearnum, y = ~mean_count, color = ~fishtype)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Date", titlefont = f)
    y <- list(title = "Fish #", titlefont = f)
    plt %>% layout(title = "ODFW - Fish abundance, yearly average", xaxis = x, yaxis = y)
    
  }
}

#plot_fish(vars,startdate = 01/01/2000,enddate = 01/01/2020,reporttype = "yearly")
#vars <- c("Hatchery.Summer.Steelhead","Wild.Summer.Steelhead", "Summer.Steelhead.RM","Summer.Steelhead.LM","Rainbow.Trout","Bull.Trout")
plot_fish_SF <- function(vars, startdate, enddate) {
  fishdata_SF <- read.csv("data/AllSteelheadData.csv", header = T)
  fishdata_times <- fishdata_SF[,2]
  fishdata_fish <- fishdata_SF %>% select(all_of(vars))
  fishdata_all <- data.frame(fishdata_times, fishdata_fish)
  fishdata_all <- fishdata_all[fishdata_all$fishdata_times >= startdate & fishdata_all$fishdata_times <= enddate,]
  fishdata_gathered <- fishdata_all %>% gather(key = fishtype, value = count, vars[1]:tail(vars,1))
  plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", x = ~fishdata_times, y = ~count, color = ~fishtype)
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f")
  x <- list(title = "Year", titlefont = f)
  y <- list(title = "Fish #, Yearly Average", titlefont = f)
  plt %>% layout(title = "ODFW - Fish Count @ Sherars Falls, RM 43", xaxis = x, yaxis = y)
 
}

# plot_fish_SF(vars2,"2000-01-01", "2020-01-01")
# vars2 <- colnames(fishdata_SF)
# vars2<- vars2[3:10]
# vars2 <- join(vars, vars2)
# vars3 <- append(vars, vars2)

shinyServer(function(input, output) {
  USGSreactive <- eventReactive(input$submit, {plot_USGS(input$variable, input$location, input$daterange[1], input$daterange[2], input$view)})
  NOAAreactive <- eventReactive(input$submit, {if (input$variableNOAA == "Atemp") {plot_NOAA(input$location2, input$daterange[1], input$daterange[2])}})
  # Fishreactive <- eventReactive(input$submit, {if (input$locationODFW == "SF") {plot_fish_SF(input$fishSF, input$daterange[1], input$daterange[2])} 
  #   else if (input$locationODFW == "RB") {plot_fish_RB(input$fishRB, input$daterange[1], input$daterange[2], input$view)}})
  Fishreactive <- eventReactive(input$submit, {plot_fish_SF(input$fishSF, input$daterange[1], input$daterange[2])}) 
  output$USGSplot <- renderPlotly({USGSreactive()})
  output$NOAAplot <- renderPlotly({NOAAreactive()})
  output$Fishplot <- renderPlotly({Fishreactive()})
  output$table <- renderTable(fishdata_SF)
    # output$madrastemps <- plotMadras(input$daterange[1],input$daterange[2])
}
)
