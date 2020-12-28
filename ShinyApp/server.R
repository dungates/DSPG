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

jscode <- "var referer = document.referrer;
           var n = referer.includes('economic');
           var x = document.getElementsByClassName('logo');
           if (n != true) {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a>';
           } else {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a>';
           }
           "

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
    plot %>% plotly::layout(title = sitename, xaxis = x, yaxis = y)
    
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
    plot %>% plotly::layout(title = sitename,xaxis = x, yaxis = y)
   
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
    plot %>% plotly::layout(title = sitename, xaxis = x, yaxis = y)
  }
}

#plot_USGS("00010", "14092500", "2000-01-01", "2020-01-01", "annual")
#siteNumber <- "USR0000OMTW"#"USC00356532" ##pelton butte dam // USC00358407 the dalles // USC00352440 dufur // USR0000OMTW mt wilson
#startdate <- "2010-01-01"
#enddate <- "2020-01-01"

#function for NOAA plotting 
plot_NOAA <- function(siteNumber, startdate, enddate) {
  siteNumber_ref <- locations_ref$NOAAref[locations_ref$num==siteNumber]
  suppressMessages(temperatures <- rnoaa::ghcnd_search(siteNumber_ref, var = c("TMAX","TMIN"), 
                                                       refresh = F, date_min = startdate, date_max = enddate))
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
  plot %>% plotly::layout(title = paste("NOAA - ",locations_ref$location[locations_ref$num==siteNumber]), xaxis = x, yaxis = y)
}
#plot_NOAA(siteNumber, startdate, enddate)

#function for fishdata ##still to add filtering by date and remove low months 

plot_fish_RB <- function(vars, startdate, enddate, reporttype) {
  if (reporttype=="daily"){
    print("Daily fish data not available, please select monthly or yearly view instead")
  }
  else if (reporttype=="monthly"){
    fishdata_times <- fishdata[,(colnames(fishdata) == "Date_time" | colnames(fishdata) == "Season" | 
                                   colnames(fishdata) == "Year"| colnames(fishdata) == "month_numeric")]
    fishdata_fish <- fishdata %>% select(all_of(vars))
    fishdata_all <- data.frame(fishdata_times, fishdata_fish)
    fishdata_all <- fishdata_all[fishdata_all$Date_time >= startdate & fishdata_all$Date_time <= enddate,]
    fishdata_gathered <- fishdata_all %>% 
      tidyr::gather(key = fishtype, value = count, vars[1]:tail(vars,1))
    fishdata_gathered$Yearmonth <- with(fishdata_gathered, sprintf("%d-%02d", Year, month_numeric))
    fishdata_gathered <- fishdata_gathered %>% group_by(Yearmonth, fishtype) %>% summarize(mean_count = mean(count, na.rm = TRUE)) %>% ungroup()
    plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", x = ~Yearmonth, y = ~mean_count, color = ~fishtype)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Date", titlefont = f)
    y <- list(title = "Fish #", titlefont = f)
    plt %>% plotly::layout(title = "ODFW @ Round Butte - Fish abundance, monthly average", xaxis = x, yaxis = y, 
                   legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 x = 0.5))
     
  }
  else if (reporttype=="annual"){
    fishdata_times <- fishdata[,(colnames(fishdata) == "Date_time" | colnames(fishdata) == "Season" | 
                                   colnames(fishdata) == "Year"| colnames(fishdata) == "Month")]
    fishdata_fish <- fishdata %>% select(all_of(x = vars))
    fishdata_all <- data.frame(fishdata_times, fishdata_fish)
    fishdata_all <- fishdata_all[fishdata_all$Date_time >= startdate & fishdata_all$Date_time <= enddate,]
    fishdata_gathered <- fishdata_all %>% 
      tidyr::gather(key = fishtype, value = count, vars[1]:tail(vars,1))
    fishdata_gathered <- fishdata_gathered %>% dplyr::group_by(Year, fishtype) %>% 
      dplyr::summarize(mean_count = mean(count, na.rm = TRUE)) %>% ungroup()
    fishdata_gathered$Yearnum <- as.numeric(fishdata_gathered$Year)
    plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", 
                           x = ~Yearnum, y = ~mean_count, color = ~fishtype)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f")
    x <- list(title = "Date", titlefont = f)
    y <- list(title = "Fish #", titlefont = f)
    plt %>% plotly::layout(title = "ODFW @ Round Butte - Fish abundance, yearly average", xaxis = x, yaxis = y, 
                   legend = list(orientation = "h",   
                                 xanchor = "center",  
                                 x = 0.5))
    
  }
}

# vars <- c("Hatchery.Summer.Steelhead","Wild.Summer.Steelhead", "Summer.Steelhead.RM",
#           "Summer.Steelhead.LM","Rainbow.Trout","Bull.Trout")

# plot_fish_RB(vars, "2004-01-01", "2020-01-01", reporttype = "annual")

plot_fish_SF <- function(vars, startdate, enddate, dataset) {
  fishdata_SF <- dataset
  fishdata_times <- fishdata_SF[,2]
  fishdata_fish <- fishdata_SF %>% select(all_of(vars))
  fishdata_all <- data.frame(fishdata_times, fishdata_fish)
  fishdata_all <- fishdata_all[fishdata_all$fishdata_times >= startdate & fishdata_all$fishdata_times <= enddate,]
  fishdata_gathered <- fishdata_all %>% tidyr::gather(key = fishtype, value = count, vars[1]:tail(vars,1))
  plt <- plotly::plot_ly(fishdata_gathered, type = "scatter", mode = "markers+lines", x = ~fishdata_times, y = ~count, color = ~fishtype)
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f")
  x <- list(title = "Year", titlefont = f)
  y <- list(title = "Fish #, Yearly Average", titlefont = f)
  plt %>% plotly::layout(title = "ODFW - Fish Count @ Sherars Falls, RM 43", xaxis = x, yaxis = y,
                 legend = list(orientation = "h",   
                               xanchor = "center",  
                               x = 0.5))
 
}

# vars2 <- colnames(fishdata_SF)
# vars2<- vars2[3:6]
# vars2 <- join(vars, vars2)
# vars3 <- append(vars, vars2)
# plot_fish_SF(vars2,"2004-01-01", "2020-01-01", fishdata_SF)


shinyServer <- function(input, output) {
  runjs(jscode)
  
  USGSreactive <- eventReactive(input$submit, 
                                {plot_USGS(input$variable, input$location, input$daterange[1], input$daterange[2], input$view)})
  NOAAreactive <- eventReactive(input$submit, 
                                {if (input$variableNOAA == "Atemp") {plot_NOAA(input$location2, input$daterange[1], input$daterange[2])}})
  Fishreactive <- eventReactive(input$submit, 
                                {if (input$locationODFW == "SF") {plot_fish_SF(input$fishSF, as.character(input$daterange[1]), 
                                                                               as.character(input$daterange[2]), fishdata_SF)} 
                                  else if (input$locationODFW == "RB") {plot_fish_RB(input$fishRB, as.character(input$daterange[1]), 
                                                                                     as.character(input$daterange[2]), input$view)}})
  #Fishreactive <- eventReactive(input$submit, {plot_fish_SF(input$fishSF, as.character(input$daterange[1]), as.character(input$daterange[2]),fishdata_SF)}) 
  output$USGSplot <- plotly::renderPlotly({USGSreactive()})
  output$NOAAplot <- plotly::renderPlotly({NOAAreactive()})
  output$Fishplot <- plotly::renderPlotly({Fishreactive()})
  output$table <- shiny::renderTable(fishdata_SF)
    # output$madrastemps <- plotMadras(input$daterange[1],input$daterange[2])
}

