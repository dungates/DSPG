library(shinydashboard)
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
setwd("~/DSPG Dash/Deschutes_Shiny/data")
library(shinyWidgets)
library(dashboardthemes)

##sidebar elements 

sidebar <- dashboardSidebar(
    width = 350,
    #inputs for locations (should work)
    checkboxGroupInput("location", "USGS Location:", 
                       c("Madras" = 2, "Biggs" = 3, "Culver" = 1, "The Dalles" = 4)), 
    checkboxGroupInput("location2", "NOAA Location:", 
                       c("Pelton Butte dam" = 7, "The Dalles" = 4, "Dufur" = 5, "Mt. Wilson" = 6)), 
    #date ranges
    #check date ranges for values 
    dateRangeInput("daterange", "Date Range",
                   start = "2000-01-01", 
                   end = base::Sys.Date(), 
                   min = NULL, max = NULL, format = "yyyy-mm-dd" ##check if format is compatible
                   #, weekstart, language, separator
                   ),
    #input for variables 
    selectInput("variable", "Indicator USGS:",
                       c("Water Temperature" = "00010", "Discharge" = "00060")),
    
    selectInput("variableNOAA", "Indicator NOAA:",
                c("Air Temperature" = "Atemp")),
    
    selectInput("view", "View:", c("Annual Means" = "annual", "Monthly Means" = "monthly", "Daily Means" = "daily")), 
    
    
    selectInput("locationODFW", "ODFW Location:",
                c("Sherars Falls" = "SF", "Round Butte" = "RB")),
    
    # checkboxGroupInput("fish", "Fish Species:",
    #                    c("Summer Steelhead, Hatchery" = "Hatchery.Summer.Steelhead", "Summer Steelhead, Wild" =  "Wild.Summer.Steelhead", 
    #                      "Spring Chinook, Hatchery" = "Hatchery.Spring.Chinook", "Spring Chinook, Wild" =  "Wild.Spring.Chinook",
    #                      "Sockeye, Unmarked" = "No.Mark.Sockeye", "Sockeye, RM" = "Sockeye.RM", "Sockeye, LM" = "Sockeye.LM",
    #                      "Fall Chinook" = "Fall.Chinook", "Bull Trout" =  "Bull.Trout", "Rainbow Trout" = "Rainbow.Trout")),
    dropdown(checkboxGroupInput("fishRB", "Fish Species at Round Butte:",
                                      c("Summer Steelhead, Hatchery" = "Hatchery.Summer.Steelhead", "Summer Steelhead, Wild" =  "Wild.Summer.Steelhead", 
                                        "Spring Chinook, Hatchery" = "Hatchery.Spring.Chinook", "Spring Chinook, Wild" =  "Wild.Spring.Chinook",
                                        "Sockeye, Unmarked" = "No.Mark.Sockeye", "Sockeye, RM" = "Sockeye.RM", "Sockeye, LM" = "Sockeye.LM",
                                        "Fall Chinook" = "Fall.Chinook", "Bull Trout" =  "Bull.Trout", "Rainbow Trout" = "Rainbow.Trout")), circle = FALSE, Label = "Test"),
    
    dropdown(checkboxGroupInput("fishSF", "Fish Species at Sherars Falls:",
                        c("Summer Steelhead, Wild" = "Number.of.Captured.Wild.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, RB" = "Number.of.Captured.Round.Butte.Hatchery.Summer.Steelhead",
                          "Summer Steelhead, Hatchery, Stray" = "Number.of.Captured.Stray.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Total" = "Total.Number.of.Captured.Hatchery.Summer.Steelhead",
                          "Summer Steelhead, Wild, Estimated" = "Estimated.Wild.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, RB, Estimated" = "Estimated.Round.Butte.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Stray, Estimated" = "Estimated.Stray.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Total, Estimated" = "Estimated.Total.Hatchery.Summer.Steelhead")), circle = FALSE, Label = "Test"),
    
    
    
    actionButton("submit", label = "Refresh")

    )



#body elements 

body <-  dashboardBody(
    
    shinyDashboardThemes(theme = "purple_gradient"),
    ##this is just test code 
    fluidRow(
        box(plotlyOutput(outputId = "USGSplot")),
        box(plotlyOutput(outputId = "NOAAplot"))    
    ),
    fluidRow(
    box(plotlyOutput(outputId = "Fishplot")),
    #box(img(src='map.png', align = "right"))
    )
    
    )


#Function that ties it together

ui <- dashboardPage(
    dashboardHeader(title = "Deschutes River Temperature Monitoring Project", titleWidth = 350),
    sidebar = sidebar,
    body = body,
    
    )
