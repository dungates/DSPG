library(shinydashboard)
library(shiny)
library(shinyjs)
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
library(dashboardthemes)

##sidebar elements 

sidebar <- dashboardSidebar(
    width = 350,
    #inputs for locations (should work)
    checkboxGroupInput("location", "USGS Location:", 
                       c("Madras" = 2, "Biggs" = 3, "Culver" = 1, "The Dalles" = 4), selected = 4), 
    checkboxGroupInput("location2", "NOAA Location:", 
                       c("Pelton Butte dam" = 7, "The Dalles" = 4, "Dufur" = 5, "Mt. Wilson" = 6),
                       selected = 4), 
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
    selectInput("fishRB", "Fish Species at Round Butte:",
                                      c("Summer Steelhead, Hatchery" = "Hatchery.Summer.Steelhead", "Summer Steelhead, Wild" =  "Wild.Summer.Steelhead", 
                                        "Spring Chinook, Hatchery" = "Hatchery.Spring.Chinook", "Spring Chinook, Wild" =  "Wild.Spring.Chinook",
                                        "Sockeye, Unmarked" = "No.Mark.Sockeye", "Sockeye, RM" = "Sockeye.RM", "Sockeye, LM" = "Sockeye.LM",
                                        "Fall Chinook" = "Fall.Chinook", "Bull Trout" =  "Bull.Trout", "Rainbow Trout" = "Rainbow.Trout")),
    
    selectInput("fishSF", "Fish Species at Sherars Falls:",
                        c("Summer Steelhead, Wild" = "Number.of.Captured.Wild.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, RB" = "Number.of.Captured.Round.Butte.Hatchery.Summer.Steelhead",
                          "Summer Steelhead, Hatchery, Stray" = "Number.of.Captured.Stray.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Total" = "Total.Number.of.Captured.Hatchery.Summer.Steelhead",
                          "Summer Steelhead, Wild, Estimated" = "Estimated.Wild.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, RB, Estimated" = "Estimated.Round.Butte.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Stray, Estimated" = "Estimated.Stray.Hatchery.Summer.Steelhead", 
                          "Summer Steelhead, Hatchery, Total, Estimated" = "Estimated.Total.Hatchery.Summer.Steelhead")),
    
    
    
    actionButton("submit", label = "Submit")

    )



#body elements 

body <-  dashboardBody(
    shinyjs::useShinyjs(),
    
    tabItems(
        tabItem("dashboard",
                div(p("Dashboard tab content"))
        ),
        tabItem("widgets",
                "Widgets tab content"
        ),
        tabItem("subitem1",
                "Sub-item 1 tab content"
        ),
        tabItem("subitem2",
                "Sub-item 2 tab content"
        )
    ),
    
    shinyDashboardThemes(theme = "purple_gradient"),
    ##this is just test code 
    fluidRow(
        box(plotly::plotlyOutput(outputId = "USGSplot"), 
            title = h4(HTML("<center>USGS Data</center>"))),
        box(plotly::plotlyOutput(outputId = "NOAAplot"), 
            title = h4(HTML("<center>NOAA Data</center>")))    
    ),
    fluidRow(
    box(plotly::plotlyOutput(outputId = "Fishplot", height = "485px"), 
        h4(HTML("<center>Fish Data</center>"))),
    box(img(src='map.png', width = "100%", align = "right")))
)


#Function that ties it together

ui <- dashboardPage(
    dashboardHeader(title = h4(HTML("Deschutes River Temperature<br/> Monitoring Project")), titleWidth = 350),
    sidebar = sidebar,
    body = body,
    title = "DSPG Deschutes Project")
