#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(rgdal)
library(rgeos)
library(stringr)
library(dplyr)
library(plotly)
source("dataImport.R")
source("widgets/toggleSwitch.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "CoViD-19",disable = T),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        useShinyjs(),
        tags$head(
            includeCSS(path = "css/style.css")
        ),
        fluidRow(
            column(
                width = 12,
                leafletOutput(outputId = "mymap"),
                
                switchButton(inputId = "Switch1",
                             label = "",
                             value = TRUE, col = "GB", type = "OO"),
                dropdownButton(
                    tags$h3("List of Input"),
                    selectInput("country", "Country:", 
                                c("None selected", as.character(currentState$country))),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "300px",
                    tooltip = tooltipOptions(title = "Control inputs")
                ),
                box(id = "myBox",
                    titlePanel(textOutput('countryTitle')),
                    infoBoxOutput("casesBox"),infoBoxOutput("deathsBox"),
                    valueBoxOutput("growthBox"),valueBoxOutput("growthBoxDeaths"),
                    valueBoxOutput("fatalRate"),
                    prettyCheckbox(
                        inputId = "showGraph", label = "Show History Graph",value = FALSE,
                        status = "success", outline = FALSE, width = "20px"
                    )
                ),
                box(id = "myPlot",
                    plotlyOutput("plot1")
                )
                
            )
        )
    )
)
# Launch App
# options(shiny.host = '192.168.2.4')
# options(shiny.host = '127.0.0.1')
# options(shiny.port = 7287)
