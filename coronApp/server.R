#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    shinyjs::hide(id = "myBox")
    shinyjs::hide(id = "myPlot")
    ## use reactive values to store the data you generate from observing the shape click
    rv <- reactiveValues()
    rv$myDf <- NULL
    proj4string(world) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    output$mymap <- renderLeaflet({
        ## This will change some values when rendering the map; it will be usefull
        ## when we like to project different polygons on the map (e.g. weather zones instead of countries)
        if(input$Switch1){
            palCases <- colorNumeric(
                palette = "Blues",
                domain = c(0, 2, 10, 50, 100, 500, 1000, 5000)
            )
            colorTiles = palCases(currentState$currentCasesPer1m)
            groupNames = currentState$country
            layers = currentState$country
            layerData = world
            minZoomLevel = 3
            latCntr = 30
            lonCntr = 10
            textSize = '5px'
            carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
            map <- leaflet(options = leafletOptions(minZoom = minZoomLevel)) %>% 
                setView(lng = lonCntr, lat = latCntr, zoom = 3) %>%
                addTiles(carto, attribution = tile_attrib <- "Map data &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap contributors</a>") %>%
                clearShapes() %>% 
                addPolygons(fillOpacity = 1,
                            fillColor = colorTiles, 
                            color = "black", 
                            group = groupNames,
                            data = layerData, 
                            weight=1,
                            layerId = layers,
                            highlight = highlightOptions(
                                weight = 1,
                                color = "black",
                                fillOpacity = .8)) %>%
                addLabelOnlyMarkers(lng = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[1]])))), 
                                    lat = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[2]])))), 
                                    label = as.character(groupNames),
                                    labelOptions = labelOptions(style = list("color" = "white"), 
                                                                textsize = textSize, noHide = T, direction = 'auto', textOnly = T))
            map %>% addLegend("topright", pal = palCases, 
                      values = c(0,2,10,50,100,500,1000,5000),
                      title = "Cases/1m Pop",
                      labFormat = function(type, cuts, p) {paste0( c(0,2,10,50,100,500,1000,">5000"))},
                      opacity = 1
            )
            
        }else{
            palDeaths <- colorNumeric(
                palette = "Reds",
                domain = c(0, 2, 10, 50, 100, 500)
            )
            colorTiles = palDeaths(currentState$currentDeathsPer1m)
            groupNames = currentState$country
            layers = currentState$country
            layerData = world
            minZoomLevel = 3
            latCntr = 30
            lonCntr = 10
            textSize = '5px'
            carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
            map <- leaflet(options = leafletOptions(minZoom = minZoomLevel)) %>% 
                setView(lng = lonCntr, lat = latCntr, zoom = 3) %>%
                addTiles(carto, attribution = tile_attrib <- "Map data &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap contributors</a>") %>%
                clearShapes() %>% 
                addPolygons(fillOpacity = 1,
                            fillColor = colorTiles, 
                            color = "black", 
                            group = groupNames,
                            data = layerData, 
                            weight=1,
                            layerId = layers,
                            highlight = highlightOptions(
                                weight = 1,
                                color = "black",
                                fillOpacity = .8)) %>%
                addLabelOnlyMarkers(lng = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[1]])))), 
                                    lat = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[2]])))), 
                                    label = as.character(groupNames),
                                    labelOptions = labelOptions(style = list("color" = "white"), 
                                                                textsize = textSize, noHide = T, direction = 'auto', textOnly = T))
            map %>% addLegend("topright", palDeaths, 
                      values = c(0,2,10,50,100,500),
                      title = "Deaths/1m Pop", 
                      labFormat = function(type, cuts, p) {paste0( c(0,2,10,50,100,">500"))},
                      opacity = 1
            )
        }
    })

    observeEvent(input$mymap_shape_click, {
        event <- input$mymap_shape_click
        
        updateSelectInput(session, "country",
              label = "Country",
              choices = c("None selected", as.character(currentState$country)),
              selected = event$group
        )
    })
    
    observeEvent(input$Switch1, {
        shinyjs::hide(id = "myBox")
        shinyjs::hide(id = "myPlot")
    })

    observeEvent(input$country,{
        proxy <- leafletProxy("mymap")
        layerData <- world
        groupNames = currentState$country
        if(input$country == "None selected") {
            proxy %>% removeShape(layerId = "Selected")
            shinyjs::hide(id = "myBox")
            shinyjs::hide(id = "myPlot")
        }else{
            selected <- layerData[match(input$country, groupNames),]
            proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
            
            proxy %>% addPolygons(data = selected, 
                                  fillColor = "#95e26a",
                                  fillOpacity = 1, 
                                  color = "black",
                                  weight = 1, 
                                  stroke = T,
                                  layerId = "Selected",
                                  group = "None selected")
            rv$myDf <- data.frame(country = input$country, cases = currentState[currentState$country == input$country, "cases"], deaths = currentState[currentState$country == input$country, "deaths"])
            shinyjs::show(id = "myBox")
            sumCases <- new(Class = "array")
            sumDeaths <- new(Class = "array")
            dateTime <- new(Class = "array")
            cases <- new(Class = "array")
            deaths <- new(Class = "array")
            for (i in 1:length(data[data$countriesAndTerritories == input$country, "cases"])){
                sumCases[i] <- sum(rev(data[data$countriesAndTerritories == input$country, "cases"])[1:i])
                sumDeaths[i] <- sum(rev(data[data$countriesAndTerritories == input$country, "deaths"])[1:i])
                cases[i] <- rev(data[data$countriesAndTerritories == input$country, "cases"])[i]
                deaths[i] <- rev(data[data$countriesAndTerritories == input$country, "deaths"])[i]
                dateTime[i] <- as.Date(rev(data[data$countriesAndTerritories == input$country, "dateRep"])[i], format = '%d/%m/%Y')
            }
            casePlot<-data.frame(date=as.Date(dateTime, origin = "1970-01-01"), sumCases = sumCases, sumDeaths = sumDeaths, cases = cases, deaths = deaths)
            diff_cases <- casePlot[length(casePlot$sumCases),"sumCases"] - casePlot[which(casePlot$sumCases != 0)[1],"cases"]
            diff_deaths <- casePlot[length(casePlot$sumDeaths),"sumDeaths"] - casePlot[which(casePlot$sumCases != 0)[1],"deaths"]
            diff_days <- length(casePlot$sumCases) - which(casePlot$sumCases != 0)[1]
            growth_rate <- format(diff_cases/diff_days, digits = 0)
            growth_rate_deaths <- format(diff_deaths/diff_days, digits = 0)
          
            ## you can now 'output' your generated data however you want
            tbl <- eventReactive(input$total, {
                rv$myDf
            })
            observeEvent(input$showGraph,{
                if(!input$showGraph){
                    print("TRUE")
                    shinyjs::hide(id = "myPlot")
                }else{
                    shinyjs::show(id = "myPlot")
                }
            })
            output$casesBox <- renderInfoBox({
                tbl()
                
                infoBox(
                    " ", rv$myDf$cases, "Cases", icon = icon("ambulance"),
                    color = "light-blue"
                )
            })
            output$deathsBox <- renderInfoBox({
                tbl()
                infoBox(
                    " ", rv$myDf$deaths, "Deaths", icon = icon("frown"),
                    color = "red"
                )
            })
            output$growthBox <- renderValueBox({
                valueBox(growth_rate, "Cases Growth")
            })
            output$growthBoxDeaths <- renderValueBox({
                valueBox(growth_rate_deaths, "Death Growth",color = "yellow")
            })   
            output$fatalRate <- renderValueBox({
                valueBox(paste(currentState$fatalRate[match(input$country, currentState$country)],"%", sep = ""), "Fatal Rate",color = "orange")
            })  
            output$countryTitle <- renderText({ input$country })
            ## plot
            output$plot1 <- renderPlotly({
                ay <- list(
                    tickfont = list(color = "red"),
                    overlaying = "y",
                    side = "right", automargin = TRUE,
                    title = "deaths"
                )
                fig <- plot_ly(casePlot, x= ~date, y= ~sumCases,type = "scatter",mode = "lines", name = "Cases")
                fig <- fig %>% add_trace(x = casePlot$date, y = casePlot$sumDeaths, type = "scatter", name = "Deaths", mode = "lines", yaxis = "y2")
                fig <- fig %>% add_trace(x = casePlot$date, y = casePlot$cases, type = "bar", yaxis = "y")
                fig <- fig %>% layout(autosize = T, height = 200, title = "", yaxis2 = ay,
                                      xaxis = list(title="Time"))
                fig <- fig %>% layout(legend = list(x = 0.1, y = 0.9))
            })              
        }
    })

    
})
