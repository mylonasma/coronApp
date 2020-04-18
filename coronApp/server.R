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
    ## use reactive values to store the data you generate from observing the shape click
    rv <- reactiveValues()
    rv$myDf <- NULL
    
    proj4string(world) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    output$mymap <- renderLeaflet({
        ## This will change some values when rendering the map; it will be usefull
        ## when we like to project different polygons on the map (e.g. weather zones instead of countries)
        if(input$Switch1){
            colorTiles = currentState$currentCasesPer1mColor
            groupNames = currentState$country
            layers = currentState$country
            layerData = world
            minZoomLevel = 3
            latCntr = 30
            lonCntr = 10
            textSize = '5px'
            carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
            leaflet(options = leafletOptions(minZoom = minZoomLevel)) %>% 
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
                addLabelOnlyMarkers(lng = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[1]])))), lat = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[2]])))), label = as.character(groupNames),
                                    labelOptions = labelOptions(style = list("color" = "white"), textsize = textSize, noHide = T, direction = 'auto', textOnly = T))
        }else{
            colorTiles = currentState$currentDeathsPer1mColor
            groupNames = currentState$country
            layers = currentState$country
            layerData = world
            minZoomLevel = 3
            latCntr = 30
            lonCntr = 10
            textSize = '5px'
            carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
            leaflet(options = leafletOptions(minZoom = minZoomLevel)) %>% 
                setView(lng = lonCntr, lat = latCntr, zoom = 3) %>%
                addTiles(carto, attribution = tile_attrib <- "Map data &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap contributors</a>, Imagery &copy; 2013 <a href='http://www.kartena.se/'>Kartena</a>") %>%
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
                addLabelOnlyMarkers(lng = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[1]])))), lat = unlist(lapply(layerData@polygons,function(p) as.numeric(data.frame(p@labpt[[2]])))), label = as.character(groupNames),
                                    labelOptions = labelOptions(style = list("color" = "white"), textsize = textSize, noHide = T, direction = 'auto', textOnly = T))
            
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

    observeEvent(input$country,{
        proxy <- leafletProxy("mymap")
        layerData <- world
        groupNames = world$ADMIN
        if(input$country == "None selected") {
            proxy %>% removeShape(layerId = "Selected")
            shinyjs::hide(id = "myBox")
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
        }

        ## you can now 'output' your generated data however you want
        tbl <- eventReactive(input$total, {
            rv$myDf
        })
        output$casesBox <- renderInfoBox({
            tbl()
            infoBox(
                rv$myDf$country, rv$myDf$cases, "Cases", icon = icon("ambulance"),
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
    })

    
})
