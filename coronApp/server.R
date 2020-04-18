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

    output$mymap <- renderLeaflet({
        ## This will change some values when rendering the map; it will be usefull
        ## when we like to project different polygons on the map (e.g. weather zones instead of countries)
            colorTiles = "grey"
            groupNames = world$ADMIN
            layers = world$ADMIN
            layerData = world
            minZoomLevel = 3
            latCntr = 30
            lonCntr = 10
            textSize = '10px'
            carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
            leaflet(options = leafletOptions(minZoom = minZoomLevel)) %>% 
                setView(lng = lonCntr, lat = latCntr, zoom = 3) %>%
                addTiles(carto) %>%
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
                                    labelOptions = labelOptions(style = list("color" = "white"), textsize = textSize, noHide = T, direction = 'right', textOnly = T))
        
    })

})
