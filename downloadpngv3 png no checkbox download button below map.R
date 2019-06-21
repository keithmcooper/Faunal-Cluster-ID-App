
library(shiny)
library(mapview)
library(leaflet)
library(htmlwidgets)
library(webshot)




ui <- fluidPage(

        sidebarPanel(),
        mainPanel(
        leafletOutput("map",height=1000),br(),downloadButton('map_down'))
)

server <- function(input, output, session) {
  
  mymap <- reactive({
    leaflet() %>%addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%addCircleMarkers(lng = -5,lat = 54.2,radius = 5,stroke = F, color = "black",weight = 1,fill = TRUE, fillColor ="red",fillOpacity = 1)%>%setView(-3,54.6,zoom=6)
}) 
  
  output$map <- renderLeaflet({
    mymap()
  })
  
  observe({
    leafletProxy("map") 
  })
  # map that will be downloaded
  mapdown <- reactive({
    # we need to specify coordinates (and zoom level) that we are currently viewing
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    mymap() %>%  setView(lng = (lngRng[1]+lngRng[2])/2, lat = (latRng[1]+latRng[2])/2, zoom = input$map_zoom)
  })
  
  output$map_down <- downloadHandler(
    filename = 'mymap.png',
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # using saveWidget and webshot (old)
      #saveWidget(mapdown(), "temp.html", selfcontained = FALSE)
      #webshot("temp.html", file = file, cliprect = "viewport")
      
      # using mapshot we can substitute the above two lines of code
       mapshot(mapdown(), file = file, cliprect = "viewport")
    }
  )
}

shinyApp(ui, server)
