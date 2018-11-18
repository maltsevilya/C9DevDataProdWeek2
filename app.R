library(shiny, quietly = TRUE, warn.conflicts = FALSE)
library(leaflet, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(maps, quietly = TRUE, warn.conflicts = FALSE)

landingsData <- read.csv("Meteorite_Landings.csv", stringsAsFactors = FALSE) %>%
  mutate(nametype = factor(nametype), recclass = factor(recclass), fall = factor(fall)) %>%
  mutate(datetime = parse_date_time(year, c("%m/%d/%Y %I:%M:%S"))) %>%
  mutate(latitude = reclat) %>%
  mutate(longitude = reclong) %>%
  mutate(mass = mass..g.) %>%
  mutate(fallColor = ifelse(fall == "Fell", "blue", "red")) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  select(-GeoLocation, -year, -reclat, -reclong) %>%
  mutate(label = paste(sep = "<br>", 
                       paste0("Name: <b>", name, "</b>"), 
                       paste0("Class: ", recclass), 
                       paste0("Type: ", nametype), 
                       paste0("Year: ", format(datetime, "%Y")), 
                       paste0("Mass: ", paste0(mass, " g"))))
landingsData <- landingsData %>%
  mutate(location = as.factor(map.where(x = landingsData$longitude, y = landingsData$latitude)))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 100,
                htmlOutput("date")
  ),
  absolutePanel(top = 10, right = 10,
                selectInput("locations", 
                            "Known meteorite landings location", 
                            c("-", "World", as.character(sort(landingsData$location))))
  )
)

server <- function(input, output) {
  
  output$date <- renderText({
    paste0("<b>08 Nov, 2018",
           "<br>",
           "Ilya Maltsev</b>",
           "<br>",
           "Developing Data Products",
           "<br>",
           "Week 2: Assignment")
  })
  
  perLocationData <- reactive({
    landingsData %>% filter(input$locations != "-" & (input$locations == "World" | location == input$locations))
  })
  
  output$map <- renderLeaflet({
    landingsData %>%
      leaflet() %>% 
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  selectedLocation <- reactive({
    input$locations
  })
  
  observe({
    location <- selectedLocation()
    
    leafletProxy("map", data = perLocationData()) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       popup = ~label,
                       color = ~fallColor)
  })
  
  observe({
    proxy <- leafletProxy("map", data = perLocationData())

    proxy %>%
      clearControls() %>%
      addLegend(position = "bottomright", labels = c("Fell", "Found"), colors = c("blue", "red"))
  })
}

shinyApp(ui = ui, server = server)
