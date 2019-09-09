library(leaflet)
library(rio)
library(dplyr)
library(shinydashboard)

  
ui <- navbarPage("Our world in beer", id="nav",
                 
                 tabPanel("Map",
                          div(class="outer",
                              tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                              
  
  leafletOutput("map", width='100%', height = '100%'),
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = "auto",
                width = 250, height = "auto",
                style = "opacity: 0.8",
                h4("Filter"),
                
    checkboxGroupInput('categories', 'Beer categories:', c('Hybrid', 'German Lager', 'North American Lager', 'North American Ale', 'Irish Ale', 'German Ale', 'British Ale', 'Other Style', 'Belgian and French Ale', 'Other Lager'), selected = c('Hybrid', 'German Lager', 'North American Lager', 'North American Ale', 'Irish Ale', 'German Ale', 'British Ale', 'Other Style', 'Belgian and French Ale', 'Other Lager')),
    
    tags$hr(),
    sliderInput("abv", "ABVs (less than or equal to selected amount):",
              min = 0, max = 50,
              value = 4),
    
    tags$hr(),
  
    sliderInput("ibu", "IBUs (less than or equal to selected amount):",
              min = 0, max = 40,
              value = 15), 
    h6("Warning: Data is sourced from opendatasoft.com, may contain inaccuracies especially with IBU values"),
    uiOutput("test")
)
)
)
)


server <- function(input, output, session) {
  
  clean = read.csv('clean.csv', stringsAsFactors=FALSE, sep=",", header=TRUE)
  data = clean

    pal = colorFactor(c('Paired'), 
    domain = data$Category)
  
  ###filter
  observe({
    data = (subset(clean, ABV <= input$abv & IBU <= input$ibu & (Category %in% input$categories)))

    output$map <- renderLeaflet({
    
     leaflet(data = data) %>%
      addProviderTiles(providers$OpenStreetMap) %>% addCircleMarkers(~data$long, ~data$lat, label = data$beer_id, labelOptions(noHide = TRUE), radius = 3, color = ~pal(data$Category)) %>%
      setView(lng = -30.85, lat = 37.45, zoom = 3) %>% addLegend(position = 'bottomright', pal = pal, values = data$Category, opacity = 0.7)
  })
  })
}

shinyApp(ui = ui, server = server)
