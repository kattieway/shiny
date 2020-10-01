
library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(readxl)
library(maps)
library(dplyr)

wd <- getwd()

data <- read_excel(paste(wd, "/oil.xlsx", sep = ""), col_names = TRUE)
new <- sapply(data[3:10], function(x) as.integer(x))
source <- cbind(data[1], new)

data(world.cities)

# merge the desired cols from that data frame with yours by country
df <- world.cities %>%
    filter(capital == 1) %>%
    dplyr::select(Country = country.etc, lat, lng = long) %>%
    left_join(source, ., by = "Country")

start_country <- filter(df, df$OIL != 0)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    
     
    absolutePanel(top = 10, right = 10,
                  pickerInput("countries", label = "Select a Country:",
                              choices = c("All Countries", start_country$Country),
                              options = list(
                                  
                                  `live-search` = TRUE)
                  )
    ),
    
    absolutePanel(top = 90, right = 10,
                  pickerInput("resource", label = "Select resource:",
                              choices = colnames(df[2:9]),
                              options = list(
                                  
                                  `live-search` = TRUE)
                  )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
   
    
    filteredData <- reactive({
        if (input$resource == "OIL") {
            
            
            if(input$countries == "All Countries") {
                df <- filter(df, df$OIL != 0)

            } else {
                df <- filter(df, df$OIL != 0)
                df <- filter(df, df$Country == input$countries)
                
            }
        } else {
            
            if(input$countries == "All Countries"){
                df <- filter(df, df[input$resource] != 0)
                start_country <- filter(df, df[input$resource] != 0)
            } else {
                
                df <- filter(df, df[input$resource] != 0)
                start_country <- filter(df, df[input$resource] != 0)
                df <- filter(df, df$Country == input$countries)
            }
           
        }
        
        
    })
    

    output$map <- renderLeaflet({
        leaflet(filteredData()) %>%
            addProviderTiles(providers$Esri.WorldTopoMap) %>%
            addTiles() %>%
            
            addCircleMarkers()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
