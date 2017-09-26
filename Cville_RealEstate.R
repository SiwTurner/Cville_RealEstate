#Load packages####
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(geojsonio)
library(sp)
library(shiny)
library(rsconnect)
library(RCurl)


#Load Data####
#GeoJson
setwd("~/13 Data Science Training/Real_Estate_2")
cvilleGeo <- geojson_read("Parcel_Boundary_Area.geojson", method = "local", what = "sp")
cville <- cvilleGeo

#load prices for assessments
data1 <- read.csv("Real_Estate_Current_Assessments.csv") %>% as_tibble()
data1 <- mutate(data1, PIN = ï..ParcelNumber)
data1 <- select(data1, PIN, CurrentAssessedValue, FullAddress)
data1 <- inner_join(cvilleGeo@data, data1)
data1 <- select(data1, PIN, CurrentAssessedValue, FullAddress)

#Load real estate base 
data2 <- read.csv("Real_Estate_Base_Active.csv") %>% as_tibble()
data2 <- mutate(data2, PIN = ParcelNumber)


#Transform Data####
#join prices and state code
data3 <- inner_join(data1, data2)
data3 <- select(data3, PIN, CurrentAssessedValue, FullAddress, StateCode)
data3 <- filter(data3, StateCode == "1.0 Residential (Urban)")
data3 <- mutate(data3, logValue = log10(CurrentAssessedValue))


#Match Geojson with assessment data
cville@data <- data.frame(cville@data, data3[match(cville@data$PIN, data3$PIN), ])

#setup leaflet vars####
bins <- c(0, 100000, 300000, 500000, 1000000, 10000000)
pal <- colorBin("viridis", domain = cville@data$CurrentAssessedValue, bins = bins)


#Create User Interface####
ui <-bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
  
)

#Define Server
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet(cville) %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addPolygons(stroke = TRUE, color = "black", weight = 1,
                  smoothFactor = 0.3, fillOpacity = 0.6, fillColor = ~pal(CurrentAssessedValue),
                  label = ~paste0(FullAddress, ": ", formatC(CurrentAssessedValue, big.mark = ","))) %>%
      addLegend(pal = pal, values = ~CurrentAssessedValue, title = "2017 Assessed Residential Property Value", opacity = .6)
    
  })
  
}

# Run App ####
shinyApp(ui, server)
