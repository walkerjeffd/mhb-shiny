library(dplyr)
library(shiny)
library(jsonlite)
library(shinydashboard)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(leaflet)

DATA_DIR <- '~/Dropbox/Work/mhb/data/'

df <- readRDS(file=file.path(DATA_DIR, 'data.Rdata'))
locations <- read.csv(file=file.path(DATA_DIR, 'locations.csv'), stringsAsFactors=FALSE)
names(locations)[1] <- 'EGAD_SEQ'

df <- filter(df, SITE_SEQUENCE_NUMBER %in% locations$EGAD_SEQ)
locations <- filter(locations, EGAD_SEQ %in% unique(df$SITE_SEQUENCE_NUMBER))

ui <- dashboardPage(
  dashboardHeader(title = "ME Healthy Beaches"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(leafletOutput('map', height=400)),
      box(textOutput('text'))
    ),
    fluidRow(
      box(plotOutput('ts', height=400))
    )
  )
)

server <- function(input, output) {
  map <- leaflet(locations) %>%
    addTiles() %>%
    addMarkers(lng=~LONGITUDE, lat=~LATITUDE, popup = ~CURRENT_SITE_NAME, layerId = ~CURRENT_SAMPLE_POINT_NAME)

  output$map <- renderLeaflet(map)

  output$text <- renderText({
#     names(input)
    input$map_marker_click$id
  })

  output$ts <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, CURRENT_SAMPLE_POINT_NAME==location_id)
    data <- filter(df, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI')
    p <- ggplot(data, aes(SAMPLE_DATE, CONCENTRATION)) +
      geom_point() +
      scale_y_log10()
    print(p)
  })
}

shinyApp(ui, server)