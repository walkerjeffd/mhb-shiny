library(dplyr)
library(shiny)
library(jsonlite)
library(shinydashboard)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(leaflet)

theme_set(theme_bw())

load('data.Rdata')

ui <- dashboardPage(
  dashboardHeader(title = "ME Healthy Beaches"),
  dashboardSidebar(
    sliderInput('thresh', label='Enterococcus Standard', min=0, max=200, value=107)
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
        box(leafletOutput('map', height=400), width = '100%')
      )
    ),
    fluidRow(
      box(plotOutput('histogram', height=300)),
      box(plotOutput('ts', height=300))
    )
  )
)

server <- function(input, output) {
  map <- leaflet(locations) %>%
    addTiles() %>%
    addMarkers(lng=~LONGITUDE, lat=~LATITUDE, popup = ~SAMPLE_POINT_NAME, layerId = ~SAMPLE_POINT_NAME)

  output$map <- renderLeaflet(map)

  output$text <- renderText({
    input$map_marker_click$id
  })

  output$histogram <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id)
    threshold <- input$thresh

    data <- filter(df, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI') %>%
      mutate(EXCEED=CONCENTRATION > threshold)

    p <- ggplot(data, aes(CONCENTRATION, fill=EXCEED)) +
      geom_histogram() +
      scale_x_log10() +
      geom_vline(xint=threshold, linetype=2, color='red') +
      scale_fill_manual('Exceedence', values=c('TRUE'='orangered', 'FALSE'='steelblue'))
    print(p)
  })

  output$ts <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id)
    threshold <- input$thresh
    data <- filter(df, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI') %>%
      mutate(EXCEED=CONCENTRATION > threshold)
    p <- ggplot(data, aes(SAMPLE_DATE, CONCENTRATION, color=EXCEED)) +
      geom_point() +
      scale_y_log10() +
      geom_hline(yint=threshold, linetype=2, color='red') +
      scale_color_manual('Exceedence', values=c('TRUE'='orangered', 'FALSE'='steelblue'))
    print(p)
  })
}

shinyApp(ui, server)