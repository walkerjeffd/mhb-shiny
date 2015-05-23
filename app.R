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
ent <- filter(df, PARAMETER_NAME=='ENTEROCOCCI')

ent_site_tally <- group_by(ent, SAMPLE_POINT_NAME) %>%
  summarise(N_SAMPLE=n(),
            START_DATE=min(SAMPLE_DATE),
            END_DATE=max(SAMPLE_DATE),
            START_YEAR=min(year(SAMPLE_DATE)),
            END_YEAR=max(year(SAMPLE_DATE)),
            N_YEAR=END_YEAR-START_YEAR+1)
locations <- left_join(locations, ent_site_tally, by='SAMPLE_POINT_NAME')

ui <- dashboardPage(
  dashboardHeader(title = "ME Healthy Beaches"),
  dashboardSidebar(
    sliderInput('thresh', label='Enterococcus Standard', min=0, max=200, value=107),
    sliderInput('nyear', label='Min # Years', min=0, max=max(locations$N_YEAR), value=0),
    sliderInput('nsample', label='Min # Samples', min=0, max=200, value=0)
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
  location_markers <- reactive({
    print(input$nyear)
    filter(locations, N_YEAR>=input$nyear, N_SAMPLE>=input$nsample)
  })

  output$map <- renderLeaflet({
    pal <- colorQuantile(c("steelblue", "orangered"), domain = c(0, 1))
    markers <- location_markers()
    frac_exceed <- group_by(ent, SAMPLE_POINT_NAME) %>%
      summarise(FRAC_EXCEED=sum(CONCENTRATION>input$thresh)/n())

    markers <- left_join(markers, frac_exceed, by='SAMPLE_POINT_NAME')
    print(summary(markers))

    map <- leaflet(markers) %>%
      addTiles() %>%
      addCircleMarkers(lng=~LONGITUDE, lat=~LATITUDE, color=~pal(FRAC_EXCEED),
                       stroke = FALSE, fillOpacity = 0.8, radius = 6,
                       popup = ~SAMPLE_POINT_NAME, layerId = ~SAMPLE_POINT_NAME)
    map
  })

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