library(dplyr)
library(shiny)
library(shinydashboard)
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

locations <- mutate(locations,
  POPUP=paste0('<table><tbody>',
               '<tr><td style="text-align:right;padding-right:10px;">SAMPLE_POINT_NAME: </td><td><b>', SAMPLE_POINT_NAME, '</b></td></tr>',
               '<tr><td style="text-align:right;padding-right:10px;">SITE_NAME: </td><td><b>', SITE_NAME, '</b></td></tr>',
               '<tr><td style="text-align:right;padding-right:10px;">TOWN: </td><td><b>', ifelse(is.na(TOWN), "Unspecified", as.character(TOWN)), '</b></td></tr>'),
               '</tbody></table>')

header <- dashboardHeader(
  title = "Maine Healthy Beaches",
  titleWidth = '100%'
)

sidebar <- dashboardSidebar(disable = TRUE)

controls <- column(width = 3,
  box(width = NULL,
      h4('Set Standard (#/100mL)'),
      sliderInput('thresh', label='Enterococcus Standard', min=0, max=200, value=107),
      h4('Filter Stations'),
      sliderInput('nyear', label='Minimum # of Years with Data', min=0, max=max(locations$N_YEAR), value=0),
      sliderInput('nsample', label='Minimum # of Samples', min=0, max=200, value=0)
  ),
  box(width = NULL, title="About",
      p('This interface was designed to explore how changing the water quality standard',
        ' for Enterococcus would effect the frequency of beach closures along the Maine coast',
        ' based on historical sampling data.'),
      p('Data were provided by ', a(href="http://www.mainehealthybeaches.org/", "Maine Healthy Beaches")),
      p('This application was built by ',
        a(href="http://walker-environmental.com", "Jeffrey D Walker, PhD"),
        ' using the ', a(href="http://shiny.rstudio.com", "Shiny"),
        ' web framework by ', a(href="http://rstudio.com", 'RStudio'), '.')
  )
)

content <- column(width = 9,
  fluidRow(
    box(
      width = 6,
      title="Sampling Locations",
      leafletOutput('map', height=600),
      p('Map shows the location of each sampling station. The stations are colored ',
        'by the fraction of samples above the standard (100%=red, 0%=blue). ',
        'Click on a station to view sample distributions and timeseries.')
    ),
    column(width = 6,
           tabPanel(
             box(width=NULL, textOutput('locationInfo')),
             box(width=NULL, plotOutput('histogram')),
             box(width=NULL, plotOutput('ts'))
           ))
  )
)

body <- dashboardBody(
  fluidRow(
    controls,
    content
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  location_markers <- reactive({
    filter(locations, N_YEAR>=input$nyear, N_SAMPLE>=input$nsample)
  })

  output$map <- renderLeaflet({
    frac_exceed <- group_by(ent, SAMPLE_POINT_NAME) %>%
      summarise(FRAC_EXCEED=sum(CONCENTRATION>input$thresh)/n())

    pal <- colorQuantile(c("steelblue", "orangered"), domain = c(0, max(frac_exceed$FRAC_EXCEED)))
    markers <- location_markers()

    markers <- left_join(markers, frac_exceed, by='SAMPLE_POINT_NAME')

    map <- leaflet(markers) %>%
      addTiles() %>%
      addCircleMarkers(lng=~LONGITUDE, lat=~LATITUDE, color=~pal(FRAC_EXCEED),
                       stroke = FALSE, fillOpacity = 0.8, radius = 6,
                       popup = ~POPUP, layerId = ~SAMPLE_POINT_NAME)
    map
  })

  output$locationInfo <- renderText({
    print(input$map_marker_click)
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