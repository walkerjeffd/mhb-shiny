library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(scales)
library(leaflet)

theme_set(theme_bw())

load('data.Rdata')
ent <- filter(df, PARAMETER_NAME=='ENTEROCOCCI') %>%
  left_join(select(locations, SAMPLE_POINT_NAME, SITE_NAME), by="SAMPLE_POINT_NAME")

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
               '<tr><td style="text-align:right;padding-right:10px;">TOWN: </td><td><b>', ifelse(is.na(TOWN), "Unspecified", as.character(TOWN)), '</b></td></tr>',
               '</tbody></table>'))

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
      leafletOutput('map', height=400),
      p('Map shows the location of each sampling station. The stations are colored ',
        'by the fraction of samples above the standard (100%=red, 0%=blue). ',
        'Click on a station to view sample distributions and timeseries.')
    ),
    column(width = 6,
           box(width=NULL,
               tabsetPanel(
                 tabPanel("Overview",
                          plotOutput('overviewBars', height="400px")
                 ),
                 tabPanel("Selected Location",
                          textOutput('locationText'),
                          tableOutput('locationInfo'),
                          plotOutput('locationHistogram', height="200px"),
                          plotOutput('locationTimeseries', height="200px"),
                          plotOutput('locationAnnual', height="200px"),
                          textOutput('locationAnnualText')
                 )
               )
           )
    )
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

    pal <- colorQuantile(c("steelblue", "orangered"), domain = c(0, max(0.5, max(frac_exceed$FRAC_EXCEED))))
    markers <- location_markers()

    markers <- left_join(markers, frac_exceed, by='SAMPLE_POINT_NAME')

    map <- leaflet(markers) %>%
      addTiles() %>%
      addCircleMarkers(lng=~LONGITUDE, lat=~LATITUDE, color=~pal(FRAC_EXCEED),
                       stroke = FALSE, fillOpacity = 0.8, radius = 6,
                       popup = ~POPUP, layerId = ~SAMPLE_POINT_NAME)
    map
  })

  output$overviewBars <- renderPlot({
    threshold <- input$thresh
    filtered_locations <- location_markers()

    data <- filter(ent, PARAMETER_NAME=='ENTEROCOCCI',
                   SAMPLE_POINT_NAME %in% filtered_locations$SAMPLE_POINT_NAME) %>%
      mutate(EXCEED=CONCENTRATION > threshold) %>%
      group_by(SITE_NAME) %>%
      summarise(N=n(),
                EXCEED=sum(EXCEED),
                FRAC_EXCEED=EXCEED/N) %>%
      arrange(desc(FRAC_EXCEED)) %>%
      mutate(SITE_NAME=ordered(SITE_NAME, levels=SITE_NAME))

    p <- ggplot(data, aes(SITE_NAME, FRAC_EXCEED, fill=FRAC_EXCEED)) +
      geom_bar(stat='identity') +
      scale_y_continuous(limits=c(0, NA), labels=percent) +
      scale_fill_gradient(low="steelblue", high="orangered",
                          limits=c(0, NA), guide=FALSE) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8)) +
      labs(x='', y='% Samples Exceeding Standard')

    print(p)
  })

  output$locationText <- renderText({
    if (is.null(input$map_marker_click$id)) {
      return('Select a location on the map.')
    }
    return('')
  })

  output$locationInfo <- renderTable({
    if (is.null(input$map_marker_click$id)) {
      return(NULL);
    }
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id) %>%
      select(SAMPLE_POINT_NAME, SITE_NAME, TOWN)
    out <- t(location)
    colnames(out) <- ' '
    out
  })

  output$locationHistogram <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id)
    threshold <- input$thresh

    data <- filter(ent, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI') %>%
      mutate(EXCEED=CONCENTRATION > threshold)

    p <- ggplot(data, aes(CONCENTRATION, fill=EXCEED)) +
      geom_histogram() +
      scale_x_log10() +
      geom_vline(xint=threshold, linetype=2, color='red') +
      scale_fill_manual('Exceedence', values=c('TRUE'='orangered', 'FALSE'='steelblue')) +
      labs(x="Ent Concentration (#/100mL)", y="# Samples") +
      theme(legend.position='top')
    print(p)
  })

  output$locationTimeseries <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id)
    threshold <- input$thresh
    data <- filter(ent, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI') %>%
      mutate(EXCEED=CONCENTRATION > threshold)
    p <- ggplot(data, aes(SAMPLE_DATE, CONCENTRATION, color=EXCEED)) +
      geom_point() +
      scale_y_log10() +
      geom_hline(yint=threshold, linetype=2, color='red') +
      scale_color_manual('Exceedence', values=c('TRUE'='orangered', 'FALSE'='steelblue'), guide=FALSE) +
      labs(y="Ent Concentration (#/100mL)", x='')
    print(p)
  })

  output$locationAnnual <- renderPlot({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    location_id <- input$map_marker_click$id
    location <- filter(locations, SAMPLE_POINT_NAME==location_id)
    threshold <- input$thresh
    data <- filter(ent, SAMPLE_POINT_NAME==location_id, PARAMETER_NAME=='ENTEROCOCCI') %>%
      mutate(YEAR=year(SAMPLE_DATE),
             EXCEED=CONCENTRATION > threshold) %>%
      group_by(YEAR) %>%
      summarise(N=n(),
                EXCEED=sum(EXCEED),
                FRAC_EXCEED=EXCEED/N)
    p <- ggplot(data, aes(factor(YEAR), FRAC_EXCEED, fill=FRAC_EXCEED)) +
      geom_bar(stat='identity') +
      geom_text(aes(y=0.01, label=N), vjust=0) +
      scale_y_continuous(limits=c(0, NA), labels=percent) +
      scale_fill_gradient(low="steelblue", high="orangered",
                          limits=c(0, NA), guide=FALSE) +
      labs(y="% Samples Exceeding Standard", x='')
    print(p)
  })

  output$locationAnnualText <- renderText({
    if (is.null(input$map_marker_click) || is.null(input$map_marker_click$id))
      return(NULL)
    'Numbers at base of each bar are # of samples / year'
  })
}

shinyApp(ui, server)