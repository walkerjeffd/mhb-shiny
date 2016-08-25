library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw(base_size=16))


calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}

log_breaks <- function(x, y) {
  # log breaks
  as.vector(outer(x, y, '*'))
}

log_labels <- function(x, y) {
  # labels for log scales with gaps
  x_na <- seq(1, 9)
  x_na[which(!(x_na %in% x))] <- NA
  x <- log_breaks(x_na, y)
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  x
}

# -------------------------------------
# SET UP DATASETS
# -------------------------------------
stn <- readRDS("stn.rda")
wq <- readRDS("wq-resamples.rda")

wq <- wq %>%
  mutate(SAMPLE_TYPE = ifelse(RESAMPLE, "Resample",
                              ifelse(OTHER, "Irregular", "Routine")),
         SAMPLE_TYPE = ordered(SAMPLE_TYPE, levels=c("Routine", "Resample", "Irregular"))) %>%
  mutate(EXCEED_GRP=cut(ENTEROCOCCI, breaks=c(0, 70, 104, Inf)),
         EXCEED_GRP=plyr::revalue(as.character(EXCEED_GRP),
                                  c("(0,70]"="<70",
                                    "(70,104]"="70-104",
                                    "(104,Inf]"=">104")),
         EXCEED_GRP=ordered(EXCEED_GRP, levels=c("<70", "70-104", ">104"))) %>%
  mutate(ACCUMULATION = ifelse(is.na(ACCUMULATION_24HR), ACCUMULATION_48HR, ACCUMULATION_24HR*2),
         PAST_WEATHER = ifelse(is.na(PAST_WEATHER_24HR), PAST_WEATHER_48HR, PAST_WEATHER_24HR)) %>%
  mutate(TIDE = ordered(TIDE, levels = c("LOW", "LOW FLOOD", "FLOOD", "HIGH FLOOD", "HIGH",
                                         "HIGH EBB", "EBB", "LOW EBB")),
         CURRENT_WEATHER = ordered(CURRENT_WEATHER, levels=c("CLEAR", "PARTLY CLOUDY", "OVERCAST", "RAIN")),
         PAST_WEATHER = ordered(PAST_WEATHER, levels=c("NO RAIN", "LIGHT RAIN", "MEDIUM RAIN", "HIGH RAIN")),
         CURRENT = ordered(CURRENT, levels=c("SLOW CURRENT", "MEDIUM CURRENT", "RAPID CURRENT")))


wq2 <- wq %>%
  arrange(SAMPLE_POINT_NAME, SAMPLE_DATE) %>%
  mutate(GT_104 = ENTEROCOCCI > 104,
         GT_70 = ENTEROCOCCI > 70,
         BW_70_104 = GT_70 * !(GT_104))


wq2 <- left_join(wq2, select(stn, SAMPLE_POINT_NAME, TOWN, SITE_NAME) %>% unique, by="SAMPLE_POINT_NAME")
wq.yr <- filter(wq2, !RESAMPLE, !OTHER) %>%
  group_by(SITE_NAME, SAMPLE_POINT_NAME, YEAR) %>%
  summarise(N=n(),
            N_104=sum(GT_104),
            N_70=sum(GT_70)-N_104,
            N_CLEAN=N-N_104-N_70) %>%
  ungroup %>%
  mutate(F_104 = N_104/N,
         F_70 = N_70/N,
         F_CLEAN = N_CLEAN/N)

wq.yr.site <- select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MEAN_N_104=mean(N_104),
            MEAN_N_70=mean(N_70),
            TOTAL=MEAN_N_104+MEAN_N_70) %>%
  ungroup %>%
  arrange(desc(TOTAL)) %>%
  mutate(SAMPLE_POINT_NAME = ordered(SAMPLE_POINT_NAME, levels=unique(SAMPLE_POINT_NAME))) %>%
  gather(VARIABLE, VALUE, MEAN_N_104, MEAN_N_70)
#
# select(wq.yr, SAMPLE_POINT_NAME, YEAR, F_CLEAN, F_70, F_104) %>%
#   gather(VAR, VALUE, F_CLEAN:F_104) %>%
#   ggplot(aes(SAMPLE_POINT_NAME, VALUE, fill=VAR)) +
#   geom_boxplot(position='dodge')
#
# theme_set(theme_bw(base_size=16))
# select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
#   group_by(SAMPLE_POINT_NAME) %>%
#   summarise(MEAN_N_70=mean(N_70)) %>%
#   ungroup %>%
#   arrange(MEAN_N_70) %>%
#   mutate(SAMPLE_POINT_NAME = ordered(SAMPLE_POINT_NAME, levels=unique(SAMPLE_POINT_NAME))) %>%
#   ggplot(aes(SAMPLE_POINT_NAME, MEAN_N_70)) +
#   geom_bar(fill="deepskyblue", stat="identity") +
#   scale_y_continuous(breaks=seq(0, 2, 0.5)) +
#   coord_flip() +
#   labs(x="Sampling Site", y="Mean # Additional Resamples per Year") +
#   theme(axis.text.y=element_text(size=6))
#
# # number samples by year
# select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
#   group_by(SAMPLE_POINT_NAME) %>%
#   summarise(MEAN_N_104=mean(N_104),
#             MEAN_N_70=mean(N_70),
#             TOTAL=MEAN_N_104+MEAN_N_70) %>%
#   ungroup %>%
#   arrange(TOTAL) %>%
#   mutate(SAMPLE_POINT_NAME = ordered(SAMPLE_POINT_NAME, levels=unique(SAMPLE_POINT_NAME))) %>%
#   gather(VARIABLE, VALUE, MEAN_N_104, MEAN_N_70) %>%
#   ggplot(aes(SAMPLE_POINT_NAME, VALUE, fill=VARIABLE)) +
#   geom_bar(stat="identity") +
#   scale_fill_manual("",
#                     values=c("MEAN_N_104"="orangered", "MEAN_N_70"="deepskyblue"),
#                     labels=c("MEAN_N_104"="> 104 (SSM)", "MEAN_N_70"="> 70 (BAV)")) +
#   scale_y_continuous(breaks=seq(0, 6, 1)) +
#   coord_flip() +
#   labs(x="Sampling Site", y="Mean # Resamples per Year") +
#   theme(axis.text.y=element_text(size=6))
#
# # number resamples by date
# select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
#   group_by(YEAR) %>%
#   summarise(SUM_N_70=sum(N_70)) %>%
#   ungroup %>%
#   mutate(YEAR = ordered(YEAR, levels=2015:2006)) %>%
#   ggplot(aes(factor(YEAR), SUM_N_70)) +
#   geom_bar(fill="deepskyblue", stat="identity") +
#   geom_text(aes(label=SUM_N_70), hjust=1, nudge_y=-2, size=8, color="white") +
#   geom_hline(yintercept=40.9, linetype="dashed") +
#   scale_y_continuous(breaks=seq(0, 70, 5)) +
#   coord_flip() +
#   labs(x="Year", y="# Additional Resamples")
#
# # average # additional samples per year
# select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
#   group_by(SAMPLE_POINT_NAME) %>%
#   summarise(MEAN_N_70=mean(N_70)) %>%
#   summarise(SUM=sum(MEAN_N_70))

# select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
#   group_by(SAMPLE_POINT_NAME) %>%
#   summarise(MEAN_N_70=mean(N_70)) %>%
#   summarise(SUM=sum(MEAN_N_70))
#


# idx <- which(wq$SAMPLE_POINT_NAME=="CAM-02" & wq$YEAR==2011 & wq$WEEKDAY=="Mon")
# wq[idx, "RESAMPLE"] <- TRUE
#
# idx <- which(wq$SAMPLE_POINT_NAME=="CAM-02" & wq$YEAR==2007 & wq$WEEKDAY=="Thu")
# wq[idx, "RESAMPLE"] <- TRUE


stat_by_site <- filter(wq, !is.na(ENTEROCOCCI)) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(COUNT       = n(),
            FRAC_GT_SSM = sum(ENTEROCOCCI > 104)/COUNT,
            FRAC_GT_BAV = sum(ENTEROCOCCI > 70)/COUNT,
            FRAC_BAV_SSM = sum((ENTEROCOCCI < 104) * (ENTEROCOCCI > 70))/COUNT,
            MEDIAN      = median(ENTEROCOCCI),
            GEOMEAN     = exp(mean(log(ENTEROCOCCI))))
stn <- left_join(stn, stat_by_site, by="SAMPLE_POINT_NAME")

color_var_labels <- c("COUNT"="# Samples",
                      "FRAC_GT_SSM"="% > SSM",
                      "FRAC_GT_BAV"="% > BAV",
                      "FRAC_BAV_SSM"="% between<br />BAV and SSM",
                      "MEDIAN"="Median Ent.<br />MPN/100mL",
                      "GEOMEAN"="Geomean Ent.<br />MPN/100mL")

stn_choices <- stn$SAMPLE_POINT_NAME
names(stn_choices) <- paste0(stn$TOWN, " - ", stn$SITE_NAME, " - ", stn$SAMPLE_POINT_NAME)
stn_choices <- c("Select a station..." = "", stn_choices[order(stn_choices)])

variable_labels <- c("WATER_SURFACE"="Water Surface",
                     "CURRENT"="Current Speed",
                     "TIDE"="Tidal Stage",
                     "CURRENT_WEATHER"="Current Weather",
                     "PAST_WEATHER"="Past Weather",
                     "ACCUMULATION"="Antecedent Rainfall (in / 48hr)",
                     "TEMP_WATER"="Water Temperature (deg C)",
                     "TEMP_AIR"="Air Temperature (deg C)",
                     "SALINITY"="Salinity (ppt)")

# -------------------------------------
# UI
# -------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "ME Healthy Beaches"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station Explorer", tabName = "station", icon = icon("map-marker")),
      menuItem("Overview Comparisons", tabName = "overview", icon = icon("globe")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    tags$ul(class="sidebar-menu",
            tags$li(class="header",
                    div(style='text-align:center;font-size:1.3em;margin-top:20px;margin-bottom:20px',
                        'Developed by ', br(),
                        a(href="http://walkerenvres.com", 'WalkerEnvironmentalResearch'), br(),
                        'for', br(),
                        a(href="http://www.mainehealthybeaches.org/", 'Maine Healthy Beaches'), br(),
                        'using', br(),
                        a(href="http://shiny.rstudio.com/", 'Shiny Web Framework'), br(),
                        'by ', a(href="http://rstudio.com", "RStudio"), br(), br(),
                        'Last updated:', br(),
                        strong('March 2016')
                    )
            )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "station",
        fluidRow(
          box(
            title = "Station Map",
            width=4,
            style="margin-bottom:200px",
            leafletOutput("stnMap"),
            selectInput("stnSelect", label = "Choose a station by clicking on the map or selecting from this dropdown menu",
                        choices = stn_choices),
            selectInput("stnMapColor", label = "Select a variable used to color stations",
                        selected = "MEDIAN",
                        choices = c("# of Samples"="COUNT",
                                    "Median Ent. Concentration"="MEDIAN",
                                    "Geometric Mean Ent. Concentration"="GEOMEAN",
                                    "% Samples > SSM"="FRAC_GT_SSM",
                                    "% Samples > BAV"="FRAC_GT_BAV",
                                    "% Samples between BAV and SSM"="FRAC_BAV_SSM"))
          ),
          tabBox(
            id = "stnTabset",
            height = "700px",
            width = 8,
            tabPanel("Info",
              tableOutput("stnInfo")
            ),
            tabPanel("Timeseries",
              checkboxGroupInput("stnTimeseriesType", "Sample Types", c("Resample", "Irregular", "Routine"),
                                 selected = c("Resample", "Irregular", "Routine")),
              plotOutput("stnTimeseriesChart")
            ),
            tabPanel("Calendar",
              checkboxGroupInput("stnCalendarType", "Sample Types", c("Resample", "Irregular", "Routine"),
                                 selected = c("Resample", "Irregular", "Routine")),
              plotOutput("stnCalendarPlot")
            ),
            tabPanel("Histogram",
              checkboxGroupInput("stnHistogramType", "Sample Types", c("Resample", "Irregular", "Routine"),
                                 selected = c("Resample", "Irregular", "Routine")),
              plotOutput("stnHistogram")
            ),
            tabPanel("Boxplot",
              fluidRow(
                column(width=6,
                       selectInput("stnBoxVariable", "Variable",
                                   choices=c("Select a variable..."="",
                                             "Water Surface"="WATER_SURFACE",
                                             "Current Speed"="CURRENT",
                                             "Tidal Stage"="TIDE",
                                             "Current Weather"="CURRENT_WEATHER",
                                             "Past Weather"="PAST_WEATHER"))
                ),
                column(width=6,
                       checkboxGroupInput("stnBoxType", "Sample Types", c("Resample", "Irregular", "Routine"),
                                          selected = c("Resample", "Irregular", "Routine"))
                )
              ),
              plotOutput("stnBoxplot")
            ),
            tabPanel("Scatterplot",
                     fluidRow(
                       column(width = 6,
                              selectInput("stnScatterVariable", "Variable",
                                          choices=c("Select an variable"="",
                                                    "Antecedent Rainfall"="ACCUMULATION",
                                                    "Water Temperature"="TEMP_WATER",
                                                    "Air Temperature"="TEMP_AIR",
                                                    "Salinity"="SALINITY"))),
                       column(width = 6,
                              selectInput("stnScatterGroup", "Group By",
                                          choices=c("None"="NONE",
                                                    "Water Surface"="WATER_SURFACE",
                                                    "Current Speed"="CURRENT",
                                                    "Tidal Stage"="TIDE",
                                                    "Current Weather"="CURRENT_WEATHER",
                                                    "Past Weather"="PAST_WEATHER")))
                     ),
                     plotOutput("stnScatter")
            )
          )
        )
      ),
      tabItem(tabName = "overview",
        box(width = 12,
            plotOutput("overResampleBySite")
        )
        # box(width = 6,
        #     plotOutput("overResampleByYear"))
      ),
      tabItem(tabName = "about",
        box(title = "About", width = 6,
            p("This web application was designed to explore water quality data collected by the ", a(href="http://www.mainehealthybeaches.org/", "Maine Healthy Beaches (MHB) Program"), "."),
            p("The MHB water quality dataset contains routine (primarily weekly) samples collected by teams of volunteers at beaches along the coast of Maine.",
              "More details about this dataset can be found at the", a(href="http://www.mainehealthybeaches.org/science.html", "MHB Science Page"), "."),
            p("The application was built by Jeff Walker of ", a(href="http://walkerenvres.com", "Walker Environmental Research"), "to support work by the MHB Technical Advisory Committee (TAC).")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  pal <- colorNumeric(
    palette = rev(RColorBrewer::brewer.pal(n=9, name="Spectral")),
    domain = stat_by_site$FRAC_GT_SSM
  )

  map <- leaflet(stn) %>%
    addTiles() %>%
    setView(zoom = 7,
            lat = 43.76,
            lng = -69.435)
  output$stnMap = renderLeaflet(map)

  rv <- reactiveValues()
  rv$stn <- NULL

  stnInfo <- reactive({
    req(rv$stn)
    filter(stn, SAMPLE_POINT_NAME==rv$stn)
  })

  stnData <- reactive({
    validate(need(rv$stn != "", "Select a station on the map or the dropdown menu below the map."))
    filter(wq, SAMPLE_POINT_NAME==rv$stn)
  })

  observeEvent(input$stnMap_marker_click, {
    rv$stn <- input$stnMap_marker_click$id
    updateSelectInput(session, "stnSelect", selected = rv$stn)
  })

  observe({
    rv$stn <- input$stnSelect
  })

  observe({
    # update map on select station
    x <- stnInfo()

    leafletProxy("stnMap") %>%
      clearPopups() %>%
      setView(zoom = 14,
              lat = x$LATITUDE[1],
              lng = x$LONGITUDE[1]) %>%
      addPopups(lat = x$LATITUDE[1],
                lng = x$LONGITUDE[1],
                popup = paste0(tags$strong('Town: '), x$TOWN[1], tags$br(),
                               tags$strong('Beach: '), x$SITE_NAME[1], tags$br(),
                               tags$strong('Site: '), x$SAMPLE_POINT_NAME[1]))

  })

  observe({
    # update map on change color
    color_var <- input$stnMapColor
    req(color_var)

    pal <- colorNumeric(
      palette = rev(RColorBrewer::brewer.pal(n=9, name="Spectral")),
      domain = stat_by_site[[color_var]]
    )
    colors <- pal(stn[[color_var]])

    if (color_var %in% c("FRAC_GT_SSM", "FRAC_GT_BAV", "FRAC_BAV_SSM")) {
      legend_formatter <- labelFormat(suffix = " %",
                                   transform = function(x) 100*x)
    } else {
      legend_formatter <- labelFormat()
    }

    leafletProxy("stnMap") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = stn[['LONGITUDE']],
        lat = stn[['LATITUDE']],
        layerId = stn[['SAMPLE_POINT_NAME']],
        fillColor = colors,
        radius = 5,
        stroke = TRUE,
        color = "black",
        weight = 1,
        fillOpacity = 0.9) %>%
      clearControls() %>%
      addLegend("bottomright", pal = pal, values = stn[[color_var]],
                title = color_var_labels[[color_var]],
                opacity = 1,
                labFormat = legend_formatter)

  })

  output$stnInfo <- renderTable({
    validate(need(rv$stn != "", "Select a station on the map or the dropdown menu below the map."))

    info_labels <- c("TOWN"="Town", "SITE_NAME"="Beach", "SAMPLE_POINT_NAME"="Station ID",
                     "LATITUDE"="Latitude", "LONGITUDE"="Longitude",
                     "COUNT"="# Samples", "GEOMEAN"="Geomean Ent. (MPN/100mL)",
                     "MEDIAN"="Median Ent. (MPN/100mL)")
    info <- stnInfo()
    info <- info[, names(info_labels)]
    names(info) <- info_labels[names(info)]
    info <- t(info)
    data.frame(FIELD=rownames(info), VALUE=unname(info))
  }, include.colnames=FALSE, include.rownames=FALSE)

  output$stnTimeseriesChart <- renderPlot({
    validate(need(length(input$stnTimeseriesType) > 0, "Select at least one Sample Type above"))

    stnData() %>%
      filter(SAMPLE_TYPE %in% input$stnTimeseriesType) %>%
      ggplot(aes(SAMPLE_DATE, ENTEROCOCCI, color=EXCEED_GRP)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
      scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
                    labels=log_labels(c(1, 5), 10^seq(0, 6))) +
      labs(x="Sample Date", y="Enterococci (MPN/100mL)") +
      theme(panel.grid.minor.y=element_blank())
  })

  output$stnHistogram <- renderPlot({
    # x <- stnData()
    # ggplot(x, aes(ENTEROCOCCI, fill=EXCEED_GRP)) +
    #   geom_histogram(binwidth=0.1) +
    #   scale_fill_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
    #   scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
    #                 labels=log_labels(c(1, 5), 10^seq(0, 6))) +
    #   labs(x="Enterococci (MPN/100mL)", y="# of Samples") +
    #   theme(axis.text.x = element_text(angle=90, hjust=1, v=0.5),
    #         panel.grid.minor.y = element_blank())
    validate(need(length(input$stnHistogramType) > 0, "Select at least one Sample Type above"))

    stnData() %>%
      filter(SAMPLE_TYPE %in% input$stnHistogramType) %>%
      mutate(ENTEROCOCCI=ifelse(ENTEROCOCCI > 400, 400, ENTEROCOCCI)) %>%
      ggplot(aes(ENTEROCOCCI, fill=EXCEED_GRP)) +
      geom_histogram(binwidth = 10, center = 5) +
      scale_fill_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
      scale_x_continuous(breaks=seq(0, 400, 50),
                         labels=c("0", "50", "100", "150", "200", "250", "300", "350", "400\nor more")) +
      # scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
      #               labels=log_labels(c(1, 5), 10^seq(0, 6))) +
      labs(x="Enterococci (MPN/100mL)", y="# of Samples") +
      theme(axis.text.x = element_text(angle=90, hjust=1, v=0.5),
            panel.grid.minor.y = element_blank())
  })

  output$stnCalendarPlot <- renderPlot({
    validate(need(length(input$stnCalendarType) > 0, "Select at least one Sample Type above"))

    stnData() %>%
      filter(SAMPLE_TYPE %in% input$stnCalendarType) %>%
      mutate(WEEK=ordered(WEEK, levels=as.character(seq(20, 46))),
             WEEKDAY=plyr::revalue(WEEKDAY, replace = c("1"="Sun", "2"="Mon", "3"="Tue", "4"="Wed", "5"="Thu", "6"="Fri", "7"="Sat"), warn_missing = FALSE),
             WEEKDAY=factor(WEEKDAY, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
      ggplot(aes(WEEK, WEEKDAY, fill=EXCEED_GRP)) +
      geom_tile() +
      geom_tile(aes(color=SAMPLE_TYPE), fill=NA, size=1) +
      scale_fill_manual("Ent Category", values=c("<70"="grey70", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
      scale_color_manual("Sample Type", values=c("Regular"="white", "Resample"="grey20", "Irregular"="olivedrab3"), drop=FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
      facet_wrap(~YEAR, nrow=5) +
      labs(x="Week of Year", y="Day of Week") +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  }, height=600)

  output$stnBoxplot <- renderPlot({
    var_x <- input$stnBoxVariable

    validate(need(var_x != "", "Select a variable from the dropdown above."),
             need(length(input$stnBoxType) > 0, "Select at least one Sample Type above"))

    stnData() %>%
      filter(SAMPLE_TYPE %in% input$stnBoxType) %>%
      ggplot() +
      aes_string(x=var_x, y="ENTEROCOCCI") +
      geom_boxplot(fill = "grey80") +
      labs(x=variable_labels[[var_x]], y="Enterococci (MPN/100mL)") +
      scale_x_discrete(drop = FALSE) +
      scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
                    labels=log_labels(c(1, 5), 10^seq(0, 6))) +
      theme(axis.text.x = element_text(angle=90, hjust=1, v=0.5),
            panel.grid.minor.y = element_blank())
  })

  output$stnScatter <- renderPlot({
    data <- stnData()
    var_x <- input$stnScatterVariable
    var_facet <- input$stnScatterGroup

    validate(need(var_x != "", "Select a variable from the dropdown above."))

    p <- ggplot(data) +
      aes_string(x=var_x, y="ENTEROCOCCI", color="EXCEED_GRP") +
      geom_point() +
      labs(x=variable_labels[[var_x]], y="Enterococci (MPN/100mL)") +
      scale_color_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
      scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
                    labels=log_labels(c(1, 5), 10^seq(0, 6))) +
      theme(panel.grid.minor.y = element_blank())

    if (var_facet != "NONE") {
      p <- p + facet_wrap(as.formula(paste0("~", var_facet)))
    }

    p
  })

  output$overResampleBySite <- renderPlot({
    p <- wq.yr.site %>%
      ggplot(aes(SAMPLE_POINT_NAME, VALUE, fill=VARIABLE)) +
      geom_bar(stat="identity") +
      scale_fill_manual("",
                        values=c("MEAN_N_104"="orangered", "MEAN_N_70"="deepskyblue"),
                        labels=c("MEAN_N_104"="> 104 (SSM)", "MEAN_N_70"="> 70 (BAV)")) +
      scale_y_continuous(breaks=seq(0, 6, 1)) +
      labs(x="Sampling Site", y="Mean # Resamples per Year") +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p
  })

  # output$overResampleByYear <- renderPlot({
  #   p <- select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  #     group_by(YEAR) %>%
  #     summarise(SUM_N_70=sum(N_70)) %>%
  #     ungroup %>%
  #     mutate(YEAR = ordered(YEAR, levels=2015:2006)) %>%
  #     ggplot(aes(factor(YEAR), SUM_N_70)) +
  #     geom_bar(fill="deepskyblue", stat="identity") +
  #     geom_text(aes(label=SUM_N_70), hjust=1, nudge_y=-2, size=8, color="white") +
  #     geom_hline(yintercept=40.9, linetype="dashed") +
  #     scale_y_continuous(breaks=seq(0, 70, 5)) +
  #     coord_flip() +
  #     labs(x="Year", y="# Additional Resamples")
  #   p
  # })
})

# Run the application
shinyApp(ui = ui, server = server)

