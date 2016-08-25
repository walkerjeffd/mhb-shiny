stop("THE SCRIPT flag-resamples.R SHOULD ONLY BE RUN INTERACTIVELY AND NOT WITH source()")

source("init.R")
library(miniUI)

# This script creates a shiny gadget for manually flaging any samples considered
# a "resample" or "other"

# Resamples are defined as a sample collected within a couple days of a previous exceedence
#   note that sometimes resamples are collected in the following week (e.g. if the sample
#   on friday was an exceedence, they might not resample until monday)

# Other samples are those that are not resamples and do not appear to be "routine"
#   there are some years and sites where samples were collected irregularly
#   other samples were flagged to leave only the regular samples for analysis

# After using the shiny gadget to assign resample/other samples, the results
# will be saved to the resamples and others data frames


# load data ---------------------------------------------------------------

calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}

wq <- readRDS("data/rdata/wq-raw.rda") %>%
  mutate(YEAR=year(SAMPLE_DATE),
         WEEK=as.character(calweek(SAMPLE_DATE)),
         WEEKDAY=as.character(wday(SAMPLE_DATE))) %>%
  mutate(WEEK=ordered(WEEK, levels=as.character(seq(20, 46))),
         WEEKDAY=plyr::revalue(WEEKDAY, replace = c("1"="Sun", "2"="Mon", "3"="Tue", "4"="Wed", "5"="Thu", "6"="Fri", "7"="Sat"), warn_missing = FALSE),
         WEEKDAY=factor(WEEKDAY, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  filter(!is.na(ENTEROCOCCI))

stations <- sort(unique(wq$SAMPLE_POINT_NAME))


# shiny gadget ------------------------------------------------------------

ggbrush <- function(data, stn) {
  # data: data frame containing all samples for a single station (stn)
  # click: when a sample is clicked, the id is printed to the console and the
  #        id and stn are appended to the `results` data frame (which is a global)

  ui <- miniPage(
    gadgetTitleBar("Click to select a sample point"),
    miniContentPanel(
      plotOutput("plot", height = "100%", click = "plot_click")
    )
  )

  server <- function(input, output, session) {
    output$plot <- renderPlot({
      data %>%
      ggplot(aes(WEEK, WEEKDAY, color=(ENTEROCOCCI > 104))) +
        geom_point(size=4) +
        geom_point(fill=NA, color="cyan", shape=21, size=4, data=filter(data, id %in% resamples$id)) +
        geom_point(fill=NA, color="orange", shape=21, size=4, data=filter(data, id %in% others$id)) +
        scale_color_manual("Exceeds 104", values=c("FALSE"="grey50", "TRUE"="red")) +
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
        facet_wrap(~YEAR, nrow=5) +
        labs(x="Week of Year", y="Day of Week",
             title=paste0("SAMPLE_POINT_NAME=", stn)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
        theme_bw()
    })

    observe({
      x <- nearPoints(data, input$plot_click)
      if (nrow(x) == 1) {
        # only record the click if there is one sample returned
        id <- x[["id"]][1]
        stn <- x[["SAMPLE_POINT_NAME"]][1]
        results <<- rbind(results, data.frame(id=id, stn=stn))
        cat("Added id = ", id, "\n")
      } else if (nrow(x) > 1) {
        cat("TOO MANY POINTS", "\n")
      } else {
        cat("NOTHING CLICKED", "\n")
      }
    })

    observeEvent(input$done, {
      # return the results data frame on "Done"
      stopApp(results)
    })

    observeEvent(input$cancel, {
      # return nothing on "Cancel"
      stopApp(NULL)
    })
  }

  runGadget(ui, server)
}

# initialize results data frame
results <- data.frame()

# loop through each station and open the widget
# click Done to go to the next station
# click Cancel to stop the loop
for (stn in stations) {
  cat("Station:", stn, "\n")
  x <- filter(wq, SAMPLE_POINT_NAME==stn) %>%
    ggbrush()
  if (is.null(x)) {
    break
  }
}

# IMPORTANT!!!
# the following variables are iterative changes to the resamples array
# note that these are NOT reproducible and only worked when I first did
# this because the results data frame will be different
resamples_1 <- results
resamples_2 <- rbind(resamples_1, filter(results, id %in% c(11191, 6254, 6260)))
resamples_3 <- filter(resamples_2, !(id %in% c(7281, 526, 9719)))
resamples_4 <- filter(resamples_3, !(id %in% others_3$id))
resamples_5 <- filter(resamples_4, !(id %in% c(4880, 6206, 3846, 7337, 8093)))
resamples_6 <- rbind(resamples_5, filter(results, id %in% c(7280, 3849, 7338, 2993, 3518, 2780, 8094)))
resamples_7 <- filter(resamples_6, !(id %in% c(6377, 2563)))
resamples_8 <- filter(resamples_7, !(id %in% c(7619, 7611, 7637)))
resamples_9 <- filter(resamples_8, !(id %in% c(533, 7452, 7479, 7554, 7540, 7574, 4722, 5284, 8411, 3799, 9634, 11815)))
resamples_10 <- rbind(resamples_9, filter(results, id == 3801))
resamples_11 <- filter(resamples_10, id != 4771)
resamples_12 <- filter(resamples_11, id != 10323)

# the following variables are iterative changes to the other array
# same caveat applies
others_1 <- results
others_2 <- rbind(others_1, filter(results, id %in% c(7667, 13165, 11145)))
others_3 <- filter(others_2, !(id %in% c(7281, 526, 9719)))
others_4 <- filter(others_3, !(id == 8851))
others_5 <- rbind(others_4, filter(results, id %in% c(2563, 6377)))
others_6 <- rbind(others_5, filter(results, id %in% c(7619, 7611, 7637)))
others_7 <- rbind(others_6, filter(results, id == 10323))
others_8 <- rbind(others_7, filter(results, id == 4771))

# copy to primary data frames
others <- others_8
resamples <- resamples_12

# export ------------------------------------------------------------------

write.csv(resamples, file="data/csv/samples-resample.csv", row.names=FALSE)
write.csv(others, file="data/csv/samples-other.csv", row.names=FALSE)
