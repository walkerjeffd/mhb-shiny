source("init.R")


# load data ---------------------------------------------------------------

ent <- readRDS("data/rdata/wq-resamples.rda") %>%
  mutate(SAMPLE_TYPE = ifelse(RESAMPLE, "Resample",
                              ifelse(OTHER, "Irregular", "Routine")),
         SAMPLE_TYPE = ordered(SAMPLE_TYPE, levels=c("Routine", "Resample", "Irregular"))) %>%
  mutate(EXCEED_GRP=cut(ENTEROCOCCI, breaks=c(0, 70, 104, Inf)),
         EXCEED_GRP=plyr::revalue(as.character(EXCEED_GRP),
                                  c("(0,70]"="<70",
                                    "(70,104]"="70-104",
                                    "(104,Inf]"=">104")),
         EXCEED_GRP=ordered(EXCEED_GRP, levels=c("<70", "70-104", ">104")))

table(ent$SAMPLE_TYPE)

# only one ENT result per day and sample point
mutate(ent, DATE=as.Date(SAMPLE_DATE)) %>%
  select(SAMPLE_POINT_NAME, DATE) %>%
  group_by(SAMPLE_POINT_NAME, DATE) %>%
  tally %>%
  (function (x) {
    stopifnot(all(x$n == 1))
  })

# functions ---------------------------------------------------------------

calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}


# plots -------------------------------------------------------------------


# heatmap of sample counts by year and station
mutate(ent, YEAR=year(SAMPLE_DATE)) %>%
  group_by(SAMPLE_POINT_NAME, YEAR) %>%
  tally %>%
  ungroup %>%
  ggplot(aes(factor(YEAR), SAMPLE_POINT_NAME, fill=n)) +
  geom_tile() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n=9, name="YlGnBu")) +
  labs(x = "Year", y = "Station") +
  theme(aspect.ratio=1)


# pdf ---------------------------------------------------------------------

pdf("pdf/calendar-plots.pdf", width=11, height=8.5)

for (x in sort(unique(ent$SAMPLE_POINT_NAME))) {
  cat(x, "\n")
  p <- filter(ent, SAMPLE_POINT_NAME==x) %>%
    ggplot(aes(WEEK, WEEKDAY, fill = EXCEED_GRP)) +
    geom_tile() +
    geom_tile(aes(color=SAMPLE_TYPE), fill=NA, size=1) +
    scale_fill_manual("Ent Category", values=c("<70"="grey70", "70-104"="deepskyblue", ">104"="orangered"), drop=FALSE) +
    scale_color_manual("Sample Type", values=c("Regular"="white", "Resample"="grey20", "Irregular"="olivedrab3"), drop=FALSE) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
    facet_wrap(~YEAR, nrow=5) +
    labs(x="Week of Year", y="Day of Week",
         title=paste0("SAMPLE_POINT_NAME=", x)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p)
}

dev.off()

