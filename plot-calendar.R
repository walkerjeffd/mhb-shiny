library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
theme_set(theme_bw())


mutate(ent, YEAR=year(SAMPLE_DATE)) %>%
  group_by(SAMPLE_POINT_NAME, YEAR) %>%
  tally %>%
  ungroup %>%
  ggplot(aes(factor(YEAR), SAMPLE_POINT_NAME, fill=n)) +
  geom_tile() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n=9, name="YlGnBu")) +
  theme(aspect.ratio=1)


calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}

# why are all years except 2015 "NORMAL ENVIRONMENTAL SAMPLE"? Then 2015 has "NOT APPLICABLE" and "REANALYSIS"
filter(ent, SAMPLE_POINT_NAME=="EEB-01") %>%
  mutate(YEAR=year(SAMPLE_DATE),
         WEEK=calweek(SAMPLE_DATE),
         WEEKDAY=wday(SAMPLE_DATE)) %>%
  ggplot(aes(factor(WEEK), factor(WEEKDAY), fill=SAMPLE_TYPE_QUALIFIER)) +
  geom_tile() +
  facet_wrap(~YEAR)


pdf("calendar-plots.pdf", width=11, height=8.5)
for (x in sort(unique(ent$SAMPLE_POINT_NAME))) {
  cat(x, "\n")
  p <- filter(ent, SAMPLE_POINT_NAME==x) %>%
    mutate(YEAR=year(SAMPLE_DATE),
           WEEK=as.character(calweek(SAMPLE_DATE)),
           WEEKDAY=as.character(wday(SAMPLE_DATE))) %>%
    mutate(WEEK=ordered(WEEK, levels=as.character(seq(20, 46))),
           WEEKDAY=plyr::revalue(WEEKDAY, replace = c("1"="Sun", "2"="Mon", "3"="Tue", "4"="Wed", "5"="Thu", "6"="Fri", "7"="Sat"), warn_missing = FALSE),
           WEEKDAY=factor(WEEKDAY, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
    ggplot(aes(WEEK, WEEKDAY, fill=(CONCENTRATION > 104))) +
    geom_tile() +
    scale_fill_manual("Exceeds 104", values=c("FALSE"="grey50", "TRUE"="red")) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
    facet_wrap(~YEAR, nrow=5) +
    labs(x="Week of Year", y="Day of Week",
         title=paste0("SAMPLE_POINT_NAME=", x)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p)
}
dev.off()



# only one ENT result per day and sample point
mutate(ent, DATE=as.Date(SAMPLE_DATE)) %>%
  select(SAMPLE_POINT_NAME, DATE) %>%
  group_by(SAMPLE_POINT_NAME, DATE) %>%
  tally %>%
  filter(n>1)

