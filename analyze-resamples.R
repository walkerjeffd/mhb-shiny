library(ggplot2)
library(dplyr)
library(lubridate)
theme_set(theme_bw())

# analyze the resamples using the data frame created from flag-resamples

calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}

wq <- readRDS('data/rdata/wq-resamples.rda') %>%
  arrange(SAMPLE_POINT_NAME, SAMPLE_DATE) %>%
  mutate(GT_104 = ENTEROCOCCI > 104,
         GT_70 = ENTEROCOCCI > 70,
         BW_70_104 = GT_70 * !(GT_104))

# check that all resamples follow an exceedence
wq %>%
  group_by(SAMPLE_POINT_NAME) %>%
  mutate(PREV_GT_104=lag(GT_104)) %>%
  filter(RESAMPLE, !PREV_GT_104) %>%
  select(SAMPLE_POINT_NAME, id, SAMPLE_DATE, WEEK) %>%
  nrow %>%
  (function (x) {
    cat("nrow =", x, "\n")
    stopifnot(x == 0)
  })

# compute fraction of routine samples between 70-104 by site and year
bav_site_year <- filter(wq, !RESAMPLE, !OTHER) %>%
  group_by(SAMPLE_POINT_NAME, YEAR) %>%
  summarise(N = n(),
            N_GT_104 = sum(GT_104),
            N_GT_70 = sum(GT_70),
            N_BW_70_104 = sum(BW_70_104),
            FRAC_BW_70_104 = N_BW_70_104/N) %>%
  ungroup
arrange(bav_site_year, desc(FRAC_BW_70_104)) %>% View

# compute fraction of samples between 70-104 by site and year
bav_site <- bav_site_year %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MIN_FRAC_BW_70_104=min(FRAC_BW_70_104),
            MAX_FRAC_BW_70_104=max(FRAC_BW_70_104),
            MEAN_FRAC_BW_70_104=mean(FRAC_BW_70_104),
            MEDIAN_FRAC_BW_70_104=median(FRAC_BW_70_104),
            N_YEAR=n(),
            N_SAMPLE=sum(N),
            MEAN_N_SAMPLE_PER_YEAR=mean(N),
            MEDIAN_N_SAMPLE_PER_YEAR=median(N),
            # mean/median/max number of additional resamples per year under BAV
            MEAN_N_RESAMPLE_PER_YEAR=MEAN_FRAC_BW_70_104*MEAN_N_SAMPLE_PER_YEAR,
            MEDIAN_N_RESAMPLE_PER_YEAR=MEDIAN_FRAC_BW_70_104*MEDIAN_N_SAMPLE_PER_YEAR,
            MAX_N_RESAMPLE_PRE_YEAR=MAX_FRAC_BW_70_104*MEAN_N_SAMPLE_PER_YEAR)


arrange(bav_site, desc(MEDIAN_N_RESAMPLE_PER_YEAR)) %>% View

# highest median number of additional resamples is almost 3 (WIL-02)
stn <- "WIL-02"
filter(wq, SAMPLE_POINT_NAME==stn, !RESAMPLE, !OTHER) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile() +
  scale_fill_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="red")) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=5) +
  labs(x="Week of Year", y="Day of Week",
       title=paste0("SAMPLE_POINT_NAME=", stn)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))


# median total number of samples per year (across all sites)
# 1151
sum(bav_site$MEDIAN_N_SAMPLE_PER_YEAR)

# mean total number of samples per year (across all sites)
# 1140
sum(bav_site$MEAN_N_SAMPLE_PER_YEAR)

# median total number of samples per year (across all sites)
# 24 (2% increase in number of samples)
sum(bav_site$MEDIAN_N_RESAMPLE_PER_YEAR)

# mean number of additional resamples per year under BAV (across all sites)
# 42 (3.7% increase in number of samples)
sum(bav_site$MEAN_N_RESAMPLE_PER_YEAR)



#
# filter(wq, SAMPLE_POINT_NAME=="EEB-01", !RESAMPLE, !OTHER) %>%
#   ggbrush()


# compute fraction of historical resamples
# that were exceedences
x <- arrange(wq, SAMPLE_POINT_NAME, SAMPLE_DATE) %>%
  mutate(DATE = as.Date(SAMPLE_DATE)) %>%
  group_by(SAMPLE_POINT_NAME, YEAR) %>%
  mutate(PREV_GT_104=lag(GT_104),
         PREV_ENT=lag(ENTEROCOCCI),
         DIFFTIME = as.numeric(difftime(DATE, lag(DATE), units = "days"))) %>%
  ungroup %>%
  filter(RESAMPLE)

# plot of resample concentration versus previous concentration (all of which were exceedences)
x %>%
  ggplot(aes(PREV_ENT, ENTEROCOCCI, color=GT_104)) +
  geom_point() +
  facet_wrap(~DIFFTIME) +
  scale_x_log10() +
  scale_y_log10()

# fraction of resamples that were exceedences by site
# where there were at least 10 resamples overall
# to exclude sites with very few resamples (generally clean)
filter(x, DIFFTIME %in% c(1, 2)) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(N=n(),
            N_GT_104 = sum(GT_104),
            FRAC=N_GT_104/N) %>%
  filter(N>10) %>%
  arrange(desc(FRAC))

# highest fraction is LIN-3
# plot shows a lot of consecutive exceedences
stn <- "LIN-3"
filter(wq, SAMPLE_POINT_NAME==stn) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile() +
  scale_fill_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="red")) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=5) +
  labs(x="Week of Year", y="Day of Week",
       title=paste0("SAMPLE_POINT_NAME=", stn)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
