library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw(base_size=16))

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

calweek <- function (x) {
  y <- week(x + days(wday(as.Date(paste(year(x), "01", "01", sep="-")))-1))
  y <- y + ifelse(y == 1 & month(x)==12, 52, 0)
  y
}
theme_set(theme_bw(base_size=16))
d <- data.frame(DATE=seq.Date(from=as.Date("2016-01-01", origin="1970-01-01"),
                              to=as.Date("2016-12-31", origin="1970-01-01"),
                              by="day")) %>%
  mutate(WEEK=calweek(DATE)) %>%
  group_by(WEEK) %>%
  summarise(START_DATE=min(DATE)) %>%
  filter(WEEK >= 23, WEEK <= 36) %>%
  mutate(START_DATE = format(START_DATE, format="%b %d"),
         START_DATE = ordered(START_DATE, levels=START_DATE))

x <- data.frame(WEEKDAY=ordered(rep("Wed", 14), levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
                WEEK=seq(23, 36, 1),
                COLOR=ordered(rep("<70", 14), levels=c("<70", "70-104", ">104")),
                TYPE=ordered("Routine", levels=c("Routine", "Resample")))
x <- left_join(x, d, by="WEEK")

ggplot(x, aes(START_DATE, WEEKDAY, fill=COLOR)) +
  scale_fill_manual("Enterococci", values=c("<70"="grey50")) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

ggplot(x[1, ], aes(START_DATE, WEEKDAY, fill=COLOR)) +
  geom_tile(color="grey80") +
  scale_fill_manual("Enterococci", values=c("<70"="grey50"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

ggplot(x, aes(START_DATE, WEEKDAY, fill=COLOR)) +
  geom_tile(color="grey80") +
  scale_fill_manual("Enterococci", values=c("<70"="grey50"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

x[5, "WEEKDAY"] <- "Tue"
ggplot(x, aes(START_DATE, WEEKDAY, fill=COLOR)) +
  geom_tile(color="grey80") +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

x[c(3, 10), "COLOR"] <- ">104"
ggplot(x, aes(factor(WEEK), WEEKDAY, fill=COLOR)) +
  geom_tile(color="grey80") +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", ">104"="orangered"),
                    labels=c("<70"="Clean (< 104)", ">104"="Dirty (> 104)"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

x <- rbind(x, data.frame(WEEKDAY=c("Thu", "Fri"), WEEK=c(25, 32), COLOR=c("<70", "<70"), TYPE="Resample") %>% left_join(d, by="WEEK"))
ggplot(x, aes(START_DATE, WEEKDAY, fill=COLOR)) +
  geom_tile(color="grey80") +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", ">104"="orangered"),
                    labels=c("<70"="<= 104", ">104"="> 104"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)

ggplot(x, aes(START_DATE, WEEKDAY, fill=COLOR)) +
  geom_tile(size=2) +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", ">104"="orangered"),
                    labels=c("<70"="<= 104", ">104"="> 104"), guide=FALSE) +
  # scale_color_manual(values=c("Routine"=NA, "Resample"="darkolivegreen2"), guide=FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=0.6)


# bacteria data ----

wq <- readRDS('data-wq-resamples.Rdata') %>%
  arrange(SAMPLE_POINT_NAME, SAMPLE_DATE) %>%
  mutate(GT_104 = ENTEROCOCCI > 104,
         GT_70 = ENTEROCOCCI > 70,
         BW_70_104 = GT_70 * !(GT_104))

idx <- which(wq$SAMPLE_POINT_NAME=="CAM-02" & wq$YEAR==2011 & wq$WEEKDAY=="Mon")
wq[idx, "RESAMPLE"] <- TRUE

idx <- which(wq$SAMPLE_POINT_NAME=="CAM-02" & wq$YEAR==2007 & wq$WEEKDAY=="Thu")
wq[idx, "RESAMPLE"] <- TRUE



# timeseries
filter(wq, SAMPLE_POINT_NAME=="CAM-02") %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  ggplot(aes(SAMPLE_DATE, ENTEROCOCCI)) +
  geom_point(color="grey50") +
  scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
                labels=log_labels(c(1, 5), 10^seq(0, 6))) +
  labs(x="Sample Date", y="Enterococci (MPN/100mL)")
filter(wq, SAMPLE_POINT_NAME=="CAM-02") %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  ggplot(aes(SAMPLE_DATE, ENTEROCOCCI, color=COLOR)) +
  geom_point() +
  geom_hline(yintercept=104, color="orangered", linetype="dashed") +
  geom_hline(yintercept=70, color="deepskyblue", linetype="dashed") +
  scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(0, 6)),
                labels=log_labels(c(1, 5), 10^seq(0, 6))) +
  scale_color_manual("", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="orangered"), guide=FALSE) +
  labs(x="Sample Date", y="Enterococci (MPN/100mL)")


theme_set(theme_bw(base_size=12))

stn <- "CAM-02"
filter(wq, SAMPLE_POINT_NAME==stn, WEEK <= 38) %>%
  mutate(WEEK = ordered(WEEK, levels=as.character(seq(20, 38)))) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104", "<104"),
         COLOR = ordered(COLOR, levels=c("<104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile(size = 1) +
  scale_fill_manual("", values=c("<104"="grey50", ">104"="red"), guide=FALSE) +
  scale_x_discrete(drop = FALSE, labels=ifelse(seq(20, 38, 1) %% 2 == 0, seq(20, 38, 1), "")) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=4) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

stn <- "CAM-02"
filter(wq, SAMPLE_POINT_NAME==stn, WEEK <= 38) %>%
  mutate(WEEK = ordered(WEEK, levels=as.character(seq(20, 38)))) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104", "<104"),
         COLOR = ordered(COLOR, levels=c("<104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR, color=RESAMPLE)) +
  geom_tile(size = 1) +
  scale_fill_manual("", values=c("<104"="grey50", ">104"="red"), guide=FALSE) +
  scale_color_manual("", values=c("TRUE"="chartreuse3", "FALSE"=NA), guide=FALSE) +
  scale_x_discrete(drop = FALSE, labels=ifelse(seq(20, 38, 1) %% 2 == 0, seq(20, 38, 1), "")) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=4) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

filter(wq, SAMPLE_POINT_NAME==stn, WEEK <= 38, !RESAMPLE) %>%
  mutate(WEEK = ordered(WEEK, levels=as.character(seq(20, 38)))) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104", "<104"),
         COLOR = ordered(COLOR, levels=c("<104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile(size = 1) +
  scale_fill_manual("", values=c("<104"="grey50", ">104"="red"), guide=FALSE) +
  scale_x_discrete(drop = FALSE, labels=ifelse(seq(20, 38, 1) %% 2 == 0, seq(20, 38, 1), "")) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=4) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

filter(wq, SAMPLE_POINT_NAME==stn, WEEK <= 38, !RESAMPLE) %>%
  mutate(WEEK = ordered(WEEK, levels=as.character(seq(20, 38)))) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile(size = 1) +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="red"),
                    labels=c("<70"="Clean (<70)", "70-104"="Above BAV (70-104)", ">104"="Dirty (>104)")) +
  scale_x_discrete(drop = FALSE, labels=ifelse(seq(20, 38, 1) %% 2 == 0, seq(20, 38, 1), "")) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=4) +
  labs(x="Week of Year", y="Day of Week") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

stn <- "EEB-01"
filter(wq, SAMPLE_POINT_NAME==stn, WEEK <= 38) %>%
  mutate(WEEK = ordered(WEEK, levels=as.character(seq(20, 38)))) %>%
  mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104",
                        ifelse(ENTEROCOCCI > 70, "70-104", "<70")),
         COLOR = ordered(COLOR, levels=c("<70", "70-104", ">104"))) %>%
  # mutate(COLOR = ifelse(ENTEROCOCCI > 104, ">104", "<104"),
  #        COLOR = ordered(COLOR, levels=c("<104", ">104"))) %>%
  ggplot(aes(WEEK, WEEKDAY, fill=COLOR)) +
  geom_tile() +
  scale_fill_manual("Enterococci", values=c("<70"="grey50", "70-104"="deepskyblue", ">104"="red"),
                    labels=c("<70"="Clean (<70)", "70-104"="Above (70-104)", ">104"="Dirty (>104)")) +
  scale_x_discrete(drop = FALSE, labels=ifelse(seq(20, 46, 1) %% 2 == 0, seq(20, 46, 1), "")) +
  scale_y_discrete(breaks=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE) +
  facet_wrap(~YEAR, nrow=4) +
  labs(x="Week of Year", y="Day of Week",
       title=paste0("SAMPLE_POINT_NAME=", stn)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))


stn <- readRDS('data-stn.Rdata')

wq <- left_join(wq, select(stn, SAMPLE_POINT_NAME, TOWN, SITE_NAME) %>% unique)
wq.yr <- filter(wq, !RESAMPLE, !OTHER) %>%
  group_by(SITE_NAME, SAMPLE_POINT_NAME, YEAR) %>%
  summarise(N=n(),
            N_104=sum(GT_104),
            N_70=sum(GT_70)-N_104,
            N_CLEAN=N-N_104-N_70) %>%
  ungroup %>%
  mutate(F_104 = N_104/N,
         F_70 = N_70/N,
         F_CLEAN = N_CLEAN/N)

select(wq.yr, SAMPLE_POINT_NAME, YEAR, F_CLEAN, F_70, F_104) %>%
  gather(VAR, VALUE, F_CLEAN:F_104) %>%
  ggplot(aes(SAMPLE_POINT_NAME, VALUE, fill=VAR)) +
  geom_boxplot(position='dodge')

theme_set(theme_bw(base_size=16))
select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MEAN_N_70=mean(N_70)) %>%
  ungroup %>%
  arrange(MEAN_N_70) %>%
  mutate(SAMPLE_POINT_NAME = ordered(SAMPLE_POINT_NAME, levels=unique(SAMPLE_POINT_NAME))) %>%
  ggplot(aes(SAMPLE_POINT_NAME, MEAN_N_70)) +
  geom_bar(fill="deepskyblue", stat="identity") +
  scale_y_continuous(breaks=seq(0, 2, 0.5)) +
  coord_flip() +
  labs(x="Sampling Site", y="Mean # Additional Resamples per Year") +
  theme(axis.text.y=element_text(size=6))

select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MEAN_N_104=mean(N_104),
            MEAN_N_70=mean(N_70),
            TOTAL=MEAN_N_104+MEAN_N_70) %>%
  ungroup %>%
  arrange(TOTAL) %>%
  mutate(SAMPLE_POINT_NAME = ordered(SAMPLE_POINT_NAME, levels=unique(SAMPLE_POINT_NAME))) %>%
  gather(VARIABLE, VALUE, MEAN_N_104, MEAN_N_70) %>%
  ggplot(aes(SAMPLE_POINT_NAME, VALUE, fill=VARIABLE)) +
  geom_bar(stat="identity") +
  scale_fill_manual("",
                    values=c("MEAN_N_104"="orangered", "MEAN_N_70"="deepskyblue"),
                    labels=c("MEAN_N_104"="> 104 (SSM)", "MEAN_N_70"="> 70 (BAV)")) +
  scale_y_continuous(breaks=seq(0, 6, 1)) +
  coord_flip() +
  labs(x="Sampling Site", y="Mean # Resamples per Year") +
  theme(axis.text.y=element_text(size=6))


theme_set(theme_bw(base_size=16))
select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(YEAR) %>%
  summarise(SUM_N_70=sum(N_70)) %>%
  ungroup %>%
  mutate(YEAR = ordered(YEAR, levels=2015:2006)) %>%
  ggplot(aes(factor(YEAR), SUM_N_70)) +
  geom_bar(fill="deepskyblue", stat="identity") +
  geom_text(aes(label=SUM_N_70), hjust=1, nudge_y=-2, size=8, color="white") +
  geom_hline(yintercept=40.9, linetype="dashed") +
  scale_y_continuous(breaks=seq(0, 70, 5)) +
  coord_flip() +
  labs(x="Year", y="# Additional Resamples")

# average # additional samples per year
select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MEAN_N_70=mean(N_70)) %>%
  summarise(SUM=sum(MEAN_N_70))

select(wq.yr, SAMPLE_POINT_NAME, YEAR, N_CLEAN, N_70, N_104) %>%
  group_by(SAMPLE_POINT_NAME) %>%
  summarise(MEAN_N_70=mean(N_70)) %>%
  summarise(SUM=sum(MEAN_N_70))



# demo viz ----

x <- runif(5*5, min=10, max=39)
x[7] <- 95.3243

x <- matrix(x, nrow=5)
y <- as.data.frame(x)
y$ROW <- 1:nrow(y)

y <- gather(y, COL, VALUE, V1:V5)

ggplot(y, aes(ROW, COL, fill=VALUE)) +
  geom_tile() +
  scale_fill_gradientn("", limits=c(0, 100), colours=rev(RColorBrewer::brewer.pal(n=9, name="Spectral"))) +
  theme_light() +
  theme(panel.grid = element_blank())

t(x[,5:1])


