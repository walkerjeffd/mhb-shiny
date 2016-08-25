source("init.R")


# load data ---------------------------------------------------------------

df <- read_excel(config$dataset,
                 sheet="Monitoring Data",
                 col_types=c(rep("text", 5),
                             "date",
                             rep("text", 25)))


# clean data --------------------------------------------------------------

df <- df[, -31]

names(df) <- str_replace_all(names(df), ' ', '_')
df <- rename(df,
             SITE_LATITUDE_UTM=`SITE_LATITUDE_(UTM)`,
             SITE_LONGITUDE_UTM=`SITE_LONGITUDE_(UTM)`)

df <- mutate(df,
             QUANITATION_LIMIT=as.numeric(QUANITATION_LIMIT),
             SITE_NAME=str_trim(SITE_NAME),
             SITE_SEQUENCE_NUMBER=as.integer(SITE_SEQUENCE_NUMBER),
             SAMPLE_SEQUENCE_NUMBER=as.integer(SAMPLE_SEQUENCE_NUMBER),
             SITE_LATITUDE_UTM=as.numeric(SITE_LATITUDE_UTM),
             SITE_LONGITUDE_UTM=as.numeric(SITE_LONGITUDE_UTM))

df <- unique(df)

stn <- select(df, SITE_SEQUENCE_NUMBER, TOWN, SITE_NAME, SAMPLE_POINT_NAME, SITE_LATITUDE_UTM, SITE_LONGITUDE_UTM) %>%
  unique

# duplicates, different lat/lon
group_by(stn, SAMPLE_POINT_NAME) %>%
  mutate(n=n()) %>%
  select(SITE_NAME, SAMPLE_POINT_NAME, SITE_LATITUDE_UTM, SITE_LONGITUDE_UTM, n) %>%
  arrange(SITE_NAME, SAMPLE_POINT_NAME) %>%
  filter(n > 1)

sort(unique(df$PARAMETER_NAME))


# stations ----------------------------------------------------------------

latlon <- list(stn$SITE_LONGITUDE_UTM, stn$SITE_LATITUDE_UTM)
latlon_wgs <- project(latlon, proj="+proj=utm +zone=19 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", inverse = TRUE)
stn$LATITUDE <- latlon_wgs$y
stn$LONGITUDE <- latlon_wgs$x
stn <- select(stn, SITE_SEQUENCE_NUMBER, TOWN, SITE_NAME, SAMPLE_POINT_NAME, LATITUDE, LONGITUDE)


# categorical variables ---------------------------------------------------

df_current_weather <- filter(df, grepl("CURRENT WEATHER", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("CURRENT WEATHER ", "", PARAMETER_NAME),
         PARAMETER_NAME = "CURRENT_WEATHER") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_past_weather_24 <- filter(df, grepl("PAST 24HR WEATHER", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("PAST 24HR WEATHER ", "", PARAMETER_NAME),
         PARAMETER_NAME = "PAST_WEATHER_24HR") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_past_weather_48 <- filter(df, grepl("PAST 48HR WEATHER", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("PAST 48HR WEATHER ", "", PARAMETER_NAME),
         PARAMETER_NAME = "PAST_WEATHER_48HR") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_sample_depth <- filter(df, grepl("SAMPLE DEPTH INTERVAL", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("SAMPLE DEPTH INTERVAL ", "", PARAMETER_NAME),
         PARAMETER_NAME = "SAMPLE_DEPTH_INTERVAL") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_tide <- filter(df, grepl("TIDE STAGE", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("TIDE STAGE: ", "", PARAMETER_NAME),
         PARAMETER_NAME = "TIDE") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_current <- filter(df,
                     grepl("WATER SURFACE", PARAMETER_NAME),
                     grepl("CURRENT$", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("WATER SURFACE ", "", PARAMETER_NAME),
         PARAMETER_NAME = "CURRENT") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)
df_water_surface <- filter(df,
                     grepl("WATER SURFACE", PARAMETER_NAME),
                     !grepl("CURRENT$", PARAMETER_NAME)) %>%
  mutate(VALUE = gsub("WATER SURFACE ", "", PARAMETER_NAME),
         PARAMETER_NAME = "WATER_SURFACE") %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)


df_categorical <- rbind(df_current_weather, df_past_weather_24, df_past_weather_48,
                        df_tide, df_current, df_water_surface) %>%
  spread(PARAMETER_NAME, VALUE)


# numeric variables -------------------------------------------------------

df_accumulation <- filter(df, grepl("ACCUMULATION", PARAMETER_NAME)) %>%
  mutate(PARAMETER_NAME = plyr::revalue(PARAMETER_NAME, c("ACCUMULATION LAST 48 HOURS"="ACCUMULATION_48HR",
                                                          "ACCUMULATION LAST 24 HOURS"="ACCUMULATION_24HR")),
         VALUE = as.numeric(CONCENTRATION)) %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)

df_air <- filter(df, SAMPLE_TYPE=="AIR") %>%
  mutate(PARAMETER_NAME = "TEMP_AIR",
         VALUE = as.numeric(CONCENTRATION)) %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)

df_water <- filter(df, SAMPLE_TYPE=="SURFACE WATER", PARAMETER_NAME %in% c("TEMPERATURE", "SALINITY (FROM SODIUM)")) %>%
  mutate(PARAMETER_NAME = plyr::revalue(PARAMETER_NAME, c("TEMPERATURE"="TEMP_WATER",
                                                          "SALINITY (FROM SODIUM)"="SALINITY")),
         VALUE = as.numeric(CONCENTRATION)) %>%
  select(SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)

df_ent <- filter(df, PARAMETER_NAME == "ENTEROCOCCI") %>%
  mutate(VALUE=as.numeric(CONCENTRATION))

idx <- which(df_ent$CONCENTRATION_QUALIFIER=="NOT DETECTED ABOVE THE ASSOCIATED QUANTITATION LIMIT")
df_ent[idx, "VALUE"] <- df_ent[idx, "QUANITATION_LIMIT"]

df_ent <- select(df_ent, SAMPLE_POINT_NAME, DEP_SAMPLE_ID, SAMPLE_DATE, PARAMETER_NAME, VALUE)

df_numeric <- rbind(df_accumulation, df_air, df_water, df_ent) %>%
  spread(PARAMETER_NAME, VALUE) %>%
  arrange(SAMPLE_POINT_NAME, SAMPLE_DATE)


# check datasets ----------------------------------------------------------

# which categorical rows are missing from numeric
anti_join(df_categorical,
          df_numeric,
          by=c("SAMPLE_POINT_NAME", "DEP_SAMPLE_ID", "SAMPLE_DATE"))

# number of samples by year and categorical variable
gather(df_categorical, PARAMETER_NAME, VALUE, -SAMPLE_POINT_NAME, -DEP_SAMPLE_ID, -SAMPLE_DATE, na.rm=TRUE) %>%
  mutate(YEAR=year(SAMPLE_DATE)) %>%
  group_by(PARAMETER_NAME, YEAR) %>%
  tally %>%
  ungroup %>%
  ggplot(aes(as.factor(YEAR), PARAMETER_NAME, fill=n)) +
  geom_tile()

# number of samples by year and numeric variable
gather(df_numeric, PARAMETER_NAME, VALUE, -SAMPLE_POINT_NAME, -DEP_SAMPLE_ID, -SAMPLE_DATE, na.rm=TRUE) %>%
  mutate(YEAR=year(SAMPLE_DATE)) %>%
  group_by(PARAMETER_NAME, YEAR) %>%
  tally %>%
  ungroup %>%
  ggplot(aes(as.factor(YEAR), PARAMETER_NAME, fill=n)) +
  geom_tile()


# merge variables ---------------------------------------------------------

wq <- full_join(df_categorical,
                df_numeric,
                by=c("SAMPLE_POINT_NAME", "DEP_SAMPLE_ID", "SAMPLE_DATE")) %>%
  mutate(id = row_number()) %>%
  mutate(YEAR=year(SAMPLE_DATE),
         WEEK=as.character(calweek(SAMPLE_DATE)),
         WEEKDAY=as.character(wday(SAMPLE_DATE))) %>%
  mutate(WEEK=ordered(WEEK, levels=as.character(seq(20, 46))),
         WEEKDAY=plyr::revalue(WEEKDAY, replace = c("1"="Sun", "2"="Mon", "3"="Tue", "4"="Wed", "5"="Thu", "6"="Fri", "7"="Sat"), warn_missing = FALSE),
         WEEKDAY=factor(WEEKDAY, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  filter(!is.na(ENTEROCOCCI))

# export ------------------------------------------------------------------

saveRDS(wq, file="data/rdata/wq-raw.rda")
saveRDS(stn, file="data/rdata/stn.rda")

write.csv(wq, file="data/csv/wq-raw.csv", row.names = FALSE)
write.csv(stn, file="data/csv/stn.csv", row.names = FALSE)
