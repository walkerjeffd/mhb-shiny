source("init.R")


# load resample/other list ------------------------------------------------

wq <- readRDS("data/rdata/wq-raw.rda")

# these files were created using the find-resamples.R script
resamples <- read.csv(file="data/csv/samples-resample.csv", stringsAsFactors = FALSE) %>%
  rename(SAMPLE_POINT_NAME = stn) %>%
  mutate(RESAMPLE = TRUE)
others <- read.csv(file="data/csv/samples-other.csv", stringsAsFactors = FALSE) %>%
  rename(SAMPLE_POINT_NAME = stn) %>%
  mutate(OTHER = TRUE)

stopifnot(all(resamples$id %in% wq$id))
stopifnot(all(others$id %in% wq$id))

stopifnot(identical(wq[which(wq$id %in% resamples$id), ][["id"]], sort(resamples[["id"]])))
stopifnot(identical(wq[which(wq$id %in% resamples$id), ][["SAMPLE_POINT_NAME"]], resamples[order(resamples$id), ][["SAMPLE_POINT_NAME"]]))

stopifnot(identical(wq[which(wq$id %in% others$id), ][["id"]], sort(others[["id"]])))
stopifnot(identical(wq[which(wq$id %in% others$id), ][["SAMPLE_POINT_NAME"]], others[order(others$id), ][["SAMPLE_POINT_NAME"]]))

# assign flags to original dataset ----------------------------------------

wq <- wq %>%
  left_join(resamples, by = c("id", "SAMPLE_POINT_NAME")) %>%
  left_join(others, by = c("id", "SAMPLE_POINT_NAME")) %>%
  mutate(RESAMPLE = coalesce(RESAMPLE, FALSE),
         OTHER = coalesce(OTHER, FALSE))

stopifnot(identical(nrow(resamples), sum(wq$RESAMPLE)))
stopifnot(identical(nrow(others), sum(wq$OTHER)))

# export ------------------------------------------------------------------

write.csv(wq, file="data/csv/wq-resamples.csv", row.names=FALSE)
saveRDS(wq, file="data/rdata/wq-resamples.rda")

