# Maine Health Beaches Data Analysis and Web Application
# Jeffrey D Walker PhD, Walker Environmental Research LLC


# load data ---------------------------------------------------------------

source("load-data.R") # exports data/rdata/wq-raw.rda and data/rdata/stn.rda


# clean data --------------------------------------------------------------

# manually select resample/other samples using shiny gadget and save [id, station_id] to csv
# ONLY RUN THIS SCRIPT INTERACTIVELY (LINE-BY-LINE) TO ASSIGN RESAMPLES
# source("clean-1-find-resample.R")
source("clean-2-assign-resample.R") # exports data/rdata/wq-resamples.rda and data/csv/wq-resamples.csv


# calendar plots ----------------------------------------------------------

source("plot-calendar.R")


# analyze data ------------------------------------------------------------

source("analyze-resamples.R")


# powerpoint --------------------------------------------------------------

source("ppt-mwsc-2016.R")


# shiny -------------------------------------------------------------------

# run shiny/app.R


# analysis report ---------------------------------------------------------

# compile rmd/mhb-data-summary.Rmd
