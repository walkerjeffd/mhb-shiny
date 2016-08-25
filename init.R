# load packages -----------------------------------------------------------

packages <- c("readxl", "dplyr", "tidyr", "lubridate", "stringr", "ggplot2",
              "yaml", "proj4", "shiny")

result <- sapply(packages, require, character.only = TRUE)

if (!all(as.vector(result))) {
  stop("Missing packages, check init.R")
}


# load configuration ------------------------------------------------------

config <- yaml.load_file("config.yaml")


# ggplot configuration ----------------------------------------------------

theme_set(theme_bw())
theme_update(strip.background = element_blank(),
             strip.text = element_text(face = "bold"))
