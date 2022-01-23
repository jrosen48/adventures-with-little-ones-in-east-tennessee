# source("create-all-trail-maps.R")
library(googlesheets4)
library(tidyverse)

trail_info <- read_sheet("https://docs.google.com/spreadsheets/d/1rsNpzmcLIaBD1ftLnXl9Sqn1XERaSZir_rwLszIwrz0/edit#gid=0",
                         col_types = "ccccddddcccccccccc")
trail_info %>% write_csv("trail-info.csv")

sci_info <- read_sheet("https://docs.google.com/spreadsheets/d/1rsNpzmcLIaBD1ftLnXl9Sqn1XERaSZir_rwLszIwrz0/edit#gid=0", sheet = 2)
trail_info %>% write_csv("sci-info.csv")

rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')
