
# source("create-all-trail-maps.R")
library(googlesheets4)

trail_info <- read_sheet("https://docs.google.com/spreadsheets/d/1rsNpzmcLIaBD1ftLnXl9Sqn1XERaSZir_rwLszIwrz0/edit#gid=0")
# trail_info <- trail_info %>% janitor::clean_names()
trail_info %>% write_csv("trail-info.csv")

rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')
