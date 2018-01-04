library(tidyverse)
library(rvest)


# Read in data
url <- "http://aquatext.com/tables/algaegrwth.htm"

# Discard the header and the footer rows
algae_tbl <- read_html(url) %>% 
  html_table(fill = TRUE) %>% 
  discard(~nrow(.x) <= 6) %>% 
  flatten_df()

