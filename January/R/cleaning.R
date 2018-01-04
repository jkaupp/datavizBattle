library(tidyverse)
library(rvest)


# Read in data
url <- "http://aquatext.com/tables/algaegrwth.htm"

# Discard the header and the footer rows
# Remove line returns
algae_tbl <- read_html(url) %>% 
  html_table(fill = TRUE) %>% 
  discard(~nrow(.x) <= 6) %>% 
  flatten_df() %>% 
  mutate_all(funs(gsub("\n","", .))) %>% 
  mutate_all(funs(gsub("\\s+", " ", .)))


tbl_headers <- algae_tbl %>% 
  slice(1:2) %>% 
  select(-1) %>% 
  summarize_all(funs(paste0(., collapse= "_"))) %>% 
  flatten_chr() %>% 
  c("species",.)

tidy_algae <- algae_tbl %>% 
  set_names(tbl_headers) %>% 
  slice(-1:-3) %>% 
  gather(var, divisions, -species) %>% 
  separate(var,c("temperature", "light_intensity"), sep = "_") %>% 
  arrange(species)
