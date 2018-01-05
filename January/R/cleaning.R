library(tidyverse)
library(rvest)
library(ggridges)
library(jkmisc)


# Read in data
url <- "http://aquatext.com/tables/algaegrwth.htm"

# Discard the header and the footer rows
# Remove line returns and convert extra spaces into single spaces.
algae_tbl <- read_html(url) %>% 
  html_table(fill = TRUE) %>% 
  discard(~nrow(.x) <= 6) %>% 
  flatten_df() %>% 
  mutate_all(funs(gsub("\n","", .))) %>% 
  mutate_all(funs(gsub("\\s+", " ", .))) %>% 
  mutate_all(funs(gsub("\\.+", ".", .)))


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
  mutate_at(c("temperature","light_intensity", "divisions"), as.numeric) %>% 
  group_by(species, temperature) %>% 
  mutate(color = ifelse(divisions < 0, 1, 0)) %>% 
  arrange(species, temperature, light_intensity) 


ggplot(tidy_algae, aes(x = temperature, y = species, group = species)) +
  geom_ridgeline(aes(height = divisions, fill = light_intensity), min_height = -0.8, scale = 0.6, size = 0.2, color = "grey20") +
  facet_wrap(~light_intensity, nrow = 1) +
  scale_x_continuous(breaks = seq(5, 30, 5)) +
  theme_jk()
