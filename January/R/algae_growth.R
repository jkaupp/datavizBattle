library(tidyverse)
library(rvest)
library(ggridges)
library(jkmisc)
library(showtext)

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

frost <-c("#8FBCBB", "#88C0D0", "#81A1C1", "#5E81AC")

plot <- ggplot(tidy_algae, aes(x = temperature, y = factor(light_intensity), group = light_intensity, fill = factor(light_intensity))) +
  geom_ridgeline(aes(height = divisions), min_height = -0.8, color = "grey30", scale = 0.8, size = 0.2) +
  facet_wrap(~species, nrow = 4) +
  scale_x_continuous(breaks = seq(5, 30, 5)) +
  scale_fill_manual("Light Intensity", values = frost[c(4,1)], labels = c("2500 lux", "5000 lux")) +
  theme_minimal() +
  labs(x = "Temperature (°C)",
       y = NULL,
       title = "Specific growth rates of algae at different light intensities and temperatures",
       subtitle = "Ridgline plot of growth rate (divisions per day) vs temperature (°C). Light intensity (lux) indicated by color") +
  theme(legend.position = c(0.9, 0.1),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid = element_line(color = "grey95", size = 0.1))

ggsave(plot = plot, "algae growth.pdf", width = 10, height = 5)
