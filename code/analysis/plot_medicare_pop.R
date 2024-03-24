
# Load libraries
library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(classInt)

# Determine which percentile to use
perct <- 95

# Load dataset
if (perct == 95) {
  load("heatwave-health-effects/data/final_dataset_95.Rdata")
  final_dataset <- final_dataset_95
  rm(final_dataset_95)
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
  final_dataset <- final_dataset_97
  rm(final_dataset_97)
}

# Load county geometries
county_geometries <- counties(year = 2016, cb = F) %>% 
  select(GEOID, geometry) %>%
  rename(fips = GEOID) %>% 
  mutate(fips = as.integer(fips))

# generate dataset for plotting
plot_medicare_pop <- final_dataset %>%
  filter(!is.na(fips)) %>%
  filter(year == 2016) %>%
  select(fips, medicare_pop) %>%
  unique() %>%
  left_join(county_geometries, by = c("fips")) %>%
  st_sf(., sf_column_name = "geometry")

# get quantile breaks
breaks_pop <- classIntervals(plot_medicare_pop$medicare_pop, n = 5, style = "quantile", include.lowest = TRUE)

# put counts into buckets
plot_medicare_pop$pop_cat <- cut(plot_medicare_pop$medicare_pop, breaks_pop$brks, dig.lab = 6)

# Plot
jpeg("heatwave-health-effects/figures/maps/medicare_pop_2016.jpg",
     width = 1800, height = 1000, quality = 100) 

plot_medicare_pop %>%
  filter(!is.na(pop_cat)) %>%
  ggplot(aes(fill = pop_cat)) +  
  geom_sf(lwd = 0.5) +
  scale_fill_brewer(palette = "PuBu") +
  labs(fill = "Medicare Pop.") +
  theme_light() + theme(text = element_text(size = 40, family = "Times New Roman"), legend.position = "bottom")         

dev.off() 
