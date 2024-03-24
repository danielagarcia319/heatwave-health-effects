
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
plot_heatwaves <- final_dataset %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>%
  summarize(total_hw_events = sum(hw_ind_95_2d_day1),
            total_hw_days = sum(hw_ind_95_2d)) %>%
  left_join(county_geometries, by = c("fips")) %>%
  st_sf(., sf_column_name = "geometry")

# get quantile breaks
breaks_events <- classIntervals(plot_heatwaves$total_hw_events, n = 5, style = "quantile")
breaks_days <- classIntervals(plot_heatwaves$total_hw_days, n = 5, style = "quantile")

# put counts into buckets
plot_heatwaves$hw_event_cat <- cut(plot_heatwaves$total_hw_events, breaks_events$brks, include.lowest = TRUE)
plot_heatwaves$hw_day_cat <- cut(plot_heatwaves$total_hw_days, breaks_days$brks, include.lowest = TRUE)

#### generate and save cumulative plots across all years

# heatwave events
jpeg(paste0("heatwave-health-effects/figures/maps/hw_events_fips_all_years_", perct, "_perct.jpg"),
     width = 1800, height = 1000, quality = 100) 

plot_heatwaves %>%
  filter(!is.na(hw_event_cat)) %>%
  ggplot(aes(fill = hw_event_cat)) +  
  geom_sf(lwd = 0.5) +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill = "Total HW Events") +
  theme_light() + theme(text = element_text(size = 40, family = "Times New Roman"))         

dev.off() 

# heatwave days
jpeg(paste0("heatwave-health-effects/figures/maps/hw_days_fips_all_years_", perct, "_perct.jpg"),
     width = 1800, height = 1000, quality = 100)

plot_heatwaves %>%
  filter(!is.na(hw_day_cat)) %>%
  ggplot(aes(fill = hw_day_cat)) + 
  geom_sf(lwd = 0.5) +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill = "Total HW Days") +
  theme_light() + theme(text = element_text(size = 40, family = "Times New Roman"))         

dev.off()
