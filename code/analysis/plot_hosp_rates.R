
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
plot_hosp_count <- final_dataset %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>%
  summarize(r_hosp_p100k = (sum(n_hosp_fluid_electrolyte) * 100000) / max(medicare_pop)) %>%
  left_join(county_geometries, by = c("fips")) %>%
  st_sf(., sf_column_name = "geometry")

# get quantile breaks
breaks_hosp <- classIntervals(plot_hosp_count$r_hosp_p100k, n = 5, style = "quantile")

# put counts into buckets
plot_hosp_count$hosp_cat <- cut(plot_hosp_count$r_hosp_p100k, breaks_hosp$brks, include.lowest = TRUE, dig.lab = 6)

# Plot
jpeg(paste0("heatwave-health-effects/figures/maps/hosp_rate_map_all_years_", perct,"_perct.jpg"),
     width = 1800, height = 1000, quality = 100) 

plot_hosp_count %>%
  filter(!is.na(hosp_cat)) %>%
  ggplot(aes(fill = hosp_cat)) +  
  geom_sf(lwd = 0.5) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(fill = "Hosp. Rate p100k") +
  theme_light() + theme(text = element_text(size = 40, family = "Times New Roman"), legend.position = "bottom")         

dev.off() 
