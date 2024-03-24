
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
  load("heatwave-health-effects/data/models_95th_perct/gnm_model_step6.Rdata")
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
  final_dataset <- final_dataset_97
  rm(final_dataset_97)
  load("heatwave-health-effects/data/models_97th_perct/gnm_model_step6.Rdata")
}

# Save US states
US_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
               "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", 
               "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
               "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# Clean coefficient data
gnm_model_step6_coef <- data.frame(coef_name = names(gnm_model_step6$coefficients),
                                   coef_val = gnm_model_step6$coefficients) 
# reset row names
row.names(gnm_model_step6_coef) <- 1:nrow(gnm_model_step6_coef) 


# Extract coefficient for heatwave treatment
hw_ind_2d_coef <- gnm_model_step6_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val

# Extract coefficients for statecode/hw interaction terms
statecode_interact_coefs <- gnm_model_step6_coef %>%
  filter(grepl("hw_ind_2d1:statecode", coef_name) == TRUE) %>%
  mutate(statecode = str_remove(coef_name, "hw_ind_2d1:statecode")) %>%
  mutate(final_coef = coef_val + hw_ind_2d_coef) %>%
  select(statecode, final_coef, interaction_val = coef_val)

# Add reference category (Alabama)
ref_state <- setdiff(US_states, statecode_interact_coefs$statecode)
statecode_interact_coefs <- rbind(statecode_interact_coefs, 
                                  data.frame(statecode = ref_state, 
                                             final_coef = hw_ind_2d_coef,
                                             interaction_val = 0))

# Load county geometries
state_geometries <- states(year = 2016, cb = F) %>% 
  select(STUSPS, geometry) %>%
  rename(statecode = STUSPS)

# Extract AC data
ac_data <- final_dataset %>%
  select(statecode, ac_prob_wt_state) %>%
  unique()

# generate dataset for plotting
plot_ac_data <- statecode_interact_coefs %>%
  mutate(final_coef_exp = exp(final_coef)) %>%
  left_join(state_geometries, by = c("statecode")) %>%
  left_join(ac_data, by = c("statecode")) %>%
  st_sf(., sf_column_name = "geometry")



####### Scatterplot ####### 

jpeg(paste0("heatwave-health-effects/figures/scatterplots/ac_scatterplot_", perct, "_perct.jpg"),
     width = 1800, height = 1000, quality = 100) 

plot_ac_data %>%
  ggplot(aes(x = final_coef_exp, y = ac_prob_wt_state)) +
  geom_point(aes(color = statecode), size = 9, alpha = 0.7) + 
  geom_smooth(method = 'lm', formula = y~x, alpha = 0.15, linewidth = 1, color = "black", linetype = 2) +
  labs(x = "Relative Risk", y = "Proportion of AC")  +
  theme_light() + 
  xlim(min(plot_ac_data %>% filter(!is.na(ac_prob_wt_state)) %>% .$final_coef_exp) - 0.01, 
       max(plot_ac_data %>% filter(!is.na(ac_prob_wt_state)) %>% .$final_coef_exp) + 0.01) + 
  theme(text = element_text(size = 40, family = "Times New Roman"), legend.position = "none") 
  
dev.off() 

####### Heatmap ####### 

# get quantile breaks
breaks_ac <- classIntervals(plot_ac_data$ac_prob_wt_state, n = 7, style = "quantile")

# put counts into buckets
plot_ac_data$ac_cat <- cut(plot_ac_data$ac_prob_wt_state, seq(0.5, 1, 0.05), include.lowest = TRUE)


# plot data
jpeg("heatwave-health-effects/figures/maps/ac_map.jpg",
     width = 1800, height = 1000, quality = 100) 

plot_ac_data %>%
  ggplot(aes(fill = ac_cat)) +  
  geom_sf(lwd = 0.5) +
  theme_light() + 
  labs(fill = "AC Prop.") + 
  theme(text = element_text(size = 40, family = "Times New Roman")) +    
  scale_fill_brewer(palette = "Spectral")

dev.off() 
 
