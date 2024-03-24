
# Load libraries
library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(classInt)
library(gridExtra)

# Determine which percentile to use
perct <- 95

# Load dataset
if (perct == 95) {
  load("heatwave-health-effects/data/final_dataset_95.Rdata")
  final_dataset <- final_dataset_95
  rm(final_dataset_95)
  load("heatwave-health-effects/data/models_95th_perct/gnm_model_step6.Rdata")
  load("heatwave-health-effects/data/models_95th_perct/gnm_model_step3.Rdata")
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
  final_dataset <- final_dataset_97
  rm(final_dataset_97)
  load("heatwave-health-effects/data/models_97th_perct/gnm_model_step6.Rdata")
  load("heatwave-health-effects/data/models_97th_perct/gnm_model_step3.Rdata")
}
 
# Save US states
US_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
               "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", 
               "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
               "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# Load county geometries
state_geometries <- states(year = 2016, cb = F) %>% 
  select(STUSPS, geometry) %>%
  rename(statecode = STUSPS)

#----------------------------------------------------------------------------------------------------
# GNM model with just state variables (step 6)
#----------------------------------------------------------------------------------------------------

# Clean coefficient data
gnm_model_step6_coef <- data.frame(coef_name = names(gnm_model_step6$coefficients),
                                   coef_val = gnm_model_step6$coefficients) 
# reset row names
row.names(gnm_model_step6_coef) <- 1:nrow(gnm_model_step6_coef) 


# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step6 <- gnm_model_step6_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val

# Extract coefficients for statecode/hw interaction terms
statecode_interact_coefs_step6 <- gnm_model_step6_coef %>%
  filter(grepl("hw_ind_2d1:statecode", coef_name) == TRUE) %>%
  mutate(statecode = str_remove(coef_name, "hw_ind_2d1:statecode")) %>%
  mutate(final_coef = coef_val + hw_ind_2d_coef_step6) %>%
  select(statecode, final_coef, interaction_val = coef_val)

# Add reference category (Alabama)
ref_state_step6 <- setdiff(US_states, statecode_interact_coefs_step6$statecode)

statecode_interact_coefs_step6 <- rbind(statecode_interact_coefs_step6, 
                                  data.frame(statecode = ref_state_step6, 
                                             final_coef = hw_ind_2d_coef_step6,
                                             interaction_val = 0))

# generate dataset for plotting
plot_state_coefs_step6 <- statecode_interact_coefs_step6 %>%
  mutate(final_coef_exp = exp(final_coef)) %>%
  left_join(state_geometries, by = c("statecode")) %>%
  st_sf(., sf_column_name = "geometry")

# get quantile breaks
breaks_coefs_step6 <- classIntervals(plot_state_coefs_step6$final_coef_exp, n = 7, style = "quantile")

# put counts into buckets
plot_state_coefs_step6$coef_cat <- cut(plot_state_coefs_step6$final_coef_exp, breaks_coefs_step6$brks, include.lowest = TRUE)

# generate plot
plot_step6 <- plot_state_coefs_step6 %>%
  ggplot(aes(fill = coef_cat)) +  
  geom_sf(lwd = 0.5) +
  labs(fill = "Relative Risk", title = "Model (3)") +
  theme_light() + 
  theme(text = element_text(size = 40, family = "Times New Roman")) +    
  scale_fill_brewer(palette = "OrRd") 


#----------------------------------------------------------------------------------------------------
# GNM model with all variables (step 3)
#----------------------------------------------------------------------------------------------------

# Clean coefficient data
gnm_model_step3_coef <- data.frame(coef_name = names(gnm_model_step3$coefficients),
                                   coef_val = gnm_model_step3$coefficients) 
# reset row names
row.names(gnm_model_step3_coef) <- 1:nrow(gnm_model_step3_coef) 


# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step3 <- gnm_model_step3_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val

# Extract coefficients for statecode/hw interaction terms
statecode_interact_coefs_step3 <- gnm_model_step3_coef %>%
  filter(grepl("hw_ind_2d1:statecode", coef_name) == TRUE) %>%
  mutate(statecode = str_remove(coef_name, "hw_ind_2d1:statecode")) %>%
  mutate(final_coef = coef_val + hw_ind_2d_coef_step3) %>%
  select(statecode, final_coef, interaction_val = coef_val)

# Add reference category (Alabama)
ref_state_step3 <- setdiff(US_states, statecode_interact_coefs_step3$statecode)

statecode_interact_coefs_step3 <- rbind(statecode_interact_coefs_step3, 
                                        data.frame(statecode = ref_state_step3, 
                                                   final_coef = hw_ind_2d_coef_step3,
                                                   interaction_val = 0))

# generate dataset for plotting
plot_state_coefs_step3 <- statecode_interact_coefs_step3 %>%
  mutate(final_coef_exp = exp(final_coef)) %>%
  left_join(state_geometries, by = c("statecode")) %>%
  st_sf(., sf_column_name = "geometry")

# get quantile breaks
breaks_coefs_step3 <- classIntervals(plot_state_coefs_step3$final_coef_exp, n = 7, style = "quantile")

# put counts into buckets
plot_state_coefs_step3$coef_cat <- cut(plot_state_coefs_step3$final_coef_exp, breaks_coefs_step3$brks, include.lowest = TRUE)

# generate plot
plot_step3 <- plot_state_coefs_step3 %>%
  ggplot(aes(fill = coef_cat)) +  
  geom_sf(lwd = 0.5) +
  labs(fill = "Relative Risk", title = "Model (4)") +
  theme_light() + 
  theme(text = element_text(size = 40, family = "Times New Roman")) +    
  scale_fill_brewer(palette = "OrRd") 

#----------------------------------------------------------------------------------------------------
# Save both plots
#----------------------------------------------------------------------------------------------------

jpeg(paste0("heatwave-health-effects/data/figures/maps/gnm_coef_map_steps3and6_", 
            perct, "_perct.jpg"),
     width = 1800, height = 2000, quality = 100) 

grid.arrange(plot_step6, plot_step3, ncol = 1, nrow = 2)

dev.off() 

