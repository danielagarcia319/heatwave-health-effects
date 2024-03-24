
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
  load("heatwave-health-effects/data/models_95th_perct/gnm_model_step5.Rdata")
  load("heatwave-health-effects/data/models_95th_perct/gnm_model_step3.Rdata")
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
  load("heatwave-health-effects/data/models_97th_perct/gnm_model_step5.Rdata")
  load("heatwave-health-effects/data/models_97th_perct/gnm_model_step3.Rdata")
}

# --------------------------------------------------------------------------------------------
# Model with just year interaction (step 5)
# --------------------------------------------------------------------------------------------

# Clean coefficient data
gnm_model_step5_coef <- data.frame(coef_name = names(gnm_model_step5$coefficients),
                                   coef_val = gnm_model_step5$coefficients) 
# reset row names
row.names(gnm_model_step5_coef) <- 1:nrow(gnm_model_step5_coef) 


# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step5 <- gnm_model_step5_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val

# Extract coefficients for statecode/hw interaction terms
year_coefs_step5 <- gnm_model_step5_coef %>%
  filter(grepl("year", coef_name) == TRUE) %>%
  mutate(Coefficient = ifelse(grepl(":year", coef_name), "interaction", "main_effect")) %>%
  mutate(year = case_when(Coefficient == "main_effect" ~ str_remove(coef_name, "year"),
                          Coefficient == "interaction" ~ str_remove(coef_name, "hw_ind_2d1:year"))) %>%
  mutate(year = as.integer(year),
         coef_val = ifelse(Coefficient == "main_effect", coef_val + hw_ind_2d_coef_step5, coef_val)) %>%
  select(year, Coefficient, coef_val) %>%
  spread(Coefficient, coef_val) %>%
  rbind(data.frame(year = 2000, 
                   main_effect = hw_ind_2d_coef_step5,
                   interaction = 0)) %>%
  arrange(year) %>%
  mutate(total_effect = main_effect + interaction) %>%
  gather("Coefficient", "value", c("main_effect", "interaction", "total_effect")) %>%
  mutate(Coefficient = case_when(Coefficient == "main_effect" ~ "main effect",
                          Coefficient == "total_effect" ~ "main effect + interaction",
                          Coefficient == "interaction" ~ "interaction"))

# generate plot
coef_plot_step5 <- year_coefs_step5 %>%
  mutate(Coefficient = factor(Coefficient, levels = c("main effect + interaction", "main effect", "interaction"))) %>%
  ggplot(aes(x = year, y = exp(value), color = Coefficient, linetype = Coefficient)) +  
  geom_hline(yintercept = 1, color = "darkgray", size = 1.5) +
  geom_line(linewidth = 3) +
  theme_light() +
  theme(text = element_text(size = 40, family = "Times New Roman"),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) +    
  labs(y = "Relative Risk", title = "Model (2)", x = "") +
  scale_x_continuous(labels = as.character(year_coefs_step3$year), breaks = year_coefs_step3$year)

# --------------------------------------------------------------------------------------------
# Model with just year interaction (step 3)
# --------------------------------------------------------------------------------------------

# Clean coefficient data
gnm_model_step3_coef <- data.frame(coef_name = names(gnm_model_step3$coefficients),
                                   coef_val = gnm_model_step3$coefficients) 
# reset row names
row.names(gnm_model_step3_coef) <- 1:nrow(gnm_model_step3_coef) 


# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step3 <- gnm_model_step3_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val

# Extract coefficients for statecode/hw interaction terms
year_coefs_step3 <- gnm_model_step3_coef %>%
  filter(grepl("year", coef_name) == TRUE) %>%
  mutate(Coefficient = ifelse(grepl(":year", coef_name), "interaction", "main_effect")) %>%
  mutate(year = case_when(Coefficient == "main_effect" ~ str_remove(coef_name, "year"),
                          Coefficient == "interaction" ~ str_remove(coef_name, "hw_ind_2d1:year"))) %>%
  mutate(year = as.integer(year),
         coef_val = ifelse(Coefficient == "main_effect", coef_val + hw_ind_2d_coef_step3, coef_val)) %>%
  select(year, Coefficient, coef_val) %>%
  spread(Coefficient, coef_val) %>%
  rbind(data.frame(year = 2000, 
                   main_effect = hw_ind_2d_coef_step3,
                   interaction = 0)) %>%
  arrange(year) %>%
  mutate(total_effect = main_effect + interaction) %>%
  gather("Coefficient", "value", c("main_effect", "interaction", "total_effect")) %>%
  mutate(Coefficient = case_when(Coefficient == "main_effect" ~ "main effect",
                          Coefficient == "total_effect" ~ "main effect + interaction",
                          Coefficient == "interaction" ~ "interaction"))

# generate plot
coef_plot_step3 <- year_coefs_step3 %>%
  mutate(Coefficient = factor(Coefficient, levels = c("main effect + interaction", "main effect", "interaction"))) %>%
  ggplot(aes(x = year, y = exp(value), color = Coefficient, linetype = Coefficient)) + 
  geom_hline(yintercept = 1, color = "darkgrey", size = 1.5) +
  geom_line(linewidth = 3) +
  theme_light() +
  theme(text = element_text(size = 40, family = "Times New Roman"),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) +    
  labs(y = "Relative Risk", title = "Model (4)", x = "") +
  scale_x_continuous(labels = as.character(year_coefs_step3$year), breaks = year_coefs_step3$year)

# --------------------------------------------------------------------------------------------
# Plot both graphs
# --------------------------------------------------------------------------------------------

jpeg(paste0("heatwave-health-effects/figures/scatterplots/gnm_year_timeseries_steps3and5_", 
            perct, "_perct.jpg"),
     width = 1800, height = 2000, quality = 100) 

grid.arrange(coef_plot_step5, coef_plot_step3, ncol = 1, nrow = 2)

dev.off() 
