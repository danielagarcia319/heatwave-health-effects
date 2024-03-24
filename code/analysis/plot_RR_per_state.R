
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

modeling_data <- final_dataset %>%
  mutate(dow = as.factor(wday(date)), 
         year = as.factor(year),
         hw_ind_2d = as.factor(hw_ind_95_2d),
         fips = as.factor(fips),
         statecode = as.factor(statecode)) 

#----------------------------------------------------------------------------------------------------
# GNM model with just state variables (step 6)
#----------------------------------------------------------------------------------------------------

# Get standard errors of coefficients (without using summary function)
se_gnm_model_step6 <- as.vector(sqrt(diag(vcov(gnm_model_step6))))

# Clean coefficient data
gnm_model_step6_coef <- data.frame(coef_name = names(gnm_model_step6$coefficients),
                                   coef_val = gnm_model_step6$coefficients)

gnm_model_step6_coef$ci_lower <- gnm_model_step6$coefficients - (1.96 * se_gnm_model_step6)
gnm_model_step6_coef$ci_upper <- gnm_model_step6$coefficients + (1.96 * se_gnm_model_step6)

# reset row names
row.names(gnm_model_step6_coef) <- 1:nrow(gnm_model_step6_coef) 

# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step6 <- gnm_model_step6_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val
hw_ind_2d_ci_lower_step6 <- gnm_model_step6_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$ci_lower
hw_ind_2d_ci_upper_step6 <- gnm_model_step6_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$ci_upper

# Extract coefficients for statecode/hw interaction terms
statecode_interact_coefs_step6 <- gnm_model_step6_coef %>%
  filter(grepl("hw_ind_2d1:statecode", coef_name) == TRUE) %>%
  mutate(statecode = str_remove(coef_name, "hw_ind_2d1:statecode")) %>%
  mutate(final_coef = coef_val + hw_ind_2d_coef_step6,
         final_ci_lower = ci_lower + hw_ind_2d_ci_lower_step6,
         final_ci_upper = ci_upper + hw_ind_2d_ci_upper_step6) %>%
  select(statecode, final_coef, interaction_val = coef_val, final_ci_lower, final_ci_upper)

# Add reference category (Alabama)
ref_state_step6 <- setdiff(US_states, statecode_interact_coefs_step6$statecode)

statecode_interact_coefs_step6 <- rbind(statecode_interact_coefs_step6, 
                                        data.frame(statecode = ref_state_step6, 
                                                   final_coef = hw_ind_2d_coef_step6,
                                                   interaction_val = 0,
                                                   final_ci_lower = 0,
                                                   final_ci_upper = 0))

# generate dataset for plotting
plot_state_coefs_step6 <- statecode_interact_coefs_step6 %>%
  mutate(final_coef_exp = exp(final_coef),
         final_ci_lower_exp = exp(final_ci_lower),
         final_ci_upper_exp = exp(final_ci_upper)) %>%
  arrange(desc(final_coef_exp))

# get quantile breaks
breaks_coefs_step6 <- classIntervals(plot_state_coefs_step6$final_coef_exp, n = 7, style = "quantile")

# put counts into buckets
plot_state_coefs_step6$coef_cat <- cut(plot_state_coefs_step6$final_coef_exp, breaks_coefs_step6$brks, include.lowest = TRUE)

# generate plot
plot_step6 <- #plot_state_coefs_step6 %>%
  rbind(head(plot_state_coefs_step6, 10), 
        tail(plot_state_coefs_step6, 10)) %>%
  ggplot(aes(reorder(statecode, final_coef_exp), final_coef_exp, fill = coef_cat)) +
  geom_bar(stat = "identity", show.legend = F, color = "black") +
  labs(x = "", y = "Relative Risk", title = "Model (3)") +
  theme_light() +
  theme(text = element_text(size = 40, family = "Times New Roman")) +
  scale_fill_brewer(palette = "OrRd") +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 1, color = "black") + 
  geom_errorbar(aes(x = statecode, ymin = final_ci_lower_exp, ymax = final_ci_upper_exp), width = 0.5, size = 1)


#----------------------------------------------------------------------------------------------------
# GNM model with all variables (step 3)
#----------------------------------------------------------------------------------------------------

# Get standard errors of coefficients (without using summary function)
se_gnm_model_step3 <- as.vector(sqrt(diag(vcov(gnm_model_step3))))

# Clean coefficient data
gnm_model_step3_coef <- data.frame(coef_name = names(gnm_model_step3$coefficients),
                                   coef_val = gnm_model_step3$coefficients)

gnm_model_step3_coef$ci_lower <- gnm_model_step3$coefficients - (1.96 * se_gnm_model_step3)
gnm_model_step3_coef$ci_upper <- gnm_model_step3$coefficients + (1.96 * se_gnm_model_step3)

# reset row names
row.names(gnm_model_step3_coef) <- 1:nrow(gnm_model_step3_coef) 

# Extract coefficient for heatwave treatment
hw_ind_2d_coef_step3 <- gnm_model_step3_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$coef_val
hw_ind_2d_ci_lower_step3 <- gnm_model_step3_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$ci_lower
hw_ind_2d_ci_upper_step3 <- gnm_model_step3_coef %>% filter(coef_name == "hw_ind_2d1") %>% .$ci_upper

# Extract coefficients for statecode/hw interaction terms
statecode_interact_coefs_step3 <- gnm_model_step3_coef %>%
  filter(grepl("hw_ind_2d1:statecode", coef_name) == TRUE) %>%
  mutate(statecode = str_remove(coef_name, "hw_ind_2d1:statecode")) %>%
  mutate(final_coef = coef_val + hw_ind_2d_coef_step3,
         final_ci_lower = ci_lower + hw_ind_2d_ci_lower_step3,
         final_ci_upper = ci_upper + hw_ind_2d_ci_upper_step3) %>%
  select(statecode, final_coef, interaction_val = coef_val, final_ci_lower, final_ci_upper)

# Add reference category (Alabama)
ref_state_step3 <- setdiff(US_states, statecode_interact_coefs_step3$statecode)

statecode_interact_coefs_step3 <- rbind(statecode_interact_coefs_step3, 
                                        data.frame(statecode = ref_state_step3, 
                                                   final_coef = hw_ind_2d_coef_step3,
                                                   interaction_val = 0,
                                                   final_ci_lower = 0,
                                                   final_ci_upper = 0))

# generate dataset for plotting
plot_state_coefs_step3 <- statecode_interact_coefs_step3 %>%
  mutate(final_coef_exp = exp(final_coef),
         final_ci_lower_exp = exp(final_ci_lower),
         final_ci_upper_exp = exp(final_ci_upper)) %>%
  arrange(desc(final_coef_exp))

# get quantile breaks
breaks_coefs_step3 <- classIntervals(plot_state_coefs_step3$final_coef_exp, n = 7, style = "quantile")

# put counts into buckets
plot_state_coefs_step3$coef_cat <- cut(plot_state_coefs_step3$final_coef_exp, breaks_coefs_step3$brks, include.lowest = TRUE)

# generate plot
plot_step3 <- #plot_state_coefs_step3 %>%
  rbind(head(plot_state_coefs_step3, 10), 
        tail(plot_state_coefs_step3, 10)) %>%
  ggplot(aes(reorder(statecode, final_coef_exp), final_coef_exp, fill = coef_cat)) +
  geom_bar(stat = "identity", show.legend = F, color = "black") +
  labs(x = "", y = "Relative Risk", title = "Model (4)") +
  theme_light() +
  theme(text = element_text(size = 40, family = "Times New Roman")) +
  scale_fill_brewer(palette = "OrRd") +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 1, color = "black") + 
  geom_errorbar(aes(x = statecode, ymin = final_ci_lower_exp, ymax = final_ci_upper_exp), width = 0.5, size = 1)

#----------------------------------------------------------------------------------------------------
# Save both plots
#----------------------------------------------------------------------------------------------------

jpeg(paste0("heatwave-health-effects/figures/maps/RR_state_barplots_", 
            perct, "_perct.jpg"),
     width = 1800, height = 2000, quality = 100) 

grid.arrange(plot_step6, plot_step3, ncol = 1, nrow = 2)

dev.off() 
