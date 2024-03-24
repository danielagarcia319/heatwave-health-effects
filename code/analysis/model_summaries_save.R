
# Load libraries
library(lubridate)
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(gnm) 
library(glmmADMB)
library(tidyverse)

#----------------------------------------------------------------------------------------------------------------------- 
# 95th percentile data
#-----------------------------------------------------------------------------------------------------------------------

# Load data
load("heatwave-health-effects/data/final_dataset_95.Rdata")

modeling_data <- final_dataset_95 %>%
  mutate(dow = as.factor(wday(date)), # week start on monday
         year = as.factor(year),
         hw_ind_2d = as.factor(hw_ind_95_2d),
         fips = as.factor(fips),
         statecode = as.factor(statecode)) 
rm(final_dataset_95)

# Load all models of interest
load("heatwave-health-effects/data/models_95th_perct/glmernb_model_step1.Rdata")
load("heatwave-health-effects/data/models_95th_perct/gnm_model_step1.Rdata")
load("heatwave-health-effects/data/models_95th_perct/gnm_model_step5.Rdata")
load("heatwave-health-effects/data/models_95th_perct/gnm_model_step6.Rdata")
load("heatwave-health-effects/data/models_95th_perct/gnm_model_step3.Rdata")

# Generate model summaries
summary_glmernb_model_step1 <- summary(glmernb_model_step1)
summary_gnm_model_step1 <- summary(gnm_model_step1)
summary_gnm_model_step5 <- summary(gnm_model_step5)
summary_gnm_model_step6 <- summary(gnm_model_step6)
summary_gnm_model_step3 <- summary(gnm_model_step3)

# Save model summaries
save(summary_glmernb_model_step1, file = "heatwave-health-effects/data/models_95th_perct/summary_glmernb_model_step1.Rdata")
save(summary_gnm_model_step1, file = "heatwave-health-effects/data/models_95th_perct/summary_gnm_model_step1.Rdata")
save(summary_gnm_model_step5, file = "heatwave-health-effects/data/models_95th_perct/summary_gnm_model_step5.Rdata")
save(summary_gnm_model_step6, file = "heatwave-health-effects/data/models_95th_perct/summary_gnm_model_step6.Rdata")
save(summary_gnm_model_step3, file = "heatwave-health-effects/data/models_95th_perct/summary_gnm_model_step3.Rdata")

# Save data frames of model summary coefficients to csvs
write.csv(round(summary_glmernb_model_step1$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_glmernb_model_step1_95th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step1$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step1_95th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step5$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step5_95th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step6$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step6_95th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step3$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step3_95th_perct.csv", row.names = T)

#----------------------------------------------------------------------------------------------------------------------- 
# 97th percentile data
#-----------------------------------------------------------------------------------------------------------------------

# Load data
load("heatwave-health-effects/data/final_dataset_97.Rdata")

modeling_data <- final_dataset_97 %>%
  mutate(dow = as.factor(wday(date)), # week start on monday
         year = as.factor(year),
         hw_ind_2d = as.factor(hw_ind_97_2d),
         fips = as.factor(fips),
         statecode = as.factor(statecode))   
rm(final_dataset_97)

# Load all models of interest
load("heatwave-health-effects/data/models_97th_perct/glmernb_model_step1.Rdata")
load("heatwave-health-effects/data/models_97th_perct/gnm_model_step1.Rdata")
load("heatwave-health-effects/data/models_97th_perct/gnm_model_step5.Rdata")
load("heatwave-health-effects/data/models_97th_perct/gnm_model_step6.Rdata")
load("heatwave-health-effects/data/models_97th_perct/gnm_model_step3.Rdata")

# Generate model summaries
summary_glmernb_model_step1 <- summary(glmernb_model_step1)
summary_gnm_model_step1 <- summary(gnm_model_step1)
summary_gnm_model_step5 <- summary(gnm_model_step5)
summary_gnm_model_step6 <- summary(gnm_model_step6)
summary_gnm_model_step3 <- summary(gnm_model_step3)

# Save model summaries
save(summary_glmernb_model_step1, file = "heatwave-health-effects/data/models_97th_perct/summary_glmernb_model_step1.Rdata")
save(summary_gnm_model_step1, file = "heatwave-health-effects/data/models_97th_perct/summary_gnm_model_step1.Rdata")
save(summary_gnm_model_step5, file = "heatwave-health-effects/data/models_97th_perct/summary_gnm_model_step5.Rdata")
save(summary_gnm_model_step6, file = "heatwave-health-effects/data/models_97th_perct/summary_gnm_model_step6.Rdata")
save(summary_gnm_model_step3, file = "heatwave-health-effects/data/models_97th_perct/summary_gnm_model_step3.Rdata")

# Save data frames of model summary coefficients to csvs
write.csv(round(summary_glmernb_model_step1$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_glmernb_model_step1_97th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step1$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step1_97th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step5$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step5_97th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step6$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step6_97th_perct.csv", row.names = T)
write.csv(round(summary_gnm_model_step3$coefficients, 3), file = "heatwave-health-effects/results/tables/summary_gnm_model_step3_97th_perct.csv", row.names = T)


