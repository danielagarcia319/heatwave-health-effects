# Xiao's Model: generalized linear mixed-effects model (GLMM) for the negative binomial family 
# https://github.com/wxwx1993/PM_COVID/blob/updated_data/Analyses.R

# Load libraries
library(lubridate)
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(gnm) 
library(glmmADMB)
library(tidyverse)

# Determine percentile to run
perct <- 95

# Load dataset
if (perct == 95) {
  load("heatwave-health-effects/data/final_dataset_95.Rdata")
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
}

#----------------------------------------------------------------------------------------------------------------------- 
# Prepare data for modeling
#-----------------------------------------------------------------------------------------------------------------------

if (perct == 95) { 
  modeling_data <- final_dataset_95 %>%
    mutate(dow = as.factor(wday(date)), # week start on monday
           year = as.factor(year),
           hw_ind_2d = as.factor(hw_ind_95_2d),
           fips = as.factor(fips),
           statecode = as.factor(statecode)) 
  rm(final_dataset_95)
}

if (perct == 97) {
  modeling_data <- final_dataset_97 %>%
    mutate(dow = as.factor(wday(date)), # week start on monday
           year = as.factor(year),
           hw_ind_2d = as.factor(hw_ind_97_2d),
           fips = as.factor(fips),
           statecode = as.factor(statecode)) 
  rm(final_dataset_97)
}

#----------------------------------------------------------------------------------------------------------------------- 
# Original Bobb et al model
#-----------------------------------------------------------------------------------------------------------------------


glmernb_model_step1 <- glmer.nb(n_hosp_fluid_electrolyte ~ hw_ind_2d + dow + year + 
                                  (1|fips) + offset(log(medicare_pop)), 
                                data = modeling_data, verbose = 2)

# view results
#print(glmernb_model_step1)
#summary(glmernb_model_step1)
#confint(glmernb_model_step1)

# save model
save(glmernb_model_step1, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/glmernb_model_step1.Rdata"))
