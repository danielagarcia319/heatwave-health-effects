# Kate's Model: overdispersed conditional Poisson regression model
# https://www.sciencedirect.com/science/article/pii/S0160412021004591 

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

gnm_model_step1 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow,  
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step1)
#summary(gnm_model_step1)  
#confint(gnm_model_step1)

# save model
save(gnm_model_step1, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step1.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Bobb et al model with year as an interaction term with treatment
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step2 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow + 
                         hw_ind_2d:year,  
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step2)
#summary(gnm_model_step2)
#confint(gnm_model_step2)

# save model
save(gnm_model_step2, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step2.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Bobb et al model with year and state as interaction terms with treatment
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step3 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow + 
                         hw_ind_2d:year + statecode + hw_ind_2d:statecode,  
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step3)
#summary(gnm_model_step3)
#confint(gnm_model_step3)

# save model
save(gnm_model_step3, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step3.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Bobb et al model with year and state as interaction terms with treatment
# and state as interaction with year
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step4 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow + 
                         hw_ind_2d:year + statecode + hw_ind_2d:statecode + year:statecode,
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step4)
#summary(gnm_model_step4)
#confint(gnm_model_step4)

# save model
save(gnm_model_step4, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step4.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Model with just year as interaction
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step5 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d*year,
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step5)
#summary(gnm_model_step5)
#confint(gnm_model_step5)

# save model
save(gnm_model_step5, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step5.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Model with just state as interaction
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step6 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d*statecode,
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step6)
#summary(gnm_model_step6) 
#confint(gnm_model_step6)

# save model
save(gnm_model_step6, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step6.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Model with r_black as interaction
#-----------------------------------------------------------------------------------------------------------------------


gnm_model_step7 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow +
                       r_black + r_black:hw_ind_2d,  
                       data = modeling_data, family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step7)
#summary(gnm_model_step7)  
#confint(gnm_model_step7)

# save model
save(gnm_model_step7, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step7.Rdata"))


#----------------------------------------------------------------------------------------------------------------------- 
# Model with AC_prev as interaction
#-----------------------------------------------------------------------------------------------------------------------

ac_model_data <- modeling_data %>% 
  filter(!is.na(ac_prob_wt_fips)) 

gnm_model_step8 <- gnm(n_hosp_fluid_electrolyte ~ hw_ind_2d + year + dow +
                         ac_prob_wt_fips + ac_prob_wt_fips:hw_ind_2d,  
                       data = ac_model_data, 
                       family = quasipoisson(), 
                       offset = log(medicare_pop), 
                       eliminate = factor(fips))

# view results
#print(gnm_model_step8)
summary(gnm_model_step8)  
#confint(gnm_model_step8)

# save model
save(gnm_model_step8, file = paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step8.Rdata"))
