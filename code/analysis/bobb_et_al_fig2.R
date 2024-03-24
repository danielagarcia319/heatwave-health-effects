# Re-create poisson regression from Bobb et al paper
# Supplementary materials: 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4319792/bin/NIHMS657527-supplement-Hospitalizations_and_heat_waves_-_Supplemental_Material.pdf

# Outcome Variable: number of hospitalizations
# Treatment Variable: heat wave
# Offset: medicare population
# Covariates: county, day of week (monday is reference), year as factor variables

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
# Recreate Figure 2 from Paper
#-----------------------------------------------------------------------------------------------------------------------

if (perct == 95) {
  # 95th percentile
  bobb_et_al_fig2_95th <- final_dataset_95 %>% 
    mutate(r_hosp_p100k = (n_hosp_fluid_electrolyte * 100000) / medicare_pop) %>%
    group_by(hw_ind_95_2d) %>%
    summarize(total_hosp = sum(n_hosp_fluid_electrolyte),
              average_daily_hosp_p100k = t.test(r_hosp_p100k)$estimate,
              lower = t.test(r_hosp_p100k)$conf.int[1],
              upper = t.test(r_hosp_p100k)$conf.int[2]) 
  write.csv(bobb_et_al_fig2_95th, file = "heatwave-health-effects/results/tables/bobb_et_al_fig2_95th.csv", row.names = F)
}

if (perct == 97) {
  # 97th percentile
  bobb_et_al_fig2_97th <- final_dataset_97 %>% 
    mutate(r_hosp_p100k = (n_hosp_fluid_electrolyte * 100000) / medicare_pop) %>%
    group_by(hw_ind_97_2d) %>%
    summarize(total_hosp = sum(n_hosp_fluid_electrolyte),
              average_daily_hosp_p100k = t.test(r_hosp_p100k)$estimate,
              lower = t.test(r_hosp_p100k)$conf.int[1],
              upper = t.test(r_hosp_p100k)$conf.int[2])
  write.csv(bobb_et_al_fig2_97th, file = "heatwave-health-effects/results/tables/bobb_et_al_fig2_97th.csv", row.names = F)
}
