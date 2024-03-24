# R script to merge covariates

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

#-----------------------------------------------------------------------------------------------------------------------
# Air Conditioning
#-----------------------------------------------------------------------------------------------------------------------

# Load data and compute weighted mean of ac prevalence for states
ac_data_state <- read.csv("heatwave-health-effects/data/AC_data/US_metro_ac_prob.csv") %>%
  select(StFIPS, TOTAL_POP, ac_prob) %>%
  group_by(StFIPS) %>%
  mutate(wt_state = TOTAL_POP / sum(TOTAL_POP)) %>%
  mutate(ac_prob_wt_state = weighted.mean(ac_prob, wt_state)) %>%
  ungroup() %>%
  select(StFIPS, ac_prob_wt_state) %>%
  unique() 

# Load data and compute weighted mean of ac prevalence for fips
ac_data_fips <- read.csv("heatwave-health-effects/data/AC_data/US_metro_ac_prob.csv") %>%
  select(StCoFIPS, TOTAL_POP, ac_prob) %>%
  group_by(StCoFIPS) %>%
  mutate(wt_fips = TOTAL_POP / sum(TOTAL_POP)) %>%
  mutate(ac_prob_wt_fips = weighted.mean(ac_prob, wt_fips)) %>%
  ungroup() %>%
  select(fips = StCoFIPS, ac_prob_wt_fips) %>%
  unique() 

#-------------------------------------------------------------------------------------------
### Merge to matched dataset(s)
#-------------------------------------------------------------------------------------------

# Load merged hosp/heat data
load("heatwave-health-effects/data/matched_dataset_95_hosp.Rdata")

# Load aggregated denom data
load("heatwave-health-effects/data/denom_agg.Rdata")

# Merge covariates to 95 percentile matched data
if (perct == 95) {
  merged_dataset_95 <- matched_dataset_95_hosp %>%
    left_join(denom_agg %>% select(fips, year, medicare_pop, r_black, n_black), by = c("fips", "year")) %>%
    mutate(StFIPS = as.integer(substring(str_pad(fips, 5, side = "left", pad = "0"), 1, 2))) %>%
    left_join(ac_data_fips, by = c("fips")) %>% 
    left_join(ac_data_state, by = c("StFIPS")) %>% 
    select(-StFIPS)
}

#-------------------------------------------------------------------------------------------
### Impute missing data (median imputation)
#-------------------------------------------------------------------------------------------

# Create a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing data 
if (perct == 95) {
  final_dataset_95 <- merged_dataset_95 %>%
    group_by(year, statecode) %>%
    mutate(medicare_pop = ifelse(is.na(medicare_pop), median(medicare_pop, na.rm = T), medicare_pop),
           r_black = ifelse(is.na(r_black), median(r_black, na.rm = T), r_black),
           n_black = ifelse(is.na(n_black), median(n_black, na.rm = T), n_black) 
    ) %>%
    ungroup() 
}

#-------------------------------------------------------------------------------------------
### Save final datasets
#-------------------------------------------------------------------------------------------

save(final_dataset_95, file = "heatwave-health-effects/data/final_dataset_95.Rdata")
