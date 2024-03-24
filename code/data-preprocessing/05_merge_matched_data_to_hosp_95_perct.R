# R script to merged matched data to hospitalizations

# Load libraries
library(tidyverse)
library(data.table)
library(fst)

# Initialize global variable
years <- 2000:2016

#-------------------------------------------------------------------------------------------
### Join heat and hospitalizations data
#-------------------------------------------------------------------------------------------
  
# Load heat index data
load("heatwave-health-effects/data/heat_index_final.Rdata")

# Load heat-related hospitalization data
load("heatwave-health-effects/data/hosp_agg.Rdata")

# Load matched indices (with first day, yearly threshold)
load("heatwave-health-effects/data/matching_95th_perct/all_inds_95.Rdata")
    
# Merge data for just diag1 diagnosis codes
matched_dataset_95_hosp <- heat_index_final %>%
  filter(ind %in% all_inds_95) %>%
  mutate(year = year(date)) %>%
  left_join(hosp_agg, by = c("fips", "date")) %>% 
  replace(is.na(.), 0)

# Shows results
matched_dataset_95_hosp %>% 
  group_by(hw_ind_95_2d) %>%
  summarize(total_hosp_fluid_electrolyte = sum(n_hosp_fluid_electrolyte),
            total_hosp_renal_failure = sum(n_hosp_renal_failure),
            total_hosp_UTI = sum(n_hosp_UTI),
            total_hosp_septicemia = sum(n_hosp_septicemia),
            total_hosp_external_causes_heat = sum(n_hosp_external_causes_heat),
            total_hosp_vascular = sum(n_hosp_vascular),
            total_hosp_diabetes = sum(n_hosp_diabetes)
  ) %>%
  ungroup() %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------
### Save data
#-------------------------------------------------------------------------------------------

save(matched_dataset_95_hosp, file = "heatwave-health-effects/data/matched_dataset_95_hosp.Rdata")
