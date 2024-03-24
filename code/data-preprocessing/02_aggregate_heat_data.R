# R script to load heat data 

# Load libraries
library(tidyverse)
library(data.table)
library(fst)
library(lubridate)
library(weathermetrics)

# Initialize global variable
years <- 2000:2016

# Load denom data
load("heatwave-health-effects/data/unique_fips.Rdata")
load("heatwave-health-effects/data/unique_fips_states.Rdata")

#----------------------------------------------------------------------------------------------------------------------- 
# Load Heat Index Data
#-----------------------------------------------------------------------------------------------------------------------

path_to_heat_index <- "path_name"
heat_index <- readRDS(path_to_heat_index) %>%
  select(StCoFIPS, Date, HImax_C) %>% 
  rename(fips = StCoFIPS, date = Date, heat_index_max = HImax_C) %>%
  mutate(year = year(date),
         fips = as.character(fips)) %>%
  mutate(fips = as.integer(fips)) %>%
  filter(year %in% years) %>% 
  filter(fips %in% unique_fips) %>%
  filter(!is.na(heat_index_max))

#----------------------------------------------------------------------------------------------------------------------- 
# Calculate Heat Waves
#-----------------------------------------------------------------------------------------------------------------------

# Compute heat index percentiles for each fips per year
heat_index_percentiles <- heat_index %>%
  rename(heat_index = heat_index_max) %>%
  select(fips, year, heat_index) %>%
  group_by(fips, year) %>%
  summarize(heat_ind_97th = quantile(heat_index, probs = 0.97, na.rm = T, names = F),
            heat_ind_95th = quantile(heat_index, probs = 0.95, na.rm = T, names = F)) %>%
  ungroup()

# Merge percentile data back to original data
# Compute binary indicator for if daily index > percentile
heat_index_merged <- heat_index %>%
  rename(heat_index = heat_index_max) %>%
  left_join(heat_index_percentiles, by = c("fips", "year")) %>%
  arrange(fips, date) %>% # THIS IS IMPORTANT
  mutate(hw_ind_97_1d = ifelse(heat_index > heat_ind_97th, 1, 0),
         hw_ind_95_1d = ifelse(heat_index > heat_ind_95th, 1, 0)) %>%
  group_by(fips) %>% 
  # compute lead and lag variables to prep for 2 day heat wave calculation
  mutate(hw_ind_97_1d_lag = lag(hw_ind_97_1d, 1, default = 0),
         hw_ind_97_1d_lead = lead(hw_ind_97_1d, 1, default = 0),
         hw_ind_95_1d_lag = lag(hw_ind_95_1d, 1, default = 0),
         hw_ind_95_1d_lead = lead(hw_ind_95_1d, 1, default = 0)) %>%
  # compute 2 day heat wave calculation
  mutate(hw_ind_97_2d = ifelse((hw_ind_97_1d_lag == 1 & hw_ind_97_1d == 1) |
                                 (hw_ind_97_1d == 1 & hw_ind_97_1d_lead == 1), 1, 0),
         hw_ind_95_2d = ifelse((hw_ind_95_1d_lag == 1 & hw_ind_95_1d == 1) |
                                 (hw_ind_95_1d == 1 & hw_ind_95_1d_lead == 1), 1, 0)) %>%
  # compute lag variable for day before beginning of 2 day heat wave
  mutate(hw_ind_97_2d_lag = lag(hw_ind_97_2d, 1, default = 0),
         hw_ind_95_2d_lag = lag(hw_ind_95_2d, 1, default = 0)) %>%
  # compute indicator for first day of heat wave
  mutate(hw_ind_97_2d_day1 = ifelse(hw_ind_97_2d == 1 & hw_ind_97_2d_lag == 0, 1, 0),
         hw_ind_95_2d_day1 = ifelse(hw_ind_95_2d == 1 & hw_ind_95_2d_lag == 0, 1, 0)) %>%
  ungroup() %>%
  select(fips, date, heat_index, 
         hw_ind_97_1d, hw_ind_97_2d, hw_ind_97_2d_day1,
         hw_ind_95_1d, hw_ind_95_2d, hw_ind_95_2d_day1) %>%
  # generate indicator for whether a date is within 3 days of 2+ heat wave
  arrange(fips, date) %>%
  group_by(fips) %>%
  mutate(hw_ind_95_2d_lag1 = lag(hw_ind_95_2d, 1, default = 0),
         hw_ind_95_2d_lag2 = lag(hw_ind_95_2d, 2, default = 0),
         hw_ind_95_2d_lag3 = lag(hw_ind_95_2d, 3, default = 0),
         hw_ind_95_2d_lead1 = lead(hw_ind_95_2d, 1, default = 0),
         hw_ind_95_2d_lead2 = lead(hw_ind_95_2d, 2, default = 0),
         hw_ind_95_2d_lead3 = lead(hw_ind_95_2d, 3, default = 0),
         hw_ind_97_2d_lag1 = lag(hw_ind_97_2d, 1, default = 0),
         hw_ind_97_2d_lag2 = lag(hw_ind_97_2d, 2, default = 0),
         hw_ind_97_2d_lag3 = lag(hw_ind_97_2d, 3, default = 0),
         hw_ind_97_2d_lead1 = lead(hw_ind_97_2d, 1, default = 0),
         hw_ind_97_2d_lead2 = lead(hw_ind_97_2d, 2, default = 0),
         hw_ind_97_2d_lead3 = lead(hw_ind_97_2d, 3, default = 0)) %>%
  mutate(within_3d_hw_ind_95_2d = ifelse(hw_ind_95_2d_lag1 == 1 |
                                           hw_ind_95_2d_lag2 == 1 |
                                           hw_ind_95_2d_lag3 == 1 |
                                           hw_ind_95_2d == 1 |
                                           hw_ind_95_2d_lead1 == 1 |
                                           hw_ind_95_2d_lead2 == 1 |
                                           hw_ind_95_2d_lead3 == 1, 1, 0),
         within_3d_hw_ind_97_2d = ifelse(hw_ind_97_2d_lag1 == 1 |
                                           hw_ind_97_2d_lag2 == 1 |
                                           hw_ind_97_2d_lag3 == 1 |
                                           hw_ind_97_2d == 1 |
                                           hw_ind_97_2d_lead1 == 1 |
                                           hw_ind_97_2d_lead2 == 1 |
                                           hw_ind_97_2d_lead3 == 1, 1, 0)) %>%
  ungroup() %>%
  select(-hw_ind_95_2d_lag1, -hw_ind_95_2d_lag2, -hw_ind_95_2d_lag3,
         -hw_ind_95_2d_lead1, -hw_ind_95_2d_lead2, -hw_ind_95_2d_lead3,
         -hw_ind_97_2d_lag1, -hw_ind_97_2d_lag2, -hw_ind_97_2d_lag3,
         -hw_ind_97_2d_lead1, -hw_ind_97_2d_lead2, -hw_ind_97_2d_lead3)

# Summarize heatwave dates
sum(heat_index_merged$hw_ind_97_2d == 1)
sum(heat_index_merged$hw_ind_95_2d == 1)

#-------------------------------------------------------------------------------------------
### Add additional variables 
#-------------------------------------------------------------------------------------------

# Add month indicator (April-June = 1, July-September = 2, October-March = 0)
HI_month <- month(heat_index_merged$date)
heat_index_merged$month_type <- as.factor(case_when(HI_month %in% c(4, 5, 6) ~ 1,
                                                    HI_month %in% c(7, 8, 9) ~ 2, 
                                                    HI_month %in% c(1, 2, 3, 10, 11, 12) ~ 0))

# Join statecode info 
heat_index_final <- heat_index_merged %>% 
  left_join(unique_fips_states, by = c("fips")) %>%
  as.data.table()

# Create ind variable
heat_index_final$ind <- 1:nrow(heat_index_final) 

#-------------------------------------------------------------------------------------------
### Save results
#-------------------------------------------------------------------------------------------

save(heat_index_final, file = "heatwave-health-effects/data/heat_index_final.Rdata")
