# R script to load denom data

# Load libraries
library(tidyverse)
library(data.table)
library(fst)
library(lubridate)
library(weathermetrics)

# Initialize global variable
years <- 2000:2016

#----------------------------------------------------------------------------------------------------------------------- 
# Medicare mortality denominator
#-----------------------------------------------------------------------------------------------------------------------

# Read in fips to zip matching
path_to_zips_to_fips <- "path_name"
zips_to_fips <- read.csv(path_to_zips_to_fips) %>%
  select(year, zip, fips) 

# For years before 2011, use 2011 fips
fips_years_add <- 2000:2010
for (i in 1:length(fips_years_add)) {
  fips_temp <- zips_to_fips %>% filter(year == 2011) %>% 
    mutate(year = fips_years_add[i])
  
  zips_to_fips <- rbind(zips_to_fips, fips_temp)
}

# Determine which cols to read
dir_denom <- "path_name"
denom_vars <- c("zip", "statecode", "year", "qid", "sex", "race", "age", "hmo_mo", "dual",
                "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
                "pct_owner_occ", "education", "population", "smoke_rate", "mean_bmi") 

US_states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
               "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", 
               "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
               "UT", "VA", "VT", "WA", "WI", "WV", "WY")

denom <- rbindlist(lapply(years, function(y) {
  read_fst(paste0(dir_denom, "confounder_exposure_merged_nodups_health_", y, ".fst"), 
           columns = denom_vars, as.data.table = TRUE)
})) %>% filter(hmo_mo == 0) %>% 
  filter(statecode %in% US_states) %>% 
  left_join(zips_to_fips, by = c("zip", "year")) %>%
  filter(!is.na(fips))
 

# Manually fix issue where multiple states are assigned to one fips
# Manually adjust unique_fips_states
denom$statecode <- case_when(denom$statecode == "OH" & denom$fips == 21015 ~ "KY",
                             denom$statecode == "NH" & denom$fips == 23031 ~ "ME", 
                             denom$statecode == "DC" & denom$fips == 24031 ~ "MD",
                             denom$statecode == "KY" & denom$fips == 47125 ~ "TN",
                             denom$statecode == "DC" & denom$fips == 51013 ~ "VA",
                             denom$statecode == "DC" & denom$fips == 51059 ~ "VA",
                             .default = denom$statecode) 

# Compute number of fips per state per year
fips_per_state_year <- denom %>% group_by(statecode, year) %>%
  summarize(n_unique_fips = length(unique(fips)))

fips_per_state <- fips_per_state_year %>% group_by(statecode) %>%
  summarize(avg_n_unique_fips = mean(n_unique_fips)) 

# Save unique fips and qids
unique_fips <- unique(denom$fips)
 
# Save unique fips with statecodes and years
unique_fips_states_years <- denom %>% select(statecode, fips, year) %>% unique()

# Save unique fips with statecodes
unique_fips_states <- denom %>% select(statecode, fips) %>% unique()

# Save unique fips with qids and years
unique_qids_fips_years <- denom %>% select(qid, fips, year) 

# Save results
save(unique_fips, file = "heatwave-health-effects/data/unique_fips.Rdata")
save(unique_fips_states_years, file = "heatwave-health-effects/data/unique_fips_states_years.Rdata")
save(unique_fips_states, file = "heatwave-health-effects/data/unique_fips_states.Rdata")
save(unique_qids_fips_years, file = "heatwave-health-effects/data/unique_qids_fips_years.Rdata")

#----------------------------------------------------------------------------------------------------------------------- 
# Aggregate denom individual level covariates to fips level
#-----------------------------------------------------------------------------------------------------------------------

# Aggregate denom data to the fips level by week
denom_agg <- denom %>%
  group_by(fips, year, statecode) %>%
  summarize(medicare_pop = n(),
            n_male = sum(sex == 1, na.rm = T),
            n_female = sum(sex == 2, na.rm = T),
            n_white = sum(race == 1, na.rm = T),
            n_black = sum(race == 2, na.rm = T), 
            n_other = sum(race == 3, na.rm = T),
            n_asian = sum(race == 4, na.rm = T),
            n_hispanic = sum(race == 5, na.rm = T),
            n_indigenous = sum(race == 6, na.rm = T),
            n_age_65_74 = sum(age >= 65 & age <= 74, na.rm = T),
            n_age_75_84 = sum(age >= 75 & age <= 84, na.rm = T),
            n_age_85_94 = sum(age >= 85 & age <= 94, na.rm = T),
            n_age_95_plus = sum(age >= 95, na.rm = T),
            n_dual = sum(dual == 1, na.rm = T)
            , avg_pct_owner_occ = mean(pct_owner_occ, na.rm = T),
            avg_poverty = mean(poverty, na.rm = T),
            avg_popdensity = mean(popdensity, na.rm = T),
            avg_medianhousevalue = mean(medianhousevalue, na.rm = T),
            avg_medhouseholdincome = mean(medhouseholdincome, na.rm = T),
            avg_education = mean(education, na.rm = T),
            avg_smoke_rate = mean(smoke_rate, na.rm = T),
            avg_mean_bmi = mean(mean_bmi, na.rm = T),
            avg_population = mean(population, na.rm = T)
            ) %>%
  ungroup() %>%
  mutate(r_male = n_male / medicare_pop,
         r_female = n_female / medicare_pop,
         r_white = n_white / medicare_pop,
         r_black = n_black / medicare_pop, 
         r_other = n_other / medicare_pop,
         r_asian = n_asian / medicare_pop,
         r_hispanic = n_hispanic / medicare_pop,
         r_indigenous = n_indigenous / medicare_pop,
         r_age_65_74 = n_age_65_74 / medicare_pop,
         r_age_75_84 = n_age_75_84 / medicare_pop,
         r_age_85_94 = n_age_85_94 / medicare_pop,
         r_age_95_plus = n_age_95_plus / medicare_pop,
         r_dual = n_dual / medicare_pop) 
 
save(denom_agg, file = "heatwave-health-effects/data/denom_agg.Rdata")
