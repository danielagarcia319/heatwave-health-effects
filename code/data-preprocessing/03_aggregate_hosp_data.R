# R script to load hospitalizations data 

# Load libraries
library(tidyverse)
library(data.table)
library(fst)
library(lubridate)
library(weathermetrics)
library(tools)
library(icd)

# Load QID-fips-year data
load("heatwave-health-effects/data/unique_qids_fips_years.Rdata")
unique_qids <- unique(unique_qids_fips_years$qid)

# Create function to read data
read_data <- function(path, years, columns) {
  files <- list.files(path, pattern = ".fst", full.names = TRUE)
  
  ## extract suffix, ensure is in years
  files <- files[substr(file_path_sans_ext(files),
                        nchar(file_path_sans_ext(files)) - 3,
                        nchar(file_path_sans_ext(files)))
                 %in% years]
  out <- rbindlist(lapply(files, read_fst, as.data.table = T, columns = columns))
  return(out)
}

#-----------------------------------------------------------------------------------------------------------------------
# Load CCS data from icd R package
#-----------------------------------------------------------------------------------------------------------------------

## Create icd code lists
ccs_codes <- list()

## Load maps from the ICD package
data("icd10_map_ccs")
data("icd9_map_single_ccs")

## Harmonize structures/names for icd9/icd10
icd10_map_ccs <- icd10_map_ccs$single
names(icd10_map_ccs) <- paste0("ccs_", names(icd10_map_ccs))
names(icd9_map_single_ccs) <- paste0("ccs_", names(icd9_map_single_ccs))

selected_codes <- paste0("ccs_", c(55, 157, 159, 2, 244, 114, 50))
for (code in selected_codes) {
  ccs_codes[[code]][["icd10"]] <- icd10_map_ccs[[code]] 
  ccs_codes[[code]][["icd9"]] <- icd9_map_single_ccs[[code]]
}

# Initialize codes for just heat stroke
icd9_codes <- c("9920", "9921", "9922", "9923", "9924", 
                "9925", "9926", "9927", "9928", "9929")

icd10_codes <- c("T670XXA", "T670XXD", "T670XXS",
                 "T671XXA", "T671XXD", "T671XXS", 
                 "T672XXA", "T672XXD", "T672XXS", 
                 "T673XXA", "T673XXD", "T673XXS", 
                 "T674XXA", "T674XXD", "T674XXS", 
                 "T675XXA", "T675XXD", "T675XXS", 
                 "T676XXA", "T676XXD", "T676XXS", 
                 "T677XXA", "T677XXD", "T677XXS", 
                 "T678XXA", "T678XXD", "T678XXS",
                 "T679XXA", "T679XXD", "T679XXS")

#-----------------------------------------------------------------------------------------------------------------------
# Medicare Hospitalizations
#-----------------------------------------------------------------------------------------------------------------------

# Specify years to include
years_to_include <- 2000:2016

# Read raw data
hosp_vars <- c("QID", "ADATE", "DDATE", "DIAG1")

path_to_hosp <- "path_name"
hosp <- read_data(path = path_to_hosp, years = years_to_include, columns = hosp_vars)

# Mutate variables
hosp[, DIAG1 := as.character(DIAG1)]
hosp[, ADATE := dmy(ADATE)]
hosp[, DDATE := dmy(DDATE)]
hosp[, year := year(ADATE)]

hosp_names_orig <- names(hosp)
names(hosp) <- tolower(hosp_names_orig) 

#-----------------------------------------------------------------------------------------------------------------------
# Aggregate hospitalizations 
#-----------------------------------------------------------------------------------------------------------------------

# Identify hosps for ICD9 codes
hosp_icd9 <- hosp %>% 
  filter(ddate < as.Date("2015-10-01")) %>%
  mutate(
    hosp_992 = as.integer(diag1 %in% icd9_codes),
    hosp_T67 = as.integer(diag1 %in% icd10_codes),
    hosp_ccs_55 = as.integer(diag1 %in% ccs_codes[["ccs_55"]][["icd9"]]),
    hosp_ccs_157 = as.integer(diag1 %in% ccs_codes[["ccs_157"]][["icd9"]]),
    hosp_ccs_159 = as.integer(diag1 %in% ccs_codes[["ccs_159"]][["icd9"]]),
    hosp_ccs_2 = as.integer(diag1 %in% ccs_codes[["ccs_2"]][["icd9"]]),
    hosp_ccs_244 = as.integer(diag1 %in% ccs_codes[["ccs_244"]][["icd9"]]),
    hosp_ccs_114 = as.integer(diag1 %in% ccs_codes[["ccs_114"]][["icd9"]]),
    hosp_ccs_50 = as.integer(diag1 %in% ccs_codes[["ccs_50"]][["icd9"]])
  ) %>%
  filter(hosp_992 > 0 | hosp_T67 > 0 | hosp_ccs_55 > 0 | hosp_ccs_157 > 0 | hosp_ccs_159 > 0 | 
           hosp_ccs_2 > 0 | hosp_ccs_244 > 0 |  hosp_ccs_114 > 0 | hosp_ccs_50 > 0 
  ) %>%
  select(-ddate, -diag1)

# Identify hosps for ICD10 codes
hosp_icd10 <- hosp %>% 
  filter(ddate >= as.Date("2015-10-01")) %>%
  mutate(
    hosp_992 = as.integer(diag1 %in% icd9_codes),
    hosp_T67 = as.integer(diag1 %in% icd10_codes),
    hosp_ccs_55 = as.integer(diag1 %in% ccs_codes[["ccs_55"]][["icd10"]]),
    hosp_ccs_157 = as.integer(diag1 %in% ccs_codes[["ccs_157"]][["icd10"]]),
    hosp_ccs_159 = as.integer(diag1 %in% ccs_codes[["ccs_159"]][["icd10"]]),
    hosp_ccs_2 = as.integer(diag1 %in% ccs_codes[["ccs_2"]][["icd10"]]),
    hosp_ccs_244 = as.integer(diag1 %in% ccs_codes[["ccs_244"]][["icd10"]]),
    hosp_ccs_114 = as.integer(diag1 %in% ccs_codes[["ccs_114"]][["icd10"]]),
    hosp_ccs_50 = as.integer(diag1 %in% ccs_codes[["ccs_50"]][["icd10"]])
  ) %>%
  filter(hosp_992 > 0 | hosp_T67 > 0 | hosp_ccs_55 > 0 | hosp_ccs_157 > 0 | hosp_ccs_159 > 0 | 
           hosp_ccs_2 > 0 | hosp_ccs_244 > 0 |  hosp_ccs_114 > 0 | hosp_ccs_50 > 0 
  ) %>%
  select(-ddate, -diag1)

# Join ICD9 and 10 data
hosp_agg <- rbind(hosp_icd9, hosp_icd10) %>%
  rename(date = adate) %>%
  mutate(year = year(date)) %>%
  left_join(unique_qids_fips_years, by = c("qid", "year")) %>%
  filter(!is.na(fips)) %>%
  select(-year) %>%
  group_by(fips, date) %>%
  summarize(
    n_hosp_heat_992 = sum(hosp_992),
    n_hosp_heat_T67 = sum(hosp_T67),
    n_hosp_fluid_electrolyte = sum(hosp_ccs_55),
    n_hosp_renal_failure = sum(hosp_ccs_157),
    n_hosp_UTI = sum(hosp_ccs_159),
    n_hosp_septicemia = sum(hosp_ccs_2), 
    n_hosp_external_causes_heat = sum(hosp_ccs_244),
    n_hosp_vascular = sum(hosp_ccs_114),
    n_hosp_diabetes = sum(hosp_ccs_50)
  ) %>%
  ungroup() %>%
  replace(is.na(.), 0)

# Save aggregated data
save(hosp_agg, file = "heatwave-health-effects/data/hosp_agg.Rdata")
