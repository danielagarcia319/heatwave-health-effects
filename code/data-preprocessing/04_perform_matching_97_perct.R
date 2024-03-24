# Perform matching on 97th percentile data

# Load libraries
library(data.table)
library(tidyverse)
library(lubridate)

# Load data
load("heatwave-health-effects/data/heat_index_final.Rdata")

#----------------------------------------------------------------------------------------------------------------------- 
# Prep Data
#-----------------------------------------------------------------------------------------------------------------------

# Create data frame of dates that are part of a heat wave event
heatwave_dates <- heat_index_final %>%
  filter(hw_ind_97_2d == 1) %>%
  select(fips, date, ind) %>%
  mutate(doy = yday(date),
         year = year(date)) %>%
  as.data.table()

# Create data frame of candidate dates that aren't part of a heat wave event
candidate_dates <- heat_index_final %>% 
  filter(within_3d_hw_ind_97_2d == 0) %>%
  select(fips, date, ind) %>%
  mutate(doy = yday(date),
         year = year(date)) %>%
  as.data.table()

#----------------------------------------------------------------------------------------------------------------------- 
# Perform Matching
#-----------------------------------------------------------------------------------------------------------------------

# Initialize variables
n <- nrow(heatwave_dates)
orig_inds_97 <- heatwave_dates$ind
hw_fips <- unique(heatwave_dates$fips)
matched_inds_97_df <- setNames(data.frame(matrix(ncol = 2, nrow = n)), c("fips", "matched_ind")) %>% 
  mutate(fips = as.integer(fips), matched_ind = as.integer(matched_ind)) %>%
  as.data.table()

# Run this code if paused running
#load("heatwave-health-effects/data/matching_97th_perct/matched_inds_97_df.Rdata")

i <- 1
last_j <- 1
last_k <- 1

for (j in last_j:length(hw_fips)) { 
  
  fips_temp <- hw_fips[j]
  candidate_dates_filter <- candidate_dates[fips == fips_temp,]
  heatwave_dates_filter <- heatwave_dates[fips == fips_temp,]
  n_temp <- nrow(heatwave_dates_filter)
  
  for(k in last_k:n_temp) {
    
    # Checkpoint
    if (i %% 1000 == 0) {
      cat(paste0("Overall Checkpoint: i = ", i, " / ", n, ", ", round(i/n*100, 3), "% done\n"))
    }
    
    # Extract current data
    temp_row <- heatwave_dates_filter[k,]
    
    # extract candidate matches based on fips, day of week, year
    # handle edge cases first
    if ((temp_row$doy > 362) | (temp_row$doy < 4)) {
      
      if (temp_row$doy == 363) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year &
             doy %in% c(360, 361, 362, 363, 364, 365, 366)) |
            (year != temp_row$year + 1 &
               doy %in% c(1)), ind] 
        
      } else if (temp_row$doy == 364) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year &
             doy %in% c(361, 362, 363, 364, 365, 366)) |
            (year != temp_row$year + 1 &
               doy %in% c(1, 2)), ind] 
        
      } else if (temp_row$doy == 365) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year &
             doy %in% c(362, 363, 364, 365, 366)) |
            (year != temp_row$year + 1 &
               doy %in% c(1, 2, 3)), ind] 
        
      } else if (temp_row$doy == 366) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year &
             doy %in% c(363, 364, 365, 366)) |
            (year != temp_row$year + 1 &
               doy %in% c(1, 2, 3)), ind] 
        
      } else if (temp_row$doy == 1) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year - 1 &
             doy %in% c(363, 364, 365, 366)) |
            (year != temp_row$year &
               doy %in% c(1, 2, 3, 4)), ind] 
        
      } else if (temp_row$doy == 2) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year - 1 &
             doy %in% c(364, 365, 366)) |
            (year != temp_row$year &
               doy %in% c(1, 2, 3, 4, 5)), ind] 
        
      } else if (temp_row$doy == 3) {
        temp_matches_pre_filter <- candidate_dates_filter[
          (year != temp_row$year - 1 &
             doy %in% c(365, 366)) |
            (year != temp_row$year &
               doy %in% c(1, 2, 3, 4, 5, 6)), ind] 
      }
    } else { # all other cases
      temp_matches_pre_filter <- candidate_dates_filter[
        year != temp_row$year &
          doy >= temp_row$doy - 3 &
          doy <= temp_row$doy + 3, ind]
    }
    
    # filter df of matched inds
    matched_inds_filter <- matched_inds_97_df[fips == fips_temp, matched_ind]
    temp_matches <- temp_matches_pre_filter[which(!(temp_matches_pre_filter %in% matched_inds_filter))]
    
    # check to see if any candidates exist
    if (length(temp_matches) == 0) {
      random_match <- 0
      
    } else if (length(temp_matches) == 1) {
      #select just the one match
      random_match <- temp_matches[1]
      
    } else {
      # randomly select a match from temp_matches
      set.seed(1)
      random_match <- sample(temp_matches, 1)
    }
    
    # store results
    matched_inds_97_df[i, c("fips")] <- fips_temp
    matched_inds_97_df[i, c("matched_ind")] <- random_match
    
    # Increase i
    i <- i + 1
  }
  
  # reset last k if running from paused results
  last_k <- 1
}

matched_inds_97 <- matched_inds_97_df[,matched_ind]

#----------------------------------------------------------------------------------------------------------------------- 
# Review matches
#-----------------------------------------------------------------------------------------------------------------------

# check for duplicates
length(matched_inds_97[which(matched_inds_97 != 0)]) == length(unique(matched_inds_97[which(matched_inds_97 != 0)]))

# Display heatwave dates and their matched dates side-by-side
matching_results <- heatwave_dates %>%
  select(fips, date) %>%
  cbind(data.frame(matched_ind = matched_inds_97)) %>%
  left_join(candidate_dates[ind %in% matched_inds_97, c("ind", "date", "fips")] %>% 
              rename(matched_date = date, matched_fips = fips, matched_ind = ind), by = c("matched_ind"))

matching_results$matched_date <- as.Date(matching_results$matched_date) 
matching_results$year_diff <- abs(year(matching_results$date) - year(matching_results$matched_date))
matching_results$doy_diff <- abs(yday(matching_results$date) - yday(matching_results$matched_date))
matching_results$do_fips_match = matching_results$fips == matching_results$matched_fips

# View dates with no match
View(matching_results %>% filter(is.na(matched_date)))

# Check if any dates have fips mis-match
any(matching_results$do_fips_match == FALSE, na.rm = T)


#----------------------------------------------------------------------------------------------------------------------- 
# Join final dataset 
#-----------------------------------------------------------------------------------------------------------------------

# Join orig and matched inds
all_inds_97 <- c(orig_inds_97[which(matched_inds_97 != 0)], 
                 matched_inds_97[which(matched_inds_97 != 0)])

# save results
save(matched_inds_97_df, file = "heatwave-health-effects/data/matching_97th_perct/matched_inds_97_df.Rdata")
save(matched_inds_97, file = "heatwave-health-effects/data/matching_97th_perct/matched_inds_97.Rdata")
save(all_inds_97, file = "heatwave-health-effects/data/matching_97th_perct/all_inds_97.Rdata")
