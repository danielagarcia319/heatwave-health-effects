Title: US metropolitan residential air conditioning prevalence

Description: Using data from the American Housing Survey (AHS) and the American Community Survey (ACS), census tract estimates of residential air conditioning (AC) probability are empirically derived for a sample of 115 metropolitan areas [core-based statistical areas (CBSAs)] across the US.

Release Date: 2022-09-14

This file is a CSV with the following fields:
	- CBSA: character identifier of census tract' core-based statistical area
	- CBSA_GEOID: numeric geographic identifier of census tract' CBSA
	- CDD_QUINTILE: character identifying census tract' CBSA quintile of annual cooling degree days (CDDs), based on 30-year climateaverage (1980-2010), details in Romitti et al. (2022)
	- StFIPS: numeric FIPS identifier for census tract' state
	- CoFIPS: numeric FIPS identifier for census tract' county
	- StCoFIPS: numeric FIPS identifier for census tract' state + county
	- NAME: character of census tract' name
	- TRACT_GEOID: numeric geographic identifier of census tract
	- TOTAL_POP: numeric estimate of census tract population, extracted from ACS 2019 5-year estimates
	- ac_prob: numeric probability of census tract level of any residential AC, methods detailed in Romitti et al. (2022)
	- cbsa_perc_rank_ac_prob: percentile ranking of ac_prob within census tract' CBSA

Source data:
	- AHS microdata https://www.census.gov/programs-surveys/ahs/data.html
	- ACS 2015-2019 5-year estimates https://api.census.gov/data/2019/acs/acs5/variables.html
	- NOAA NClimDiv county CDDs https://www.ncei.noaa.gov/pub/data/cirs/climdiv/

================================================================================
Documentation: 	US metropolitan residential air conditioning prevalence
================================================================================

Description: Using data from the American Housing Survey (AHS) and the American Community Survey (ACS), 
census tract estimates of residential air conditioning (AC) probability are empirically derived 
for a sample of 115 metropolitan areas [core-based statistical areas (CBSAs)] across the US.


---------- FILE METADATA -------------------------------------------------------
File name:              US_metro_ac_prob-1.tab
File created by:        Y. Romitti (email address)
Date added:             2022-09-14
Date last modified:     2022-09-14 (v1)

---------- DATA EXTENT/RESOLUTION ----------------------------------------------

Time extent/resolution: AC probabilities use ACS 2015-2019 5yr estimates as source data

Spatial extent:		115 US CBSAs
Spatial resolution:     census tract

---------- CHANGES BY VERSION NUMBER -------------------------------------------

v1     No changes; this is the original dataset

---------- VARIABLES -----------------------------------------------------------

CBSA: character identifier of census tract' core-based statistical area
CBSA_GEOID: numeric geographic identifier of census tract' CBSA
CDD_QUINTILE: character identifying census tract' CBSA quintile of annual cooling degree days (CDDs), based on 30-year climateaverage (1980-2010), details in Romitti et al. (2022)
StFIPS: numeric FIPS identifier for census tract' state
CoFIPS: numeric FIPS identifier for census tract' county
StCoFIPS: numeric FIPS identifier for census tract' state + county
NAME: character of census tract' name
TRACT_GEOID: numeric geographic identifier of census tract
TOTAL_POP: numeric estimate of census tract population, extracted from ACS 2019 5-year estimates
ac_prob: numeric probability of census tract level of any residential AC, methods detailed in Romitti et al. (2022)
cbsa_perc_rank_ac_prob: percentile ranking of ac_prob within census tract' CBSA


---------- RAW SOURCE DATA -----------------------------------------------------
Data Source 1: 	AHS microdata https://www.census.gov/programs-surveys/ahs/data.html
		* Accessed September 2021
Data Source 2: 	ACS 2015-2019 5-year estimates https://api.census.gov/data/2019/acs/acs5/variables.html
		* Accessed September 2021
Data Source 3: 	NOAA NClimDiv county CDDs https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
		* Accessed August 2021

---------- DESCRIPTION OF DATA SET DERIVATION ----------------------------------

The underlying methodology for the generation of residential AC probabilities
is described in Romitti et al. (2022)
