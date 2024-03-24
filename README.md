# Heatwaves: A Burning Issue (Code Repository)
**Assessing the Temporal and Spatial Heterogeneity of Health Impacts from Extreme Heat Exposure among Elderly Populations in the Contiguous U.S. from 2000 to 2016**

This repository contains all of the code for Daniela Garcia's senior thesis project for the Harvard Undergraduate Statistics Department. 

## Abstract

2023 was the hottest year in recorded history. With the rise in global temperatures due to climate change, we will witness a greater number of sporadic temperature spikes, or "heatwaves", which have a wide range of short- and long-term effects on human health such as heat exhaustion, dehydration, fluid and electrolyte imbalance, and heat stroke. Previous work on the relationship between heatwave exposure and health outcomes has included studying the impact of heatwaves on both mortality and morbidity at varying intervals of time and across varying geographical areas. However, the bulk of the literature lacks a robust assessment of the spatio-temporal heterogeneity in heatwave vulnerability. Our research aimed to examine the heterogeneity in the link between heatwave exposure and hospitalizations for fluid and electrolyte imbalances among Medicare enrollees in the contiguous United States from 2000 to 2016. In particular, we utilized conditional over-dispersed Poisson regression to analyze temporal and spatial variations in heatwave vulnerability. Our most significant finding was that the Northeast (e.g., Vermont, New Hampshire, Maine, Massachusetts, New York), West Coast (California, Washington), and parts of the Midwest (Montana, Colorado, Wisconsin) experienced the greatest relative risk to fluid and electrolyte hospitalizations during a heatwave, which may be driven by infrastructural shortcomings such as a lack of air conditioning. Moreover, although the relative risk of hospitalizations due to fluid and electrolyte imbalances has decreased over the study period, the comparative risk of hospitalization during heatwave days versus non-heatwave days has remained fairly constant. Overall, our research underscores the persistent concern of heat-induced hospitalizations as a pressing issue and confirms that the effects of heatwaves are numerous and diverse. Our study gives an initial insight into where certain populations particularly struggle, but there is still more work to-do to keep all different types of individuals safe from extreme heat and its negative health impacts.

## Data Pipeline

### Data Preprocessing 

All data pre-processing code is stored in the `code/data-preprocessing` folder. The steps of data pre-processing are:

* Aggregate Denominator Data
    * `code/data-preprocessing/01_aggregate_denom_data.R`
* Aggregate Heat Data
    * `code/data-preprocessing/02_aggregate_heat_data.R`
* Aggregate Hospitalizations Data
    * `code/data-preprocessing/03_aggregate_hosp_data.R`
* Perform Matching (on 95th and 97th percentile data)
    * `code/data-preprocessing/04_perform_matching_95_perct.R`, `code/data-preprocessing/04_perform_matching_97_perct.R`
* Merge Matched Data to Hospitalizations Data (on 95th and 07th percentile data)
    * `code/data-preprocessing/05_merge_matched_data_to_hosp_95_perct.R`, `code/data-preprocessing/05_merge_matched_data_to_hosp_97_perct.R`
* Merge Covariates to Data (on 95th and 97th percentile data)
    * `code/data-preprocessing/06_merge_covariates_95_perct.R`, `code/data-preprocessing/06_merge_covariates_97_perct.R`

### Analysis 

All modeling, visualization, and table creation code is stored in the `code/analysis` folder. The various scripts are as follows:

* Modeling
    * `model_glmernb.R`
    * `model_gnm.R`
* Visualization
    * `plot_ac_data.R`
    * `plot_hosp_counts.R`
    * `plot_hosp_rates.R`
    * `plot_hw_maps.R`
    * `plot_medicare_pop.R`
    * `plot_RR_per_state.R`
    * `plot_state_coef_maps.R`
    * `plot_year_coef_timeseries.R`
* Table Creation
    * `bobb_et_al_fig2.R`  
    * `latex_models.R`
    * `models_summaries_save.R`

### Figures and Results

The `figures/` and `results/` folders contain all of the images and tables produced by the code in the `code/analysis/` folder. 
