# Compare models using texreg package

# Load libraries
library(lubridate)
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(gnm) 
library(glmmADMB)
library(tidyverse)
library(texreg)

# Determine percentile to run
perct <- 95

# Load dataset
if (perct == 95) {
  load("heatwave-health-effects/data/final_dataset_95.Rdata")
}

if (perct == 97) {
  load("heatwave-health-effects/data/final_dataset_97.Rdata")
}

# Load all models of interest
load(paste0("heatwave-health-effects/data/models_", perct, "th_perct/glmernb_model_step1.Rdata"))
load(paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step1.Rdata"))
load(paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step3.Rdata"))
load(paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step5.Rdata"))
load(paste0("heatwave-health-effects/data/models_", perct, "th_perct/gnm_model_step6.Rdata"))

# Bobb et al glmernb vs gnm (1)
# don't print stars because it is messed up for gnm (need to do it by hand)
texreg(list(`Negative Binomial Model` = glmernb_model_step1, 
            `Conditional Poisson Model` = gnm_model_step1), 
       caption = paste0("Recreating Bobb et al 2014's Model with a Negative ", 
                        "Binomial Model (glmer.nb) and Overdispersed Conditional Poisson Model (gnm) using ", 
                        perct, "th percentile data"), 
       caption.above = T, ci.force = F, longtable = T, single.row = F, stars = numeric(0),
       digits = 3, bold = F, use.packages = F,
       label = paste0("table:bobb_comparisons_", perct, "th_perct"),
       file = paste0("heatwave-health-effects/figures/tables/bobb_comparisons_", perct, "th_perct.tex"))

# all models
# don't print stars because it is messed up for gnm (need to do it by hand)
texreg(list(`(1)` = gnm_model_step1,
            `(2)` = gnm_model_step5,
            `(3)` = gnm_model_step6,
            `(4)` = gnm_model_step3), 
       caption = paste0("Comparing all four gnm models using ", perct, "th percentile data: ", 
                        "(1) Bobb et al 2014, (2) Interaction between Treatment and Year, ",
                        "(3) Interaction between Treatment and State, (4) Bobb et al with Year and State interacting with Treatment"), 
       caption.above = T, ci.force = F, longtable = T, single.row = F, stars = numeric(0),
       digits = 3, bold = F, use.packages = F,
       label = paste0("table:all_gnm_models_", perct, "th_perct"),
       omit.coef = "^statecode..$", 
       file = paste0("heatwave-health-effects/figures/tables/all_gnm_models_", perct, "th_perct.tex"))
  