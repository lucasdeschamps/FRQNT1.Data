## code to prepare soil properties for Deschamps 2021 model

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

library(tidyverse)
library(lubridate)
library(brms)
library(emmeans)
library(tidybayes)


# Import data set containing draws of soil physical properties ------------
load("data/Deschamps_2021_Soil_sat_draws.rda")

# Compute the median of each variable -------------------------------------
Deschamps_2021_Soil_sat_mean <- Deschamps_2021_Soil_sat_draws %>%
  group_by(Fertilization, Exclos, Horizon) %>%
  summarise_at(vars(logit_LOI, logit_Porosity, log_Density,
                    logit_VWC, theta_sat,
                    K_soil, VolHeatCap, ThermDiff,
                    K_soil_frozen, VolHeatCap_frozen, ThermDiff_frozen),
               .funs = median) %>%
  mutate(LOI = brms::inv_logit_scaled(logit_LOI),
         Porosity = brms::inv_logit_scaled(logit_Porosity),
         Density = exp(log_Density),
         VWC = brms::inv_logit_scaled(logit_VWC))


# Make dataset in its final form ---------------------------------------------
Deschamps_2021_Soil_sat_mean <- Deschamps_2021_Soil_sat_mean %>%
  ## Select relevant variables
  select(-logit_LOI:-logit_VWC)

usethis::use_data(Deschamps_2021_Soil_sat_mean, overwrite = TRUE)
