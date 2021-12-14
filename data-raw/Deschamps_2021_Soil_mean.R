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
load("data/Deschamps_2021_Soil_draws.rda")

# Compute the median of each variable -------------------------------------
Deschamps_2021_Soil_mean <- Deschamps_2021_Soil_draws %>%
  group_by(Fertilization, Grazing, Horizon) %>%
  summarise_at(vars(logit_LOI, logit_Porosity, log_Density,
                    log_WTD, logit_VWC, theta_sat,
                    K_soil, VolHeatCap, ThermDiff,
                    K_soil_frozen, VolHeatCap_frozen, ThermDiff_frozen),
               .funs = median) %>%
  mutate(LOI = brms::inv_logit_scaled(logit_LOI),
         Porosity = brms::inv_logit_scaled(logit_Porosity),
         Density = exp(log_Density),
         VWC = brms::inv_logit_scaled(logit_VWC),
         WaterTableDepth = exp(log_WTD))


# Make dataset in its final form ---------------------------------------------
Deschamps_2021_Soil_mean <- Deschamps_2021_Soil_mean %>%
  ## Keep only two plots
  filter(Fertilization == "High N+P" & Grazing == "Ungrazed" |
           Fertilization == "Control" & Grazing == "Ungrazed") %>%
  ## Select relevant variables
  select(-logit_LOI:-logit_VWC)

usethis::use_data(Deschamps_2021_Soil_mean, overwrite = TRUE)
