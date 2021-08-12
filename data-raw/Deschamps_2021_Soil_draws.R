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

# Import models -----------------------------------------------------------
fit_LOI <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_LOI.RDS")
fit_Porosity <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_Porosity.RDS")
fit_Density <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_Density.RDS")
fit_VWC <-readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_VWC.RDS")
fit_WTD <-readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_WTD.RDS")

# Extract mean values of soil parameters ---------------------------------------
options(contrasts=c('contr.sum','contr.poly'))

## Compute mean LOI and porosity by slices of 5cm
upper <- c(0,5,10)
lower <- c(5,10,15)

d = 1
for(d in 1:3){
  ## Compute LOI ems
  em_LOI <- emmeans(fit_LOI, "Fertilization", by = "Exclos",
                    at = list(Depth = upper[d]:lower[d]),
                    type = "response") %>%
    gather_emmeans_draws(value = "logit_LOI") %>%
    filter(Fertilization %in% c(0,14))

  ## Compute Porosity ems
  em_Porosity <- emmeans(fit_Porosity, "Fertilization", by = "Exclos",
                    at = list(Depth = upper[d]:lower[d]),
                    type = "response") %>%
    gather_emmeans_draws(value = "logit_Porosity") %>%
    filter(Fertilization %in% c(0,14))
  ## Compute Density ems
  em_Density <- emmeans(fit_Density, "Fertilization", by = "Exclos",
                         at = list(Depth = upper[d]:lower[d]),
                         type = "response") %>%
    gather_emmeans_draws(value = "log_Density") %>%
    filter(Fertilization %in% c(0,14))


  if(d == 1) em_Soil <- left_join(em_LOI, em_Porosity) %>%
    left_join(em_Density) %>%
    mutate(Horizon = paste(upper[d], lower[d], sep = "-"))
  if(d > 1) em_Soil <- bind_rows(em_Soil,
                                 left_join(em_LOI, em_Porosity) %>%
                                   left_join(em_Density) %>%
                                  mutate(Horizon = paste(upper[d], lower[d], sep = "-"))
                                 )
  if(d == 3) em_Soil <- bind_rows(em_Soil,
                                  left_join(em_LOI, em_Porosity) %>%
                                    left_join(em_Density) %>%
                                    mutate(Horizon = paste(15, 300, sep = "-"))
  )
}

# Compute expected mean of SVWC and WTD -----------------------------------
em_VWC <- emmeans(fit_VWC, specs = "Fertilization", by = "Exclos",
                  cov.keep = "DOY",
                  type = "response") %>%
  gather_emmeans_draws(value = "logit_VWC") %>%
  filter(Fertilization %in% c(0,14))

em_WTD <- emmeans(fit_WTD, specs = "Fertilization", by = "Exclos",
                  cov.keep = "DOY",
                  type = "response") %>%
  gather_emmeans_draws(value = "log_WTD") %>%
  filter(Fertilization %in% c(0,14))

# Join everything together ------------------------------------------------
Deschamps_2021_Soil <- left_join(em_Soil, em_VWC) %>%
  left_join(em_WTD)


# Create a new mean WVC taking into account WTD ---------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  mutate(logit_VWC = ifelse(Fertilization == "14" &
                             Exclos == "Exclos" &
                             Horizon %in% c("10-15", "15-300"),
         logit_Porosity, logit_VWC)) %>%
  mutate(logit_VWC = ifelse(Fertilization == "14" &
                             Exclos == "Temoin" &
                             Horizon %in% c("5-10","10-15", "15-300"),
                           logit_Porosity, logit_VWC)) %>%
  mutate(logit_VWC = ifelse(Fertilization == "0" &
                             Horizon %in% c("5-10","10-15", "15-300"),
                           logit_Porosity, logit_VWC))


# Create variables on natural scales --------------------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  mutate(LOI = brms::inv_logit_scaled(logit_LOI),
       Porosity = brms::inv_logit_scaled(logit_Porosity),
       Density = exp(log_Density),
       VWC = brms::inv_logit_scaled(logit_VWC),
       WTD = exp(log_WTD))


# Compute thermal conductivity --------------------------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  ### Create parameters used in equations
  mutate(rho_om = 1.3,
         rho_min = 2.65,
         K_om = 0.25,
         K_min = 2.5,
         K_air = 0.025,
         K_water = 0.57,
         K_ice = 2.3,
         a = 0.053,
         alpha = 0.24,
         beta = 18.3) %>%
  ## Compute particle density
  mutate(Particle_Density = 1 / (LOI/rho_om + (1-LOI)/rho_min)) %>%
  ## Compute OM and mineral volumetric fraction
  mutate(V_om = LOI * (Density/rho_om),
         V_min = (1-LOI) * (Density/rho_min)) %>%
  ### Compute volumetric fraction of soil solids
  mutate(V_oms = V_om / (1-Porosity),
         V_mins = 1-V_oms) %>%
  ### Compute the proportion of pores saturated by water
  mutate(theta_sat = VWC/(Porosity),
         theta_sat = ifelse(theta_sat > 1, 1, theta_sat)) %>%
  ### Compute thermal conductivity of solids
  mutate(K_solid = K_om ^ V_oms * K_min ^ V_mins) %>%
  ### Compute thermal conductivity of dry soil
  mutate(K_dry = ( (a * K_solid - K_air) * Density +
                     K_air * Particle_Density)/
           (Particle_Density - (1-a)*Density)) %>%
  ### Compute the saturated conductivities
  mutate(K_sat = K_solid ^ (1-Porosity) * K_water^(Porosity),
         K_sat_frozen = K_solid ^ (1-Porosity) * K_ice ^ Porosity) %>%
  ### Compute the contribution of water to thermal conductivity
  mutate(K_e = (theta_sat ^ (0.5 * (1 + V_oms)) ) *
           ( (1/(1 + exp(- beta * theta_sat)))^3 -
               ((1-theta_sat)/2)^3  )^(1-V_oms),
         K_e_frozen = theta_sat ^ (1+V_oms)
  ) %>%
  ### Compute predicted thermal conductivity
  mutate(K_soil = (K_sat - K_dry) * K_e + K_dry,
         K_soil_frozen = (K_sat_frozen - K_dry) * K_e_frozen + K_dry)

# Compute volumetric heat capacity ----------------------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  ## Create parameters used in equations
  mutate(C_om = 2.51e6,
         C_min = 2.01e6,
         C_air = 1297,
         C_water = 4.180e6,
         C_ice = 1.88e6) %>%
  ## Compute the volumic fraction of water
  mutate(V_air = 1 - theta_sat) %>%
  ## Compute Volumic heat capacity
  mutate(VolHeatCap = C_om * V_om + C_air * V_air + C_water * VWC + C_min * V_min,
         VolHeatCap_frozen = C_om * V_om + C_air + C_air + C_ice * VWC + C_min * V_min) %>%
  ## Compute thermal diffusivity
  mutate(ThermDiff = K_soil/VolHeatCap,
         ThermDiff_frozen = K_soil_frozen/VolHeatCap_frozen)

# Make dataset its final form ---------------------------------------------
Deschamps_2021_Soil_draws <- Deschamps_2021_Soil %>%
  ## Select relevant variables
  select(-.chain, - .iteration) %>%
  mutate(Horizon = factor(Horizon,
                          levels = c("0-5", "5-10", "10-15", "15-300")))

usethis::use_data(Deschamps_2021_Soil_draws, overwrite = TRUE)
