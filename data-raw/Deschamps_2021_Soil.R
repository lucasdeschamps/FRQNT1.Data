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
    as_tibble() %>%
    filter(Fertilization %in% c(0,14)) %>%
    rename(mean_LOI = response,
           lower_LOI = lower.HPD,
           upper_LOI = upper.HPD)
  ## Compute Porosity ems
  em_Porosity <- emmeans(fit_Porosity, "Fertilization", by = "Exclos",
                    at = list(Depth = upper[d]:lower[d]),
                    type = "response") %>%
    as_tibble() %>%
    filter(Fertilization %in% c(0,14)) %>%
    rename(mean_Porosity = response,
           lower_Porosity = lower.HPD,
           upper_Porosity = upper.HPD)
  ## Compute Density ems
  em_Density <- emmeans(fit_Density, "Fertilization", by = "Exclos",
                         at = list(Depth = upper[d]:lower[d]),
                         type = "response") %>%
    as_tibble() %>%
    filter(Fertilization %in% c(0,14)) %>%
    rename(mean_Density = response,
           lower_Density = lower.HPD,
           upper_Density = upper.HPD)


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
  as_tibble() %>%
  filter(Fertilization %in% c(0,14)) %>%
  rename(mean_VWC = response,
         lower_VWC = lower.HPD,
         upper_VWC = upper.HPD)

em_WTD <- emmeans(fit_WTD, specs = "Fertilization", by = "Exclos",
                  cov.keep = "DOY",
                  type = "response") %>%
  as_tibble() %>%
  filter(Fertilization %in% c(0,14)) %>%
  rename(mean_WTD = response,
         lower_WTD = lower.HPD,
         upper_WTD = upper.HPD)

# Join everything together ------------------------------------------------
Deschamps_2021_Soil <- left_join(em_Soil, em_VWC) %>%
  left_join(em_WTD)

# Create a new mean WVC taking into account WTD ---------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  mutate(mean_VWC = ifelse(Fertilization != "14" |
                             Exclos != "Exclos" |
                             Horizon %nin% c("10-15", "15-300"),
         mean_VWC, mean_Porosity)) %>%
  mutate(mean_VWC = ifelse(Fertilization != "14" |
                             Exclos != "Temoin" |
                             Horizon %nin% c("5-10","10-15", "15-300"),
                           mean_VWC, mean_Porosity)) %>%
  mutate(mean_VWC = ifelse(Fertilization != "0" |
                             Exclos %nin% c("Temoin", "Exclos") |
                             Horizon %nin% c("5-10","10-15", "15-300"),
                           mean_VWC, mean_Porosity))


# Compute thermal conductivity --------------------------------------------
Deschamps_2021_Soil <- Deschamps_2021_Soil %>%
  ### Create variables used in equation
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
  mutate(mean_Particle_Density = 1 / (mean_LOI/rho_om + (1-mean_LOI)/rho_min)) %>%
  ## Compute OM and mineral volumetric fraction
  mutate(mean_V_om = mean_LOI * (mean_Density/rho_om),
         mean_V_min = (1-mean_LOI) * (mean_Density/rho_min)) %>%
  ### Compute volumetric fraction of soil solids
  mutate(mean_V_oms = mean_V_om / (1-mean_Porosity),
         mean_V_mins = 1-mean_V_oms) %>%
  ### Compute the proportion of pores saturated by water
  mutate(mean_theta_sat = mean_VWC/(mean_Porosity)) %>%
  ### Compute thermal conductivity of solids
  mutate(K_solid = K_om ^ mean_V_oms * K_min ^ mean_V_mins) %>%
  ### Compute thermal conductivity of dry soil
  mutate(K_dry = ( (a * K_solid - K_air) * mean_Density +
                     K_air * mean_Particle_Density)/
           (mean_Particle_Density - (1-a)*mean_Density)) %>%
  ### Compute the saturated conductivities
  mutate(K_sat = K_solid ^ (1-mean_Porosity) * K_water^(mean_Porosity),
         K_sat_frozen = K_solid ^ (1-mean_Porosity) * K_ice ^ mean_Porosity) %>%
  ### Compute the contribution of water to thermal conductivity
  mutate(K_e = (mean_theta_sat ^ (0.5 * (1 + mean_V_oms)) ) *
           ( (1/(1 + exp(- beta * mean_theta_sat)))^3 -
               ((1-mean_theta_sat)/2)^3  )^(1-mean_V_oms),
         K_e_frozen = mean_theta_sat ^ (1+mean_V_oms)
  ) %>%
  ### Compute predicted thermal conductivity
  mutate(K_soil = (K_sat - K_dry) * K_e + K_dry,
         K_soil_frozen = (K_sat_frozen - K_dry) * K_e_frozen + K_dry)

# Make dataset its final form ---------------------------------------------
Deschamps_2021_Forcing_Day <- Deschamps_2021_Forcing %>%
  ## Summarise by date
  group_by(Fertilization, Grazing, Date) %>%
  summarise_at(vars(Year, Month, Day, DOY,
                    `WindSpeed m/s`, `Air Temp, degC`,
                    `Relative Humidity`, `Specific humidity g/kg`,
                    `Long Wave Downwell, ERA5 W m-2`,
                    `Short Wave Downwell, CNR4 W m-2`, `Short Wave downwell, ERA5 W m-2`,
                    Net_ShortWave, Net_LongWave,
                    `Pressure kPa`,
                    `Precip, Total  mm/h`, `Precip, Rain`, `Precip, Snow`, `Precip  season cumul`,
                    `Snow depth m`,
                    Temp_11m,
                    Shade_mod, x , FCC_mod, Albedo_ground),
               .funs = mean)

usethis::use_data(Deschamps_2021_Forcing_Day, overwrite = TRUE)
