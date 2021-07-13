## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)


# Import dataset containing vascular plant traits -------------------------
Horizon <- readr::read_csv("data-raw/Soil/Soil_Horizons.csv")

## Clean horizons characters
Horizon_clean <- data.cleaning(Horizon)

# Create final data set ---------------------------------------------------
Soil_Physic_Horizon <- Horizon_clean %>%
  ## Correct uncertain data
  mutate(Prof_down = replace(Prof_down, Prof_down ==  "7 (6.5)7", 7)) %>%
  mutate(Prof_down = as.numeric(Prof_down)) %>%
  ## Correct bad data
  mutate(Poids_sec_brulage = replace(Poids_sec_brulage,
                                     Poids_sec_brulage == 39.4625,
                                     30.4625)) %>%
  ### Compute Volume
  mutate(Largeur_mean = rowMeans(data.frame(Largeur1, Largeur2, Largeur3 , Largeur4), na.rm = T),
         Longueur_mean = rowMeans(data.frame(Longueur1, Longueur2, Longueur3, Longueur4), na.rm = T),
         Hauteur_mean = rowMeans(data.frame(Hauteur1, Hauteur2, Hauteur3, Hauteur4), na.rm = T),
         Volume = Largeur_mean * Longueur_mean * Hauteur_mean,
         Prof_mean = (Prof_up + Prof_down)/2,
         Volume_brulage_ml = Volume_cuiller_ml * Nbre_cuiller,
         Poids_sec_brulage_net = Poids_sec_brulage - Poids_cupule,
         Poids_brule_net = Poids_brule - Poids_cupule) %>%
  ### Compute densities
  mutate(Density = Poids_sec / Volume) %>%
  ### Compute organic matter content
  mutate(LOI = (Poids_sec_brulage_net - Poids_brule_net)/Poids_sec_brulage_net,
         V_om = ((Poids_sec_brulage_net - Poids_brule_net)/1.3)/
           (Poids_sec_brulage_net/Density)) %>%
  ### Compute mineral content
  mutate(Mineral = (1 - (Poids_sec_brulage_net - Poids_brule_net))/Poids_sec_brulage_net,
         V_min = ((1 - (Poids_sec_brulage_net - Poids_brule_net)) / 2.65)/
           (Poids_sec_brulage_net/Density)) %>%
  ### Compute particle densities
  mutate(Particle_Density = ifelse(Poids_sec_brulage_net / Volume_brulage_ml < Density,
                                   Poids_sec_brulage_net / (Volume_brulage_ml),
                                   Poids_sec_brulage_net / Volume_brulage_ml),
         Particle_Density_computed = 1 / (LOI/1.3 + (1-LOI)/2.65)) %>%
  ### Compute porosity
  mutate(Porosity = (1 - (Density/Particle_Density)) * 100,
         Porosity_computed = (1 - Density/Particle_Density_computed)*100) %>%
  ### Compute water mass and water volume
  mutate(Water_mass = Poids_frais - Poids_sec) %>%
  mutate(Water_volume = Water_mass) %>%  #1g d'eau = 1cm3 d'eau
  ### Compute gravimetric and volumetric water content
  mutate(GWC = Water_mass / Poids_sec,
         VWC = Water_volume/Volume)

## Compute thermal conductivity following Balland and Arp 2005
Soil_Physic_Horizon <- Soil_Physic_Horizon %>%
  ### Create variables used in equation
  mutate(K_om = 0.25,
         K_min = 2.5,
         K_air = 0.025,
         K_water = 0.57,
         a = 0.053,
         alpha = 0.24,
         beta = 18.3) %>%
  ### Compute volumetric fraction of soil solids
  mutate(V_oms = V_om / (1-Porosity_computed/100),
         V_mins = 1-V_oms) %>%
  ### Compute the proportion of pores saturated by water
  mutate(theta_sat = VWC/(Porosity_computed/100)) %>%
  ### Compute thermal conductivity of solids
  mutate(K_solid = K_om ^ V_oms * K_min ^ V_mins) %>%
  ### Compute thermal conductivity of dry soil
  mutate(K_dry = ( (a * K_solid - K_air) * Density + K_air * Particle_Density_computed)/
           (Particle_Density_computed - (1-a)*Density)) %>%
  ### Compute the saturated conductivity
  mutate(K_sat = K_solid ^ (1-Porosity_computed/100) * K_water^(Porosity_computed/100)) %>%
  ### Compute the contribution of water to thermal conductivity
  mutate(K_e = (theta_sat ^ (0.5 * (1 + V_oms)) ) *
           ( (1/(1 + exp(- beta * theta_sat)))^3 -
               ((1-theta_sat)/2)^3  )^(1-V_oms)
  ) %>%
  ### Compute predicted thermal conductivity
  mutate(K_soil = (K_sat - K_dry) * K_e + K_dry)

## Compute Volumetric heat capacity
Soil_Physic_Horizon <- Soil_Physic_Horizon %>%
  ## Compute the volumic fraction of water
  mutate(V_air = 1 - (V_om + V_min + VWC)) %>%
  ## Compute Volumic heat capacity
  mutate(VolHeatCap = 2.496e6 * V_om + 1297 * V_air + 4.180e6 * VWC + 2.385e6 * V_min) %>%
  ## Compute thermal diffusivity
  mutate(ThermDiff = K_soil/VolHeatCap)

# Add Treatments variables
Soil_Physic_Horizon  <- Soil_Physic_Horizon %>%
  select(Date, Parcelle, Traitement, Exclos, Prof_up, Prof_down, Prof_mean, Oxydo_Reduction,
         Volume, Density, LOI, V_om, V_oms, Mineral, V_min, V_mins,
         Particle_Density, Particle_Density_computed,
         Porosity, Porosity_computed, GWC, VWC,
         theta_sat, K_solid, K_dry, K_sat, K_e, K_soil,
         VolHeatCap, ThermDiff) %>%
  rename(Depth_up = Prof_up, Depth_down = Prof_down, Depth_mean = Prof_mean) %>%
  add.treatments()

usethis::use_data(Soil_Physic_Horizon, overwrite = TRUE)
