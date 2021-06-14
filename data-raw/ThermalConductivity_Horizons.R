## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)


# Import dataset containing thermal conductivity measurements -------------------------
ThermalK <- readr::read_csv("data-raw/Soil/Thermal_Conductivity_Horizons.csv")

## Clean horizons characters
ThermalK_clean <- data.cleaning(ThermalK)


# Import dataset containing soil physical properties ----------------------
load("data/Soil_Physic_mm.rda")

## Compute the mean predicted thermal conductivity at 5 cm
Horizons_5 <- Soil_Physic_mm %>%
  filter(Depth < 8 & Depth > 2) %>%
  group_by(Parcelle, Traitement, Exclos) %>%
  summarise_at(vars(Volume:ThermDiff), .funs =  mean) %>%
  mutate(Profondeur = 5)
## Compute the mean predicted thermal conductivity at 10 cm
Horizons_10 <- Soil_Physic_mm %>%
  filter(Depth < 13 & Depth > 7) %>%
  group_by(Parcelle, Traitement, Exclos) %>%
  summarise_at(vars(Volume:ThermDiff), .funs =  mean) %>%
  mutate(Profondeur = 10)
## Compute the mean predicted thermal conductivity at 10 cm
Horizons_15 <- Soil_Physic_mm %>%
  filter(Depth < 18 & Depth > 12) %>%
  group_by(Parcelle, Traitement, Exclos) %>%
  summarise_at(vars(Volume:ThermDiff), .funs =  mean) %>%
  mutate(Profondeur = 15)

Horizons <- bind_rows(Horizons_5, Horizons_10, Horizons_15)

# Create final data set ---------------------------------------------------
Soil_ThermalConductivity_Horizons <- ThermalK_clean %>%
  ### Average the repetitions
  group_by(Date, Parcelle, Traitement, Exclos, Profondeur) %>%
  summarise(Conductivite_thermique = mean(Conductivite_thermique, na.rm = T),
            mean_Temp = mean(Temp_moy, na.rm = T)) %>%
  ungroup() %>%
  left_join(Horizons)

# Add Treatments variables
Soil_ThermalConductivity_Horizons  <- Soil_ThermalConductivity_Horizons %>%
  rename(Depth = Profondeur) %>%
  add.treatments()

usethis::use_data(Soil_ThermalConductivity_Horizons, overwrite = TRUE)
