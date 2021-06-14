## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)


# Import dataset containing vascular plant traits -------------------------
load("data/Soil_Physic_Horizons.rda")

# Create final data set ---------------------------------------------------
Soil_Physic_mm <- Soil_Physic_Horizons %>%
  group_by(Date, Parcelle, Traitement, Exclos, Depth_up, Depth_down) %>%
  ## Collapse all date, and use the vector to create new rows
  mutate(Depth = paste(seq(Depth_up, Depth_down, by = 0.5), collapse = ",")) %>%
  separate_rows(Depth, sep = ",") %>%
  mutate(Depth = as.numeric(Depth))

usethis::use_data(Soil_Physic_mm, overwrite = TRUE)
