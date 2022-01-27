## code to prepare `Moss_WRC_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

# Load packages
library(tidyverse)

# Import raw WRC data -----------------------------------------------------
# Import raw WRC data
WRC_raw <- read_csv2("data-raw/Traits/Moss_WRC.csv")

## Clean WRC data
WRC_clean <- data.cleaning(WRC_raw)

# Compute WRC -------------------------------------------------------------
# WRC pour la motte (standardiser par la masse)
Moss_WRC_Clod <- WRC_clean %>%
  filter(complete.cases(Biom_sec)) %>% # Garder une ligne par motte
  select(Parcelle, Traitement, Exclos, Grazing, Motte,
         Hauteur_motteMoy.,Eau_motte_t, Biom_sec, WRC) %>%
  rename(H_motte = Hauteur_motteMoy.) %>%
  mutate(WHC_motte = Eau_motte_t/Biom_sec) %>%
  mutate(WRC_motte = WRC/Biom_sec) # standardiser par la masse seche de la motte

# Create final data set ---------------------------------------------------
Moss_WRC_Clod <- Moss_WRC_Clod %>%
select(Parcelle, Traitement, Exclos, Grazing, Motte, H_motte, WHC_motte, WRC_motte) %>%
  rename(WHC = WHC_motte, WRC = WRC_motte) %>%
  add.treatments()

usethis::use_data(Moss_WRC_Clod, overwrite = TRUE)
