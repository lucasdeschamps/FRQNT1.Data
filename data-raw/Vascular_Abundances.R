## code to prepare `Vascular_Abundances` dataset goes here

## Source cleaning function
source("R/data.cleaning.R")

library(tidyverse)


# Import dataset containing vascular plant traits -------------------------
Traits <- readr::read_csv2("data-raw/Traits/Vascular_Traits.csv")

## Clean Trait characters
Traits_clean <- data.cleaning(Traits) %>%
  ## Select needed columns
  select(Parcelle, Traitement, Exclos, Herbivorie, Sp,
         BIOSHOOT_sec, Leaf_Msec)


# Import data set containing vascular plants density ----------------------
Dens <- readr::read_csv2("data-raw/Abundances/Vascular_Density.csv")

## Clean density characters
Dens_clean <- data.cleaning(Dens) %>%
  select(Parcelle, Traitement, Exclos, Herbivorie, Sp, Nbr_Tiges)

Vascular_Abundances_Clod <- left_join(Dens_clean, Traits_clean)

usethis::use_data(Vascular_Abundances_Clod, overwrite = TRUE)


