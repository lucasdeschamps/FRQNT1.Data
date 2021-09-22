## code to prepare `Vascular_Traits_Plot` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)


# Import dataset containing vascular plant traits -------------------------
Traits <- readr::read_csv2("data-raw/Traits/Vascular_Traits.csv")

## Clean Trait characters
Vascular_Traits_Clod <- data.cleaning(Traits) %>%
  ## Select needed columns
  select(Parcelle, Traitement, Exclos, Grazing, Sp,
         Leaf_N15, LNC, Leaf_C13, LCC, Root_N15, RNC, Leaf_pH,
         Hveg1:Hveg8,
         Leaf_Msec, Leaf_Mfresh, Root_Msec, Root_Mfresh,
         Total_leaf_area, Leaf_number) %>%
  ## Compute new traits
  mutate(Hveg = rowMeans(
    data.frame(Hveg1, Hveg2, Hveg3, Hveg4, Hveg5, Hveg6, Hveg7, Hveg8),
    na.rm = T),
    LDMC = Leaf_Msec/Leaf_Mfresh,
    RDMC = Root_Msec/Root_Mfresh,
    LA = Total_leaf_area/Leaf_number,
    SLA = Total_leaf_area/Leaf_Msec
    ) %>%
  select(Parcelle, Traitement, Exclos, Grazing, Sp,
         Leaf_N15, LNC, Leaf_C13, LCC, Root_N15, RNC, Leaf_pH,
         Hveg, LDMC, RDMC, LA, SLA)

# Summarise result per plot, as only a few plots have repeated mea --------
Vascular_Traits_Plot <- Vascular_Traits_Clod %>%
  group_by(Parcelle, Traitement, Exclos, Grazing, Sp) %>%
  summarise_all(.funs = mean, na.rm = T)

# Add Treatments variables
Vascular_Traits_Plot <- add.treatments(Vascular_Traits_Plot)

usethis::use_data(Vascular_Traits_Plot, overwrite = TRUE)


