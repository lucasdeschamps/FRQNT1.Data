## code to prepare `Gignac_Biomass_Plot` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)

# Import litter data ------------------------------------------------------
L <- read_csv2("data-raw/Environment/2019_Bylot_Litter.csv")

## Summarise by plot
L_sum <- L %>% group_by(ROC, Trt, B_NB) %>%
  summarise(Biomasse = sum(Biomasse)) %>%
  rename(Traitement = Trt,
         Broutement = B_NB,
         Litter = Biomasse) %>%
  mutate(Nb_motte = ifelse(ROC %in% c(5,7), 3, 2),
         Litter_g_m2 = Litter / (Nb_motte))

# Import living biomass data and join data sets ---------------------------
B <- read_csv2("data-raw/Environment/2019_Bylot_Biomass.csv")

D <- left_join(B, L_sum)

# Prepare final data set --------------------------------------------------
Gignac_Biomass_Plot <- D %>%
  mutate(Parcelle = paste("ROC", ROC, sep = ""),
         Grazing = ifelse(Broutement == "B", "Grazed", "Ungrazed")) %>%
  select(Parcelle, Traitement, Grazing, PP_Vasc, PP_Bryo, Litter_g_m2) %>%
  rename(Fertilization = Traitement,
         Biomass_Vasc_g_m2 = PP_Vasc, Biomass_Bryo_g_m2 = PP_Bryo) %>%
  filter(Parcelle %in% c("ROC3", "ROC6", "ROC7", "ROC8"),
         Fertilization %in% c(1, 4, 6, 10, 14))

usethis::use_data(Gignac_Biomass_Plot, overwrite = TRUE)
