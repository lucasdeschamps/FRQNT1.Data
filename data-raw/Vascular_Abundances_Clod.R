## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)


# Import dataset containing vascular plant traits -------------------------
Traits <- readr::read_csv2("data-raw/Traits/Vascular_Traits.csv")

## Clean Trait characters
Traits_clean <- data.cleaning(Traits) %>%
  ## Select needed columns
  select(Parcelle, Traitement, Exclos, Herbivorie, Sp,
         BIOSHOOT_sec, Leaf_Msec, Poids_non_ligneux, Poids_1an,
         Nbre_ind_feuille, Nbre_feuille_mature, Nbre_feuille_tot, `Subsampling?`,
         Total_leaf_area, Leaf_number)


# Import data set containing vascular plants density ----------------------
Dens <- readr::read_csv("data-raw/Abundances/Vascular_Density.csv")

## Clean density characters
Dens_clean <- data.cleaning(Dens) %>%
  select(Parcelle, Traitement, Exclos, Herbivorie, Motte, Motte_enveloppe, Sp, Nbr_Tiges)


# Create final data set ---------------------------------------------------
## Join the two databases together
Vascular_Abundances_Clod <- left_join(Dens_clean, Traits_clean)

Vascular_Abundances_Clod <- Vascular_Abundances_Clod %>%
  #####
  ## Compute total biomass and primary productivity per species per plot
  mutate(Biomass_plot = select(., BIOSHOOT_sec, Leaf_Msec) %>% apply(1, sum, na.rm = T),
         Productivity_plot = ifelse(is.na(Poids_non_ligneux),
                                    Biomass_plot,
                                    select(., Poids_non_ligneux, Leaf_Msec, Poids_1an) %>% apply(1, sum, na.rm = T)
                                    )) %>%
  #####
  ## Interpolate the mass and PP of each species in each clod
  ### Compute the number of tiller in a plot
  group_by(Parcelle, Traitement, Exclos, Sp) %>%
  mutate(Tiller_nbr_plots = sum(Nbr_Tiges, na.rm = T)) %>%
  ungroup() %>%
  ### Compute the mean mass and productivity of an individual
  mutate(Biomass_ind_mean = Biomass_plot / Tiller_nbr_plots,
         Productivity_ind_mean = Productivity_plot / Tiller_nbr_plots) %>%
  ### Compute the mass and productivity of each species in each clod
  mutate(Biomass = Biomass_ind_mean * Nbr_Tiges,
         Productivity = Productivity_ind_mean * Nbr_Tiges) %>%
  #####
  ## Compute the leaf area of each species in each clod
  ### Compute leaf area
  mutate(LA = Total_leaf_area/Leaf_number) %>%
  ### Compute mean leaf area per species per treatment
  group_by(Traitement, Exclos, Sp) %>%
  mutate(LA_mean_treat = mean(LA, na.rm = T)) %>%
  ungroup() %>%
  ### Compute mean leaf area per species
  group_by(Sp) %>%
  mutate(LA_mean_sp = mean(LA, na.rm = T)) %>%
  #####
  ## Compute the mean number of leaves per individuals for each species of each plot
  ### Correct the number of individuals measured
  mutate(Nbre_ind_feuille = ifelse(is.na(`Subsampling?`), Tiller_nbr_plots, 15)) %>%
  ### Compute the mean number of leaves per individuals for each species in each plot
  mutate(Nbre_feuille_ind_plot = ifelse(is.na(`Subsampling?`),
                                    (Nbre_feuille_mature + Leaf_number)/ Nbre_ind_feuille, Nbre_feuille_mature/Nbre_ind_feuille)) %>%
  ### Compute the mean number of leaves per individuals for each species in each treatment
  group_by(Traitement, Exclos, Sp) %>%
  mutate(Nbre_feuille_ind_treat = mean(Nbre_feuille_ind_plot, na.rm = T)) %>%
  ### Compute the mean number of leaves per individuals for each species
  group_by(Sp) %>%
  mutate(Nbre_feuille_ind_sp = mean(Nbre_feuille_ind_plot, na.rm = T)) %>%
  ##### Complete the LA and Nbre_feuille columns
  mutate(LA_comp = ifelse(is.na(LA_mean_treat), LA_mean_sp, LA_mean_treat),
         Nbre_feuille_ind_treat_comp = ifelse(is.na(Nbre_feuille_ind_treat), Nbre_feuille_ind_sp, Nbre_feuille_ind_treat),
         Nbre_feuille_comp = ifelse(is.na(Nbre_feuille_ind_plot) | Nbre_ind_feuille < 10,
                                    Nbre_feuille_ind_treat_comp, Nbre_feuille_ind_plot)) %>%
  #####
  ## Compute the LAI per species per clod
  mutate(LAI = LA_comp * Nbre_feuille_comp * Nbr_Tiges) %>%
  #####
  ## Scale everything in m2
  mutate(Density_ind_m2 = Nbr_Tiges/0.01,
         Biomass_g_m2 = Biomass/0.01,
         Productivity_g_m2 = Productivity/0.01,
         LAI_m2_m2 = LAI/10000/0.01) %>%
  #####
  ## Compute relative measures
  ### Create the sum per clod
  group_by(Parcelle, Traitement, Exclos, Motte, Motte_enveloppe) %>%
  mutate(Density_clod = sum(Density_ind_m2, na.rm = T),
         Biomass_clod = sum(Biomass_g_m2, na.rm = T),
         Productivity_clod = sum(Productivity_g_m2, na.rm = T),
         LAI_clod = sum(LAI_m2_m2, na.rm = T)) %>%
  ungroup() %>%
  mutate(Density_rel = Density_ind_m2/Density_clod,
         Biomass_rel = Biomass_g_m2/Biomass_clod,
         Productivity_rel = Productivity_g_m2/Productivity_clod,
         LAI_rel = LAI_m2_m2/LAI_clod) %>%
  ##### Select columns
  select(Parcelle, Traitement, Exclos, Herbivorie, Motte, Motte_enveloppe, Sp,
         Density_ind_m2, Density_rel,
         Biomass_g_m2, Biomass_rel,
         Productivity_g_m2, Productivity_rel,
         LAI_m2_m2, LAI_rel)

# Add Treatments variables
Vascular_Abundances_Clod <- add.treatments(Vascular_Abundances_Clod)

usethis::use_data(Vascular_Abundances_Clod, overwrite = TRUE)


