## code to prepare `Moss_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

# Load packages
library(tidyverse)

# Import biomass data -------------------------------------------------------------
## Import raw mass data
Mass_raw <- read_csv2("data-raw/Abundances/Moss_Density.csv")

## Clean biomass data
Mass_clean <- data.cleaning(Mass_raw)

# Import WRC data ---------------------------------------------------------
# Import raw WRC data
WRC_raw <- read_csv2("data-raw/Traits/Moss_WRC.csv")

## Clean WRC data
WRC_clean <- data.cleaning(WRC_raw)

# Import trait data -------------------------------------------------------
# Import raw trait data
Traits_raw <- read_csv2("data-raw/Traits/Moss_Traits.csv")

## Clean Traits data
Traits_clean <- data.cleaning(Traits_raw)

# Import abundance in residual mix ----------------------------------------
# Import raw melange data
Melange_raw <- read_csv2("data-raw/Abundances/Moss_Abundance_Mix.csv", na = c("", "NA", "\n"))

## Clean Melange data
Melange_clean <- data.cleaning(Melange_raw)

#### Manipulation Data mass --------------------------------------------------
# Créer un tableau avec le poids de chaque motte
Mass_motte <- WRC_clean %>%
  filter(complete.cases(Biom_sec)) %>% # Garder une ligne par motte
  select(Parcelle, Traitement, Exclos, Grazing, Motte,
         Hauteur_motteMoy., Biom_sec) %>%
  rename(Masse_motte = Biom_sec)

# Créer un tableau avec le poids de chaque sous-motte
Mass_sousmotte <- Mass_clean %>% filter(complete.cases(Masse_tot)) %>%
  select(Parcelle, Traitement, Exclos, Grazing, Motte, Masse_tot) %>%
  rename(Masse_sousmotte = Masse_tot)

# Joindre les masses de sous-motte aux masses d'espèces
Mass <- Mass_clean %>%
  left_join(Mass_sousmotte) %>%
  left_join(Mass_motte) %>%
  rename(Biomasse_sousmotte = Masse)
  # group_by(Parcelle, Traitement, Exclos, Motte) %>%
  # mutate(Masse_sousmotte_sum = sum(Biomasse_sousmotte, na.rm = T)) %>%
  # mutate(Masse_sousmotte = ifelse(is.na(Masse_sousmotte), Masse_sousmotte_sum, Masse_sousmotte)) %>%
  # mutate(Masse_motte = ifelse(is.na(Masse_motte), Masse_sousmotte, Masse_motte))

summary(Mass)

# Créer un poid pour une motte dont on avait que les espèces
## Rempli les valeurs de ROC3 14 Exclos Motte 4
Mass$Masse_sousmotte[is.na(Mass$Masse_motte)] <- sum(Mass$Biomasse_sousmotte[is.na(Mass$Masse_motte)])
Mass$Masse_motte[is.na(Mass$Masse_motte)] <- sum(Mass$Biomasse_sousmotte[is.na(Mass$Masse_motte)])
## Rempli les valeurs de PR C1 Témoin motte 1 et 4
Mass$Masse_sousmotte[is.na(Mass$Masse_sousmotte)] <- Mass$Masse_motte[is.na(Mass$Masse_sousmotte)]/2

summary(Mass)

# Renomer sp. qui sont associees ensemble dans Data Traits
Mass <- Mass %>%
  mutate(Sp = ifelse (Parcelle == "26" & Traitement == "PAS" &
                        Herbivorie_enveloppe == "Non Broute" & Sp %in% c("Sco.sco", "Bra.alb"),
                      "Bra.alb+Sco.sco", Sp)) %>%
  mutate(Sp = ifelse (Parcelle == "ROC8" & Traitement == "14" &
                        Herbivorie_enveloppe == "Broute" & Sp %in% c("Lep.rip", "Bra.alb"),
                      "Lep.rip+Bra.alb", Sp))

# somme de surface des mottes par plot (pour calcul d'abondance plus tard)
Air_plot <- Mass %>%
  filter(complete.cases(Air_motte)) %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe) %>%
  summarise(Air_plot = sum(Air_motte))

#temporaire
#Mass <- Mass %>%
#  filter(complete.cases(Biomasse_sousmotte)) #retirer sans biomasse (enveloppe disparue)

#temporaire : Retirer mottes avec sp. sans biomasse
Mass <- Mass %>%
  subset(., Parcelle != "H1" | Traitement != "PO" |
           Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC3" | Traitement != "6" |
           Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC3" | Traitement != "6" |
           Herbivorie_enveloppe != "Non Broute" | Motte != "2") %>%
  subset(., Parcelle != "ROC8" | Traitement != "14" |
           Herbivorie_enveloppe != "Non Broute" | Motte != "3")

## Add the biomass of each sp in the residual organic mix
### Put mix data into long format
Melange_motte <- Melange_clean %>%
  select(-Terre, -Somme) %>%
  pivot_longer(.,cols= 6:31, names_to = "Sp", values_to = "prop_Mix", values_drop_na = TRUE) %>%
  mutate(prop_Mix = prop_Mix)

### Compute the proportion of mosses (organi) in the residual mix
Melange_motte <-  Melange_motte   %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe, Motte) %>%
  mutate(sum_prop_Org_Mix = sum(prop_Mix))

### Store the biomass of the organic mix
Melange_motte <- Mass_clean %>%
  filter(.,Sp == "Mélange_org") %>%
  select(Parcelle, Traitement, Exclos, Grazing, Motte, Masse) %>%
  rename(BiomassTot_Org_Mix = Masse) %>%
  right_join(Melange_motte) %>%
  mutate(BiomassTot_Org_Mix = ifelse(is.na(BiomassTot_Org_Mix), 0, BiomassTot_Org_Mix))

# Compute the proportion in the organic
Melange_motte  <- Melange_motte  %>%
  mutate(prop_Org_Mix = prop_Mix/sum_prop_Org_Mix) %>%
  mutate(Biomass_Org_Mix = prop_Org_Mix * BiomassTot_Org_Mix) %>%
  subset(., Parcelle != "E1" | Traitement != "PO" | Herbivorie_enveloppe != "Broute" | Motte != "3") #pas de mesure d'abondance pour cette motte

# Calculer la biomasse de chaque espèce dans la motte première
Biomass_motte <- Mass %>%
  select(-Herbivorie_enveloppe, - Herbivorie) %>%
  left_join(Melange_motte) %>%
  filter(.,Sp != "PV", Sp != "Lichen", Sp != "Mélange", Sp != "Mélange_org") %>%
  fill(Air_motte) %>%
  mutate(Biomasse_motte = ifelse(is.na(Biomass_Org_Mix),
                                 (Biomasse_sousmotte) * Masse_motte / Masse_sousmotte,
                                 (Biomasse_sousmotte + Biomass_Org_Mix) * Masse_motte / Masse_sousmotte)) %>%
  mutate(Biomass_g_cm2 = Biomasse_motte / Air_motte,
         Biomass_g_m2 = Biomass_g_cm2*10000) %>%
  ## Compute relative biomass
  group_by(Parcelle, Traitement, Exclos, Motte) %>%
  mutate(BiomTot_Motte = sum(Biomass_g_m2, na.rm = T)) %>%
  ungroup() %>%
  mutate(Biomass_rel = Biomass_g_m2/BiomTot_Motte) %>%
  subset(., Parcelle != "72" | Traitement != "PA" | Herbivorie_enveloppe != "Broute" | Motte != "3" | Sp != "NI18")

# Create final data set ---------------------------------------------------
Moss_Abundances_Clod <- Biomass_motte %>%
  select(Parcelle, Traitement, Exclos, Grazing, Sp, Motte, Sp, Biomass_g_m2, Biomass_rel)

## Add treatments
Moss_Abundances_Clod <- add.treatments(Moss_Abundances_Clod)

usethis::use_data(Moss_Abundances_Clod, overwrite = TRUE)

