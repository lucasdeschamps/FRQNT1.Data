## code to prepare `Moss_Traits_Plot` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

# Load packages
library(tidyverse)

# Import raw traits data --------------------------------------------------
# Import raw trait data
Traits_raw <- read_csv2("data-raw/Traits/Moss_Traits.csv")

## Clean Traits data
Traits_clean <- data.cleaning(Traits_raw)

# Compute final traits ----------------------------------------------------
# Creation Data Traits
Moss_Traits_Plot <- Traits_clean %>%
  select(Parcelle, Traitement, Exclos, Grazing, Sp, nb_ind, m_frais,
         m_vert, m_brun, nb_emb_moy, LA, P_Ulaval, P_CNETE, Fonct_H_moy, Vert_H_moy, An1_H_moy,
         An2_H_moy, An3_H_moy, pH_moy, Si, dNormC13, dNNorm15, '%C', '%N', Ratio_CN) %>%
  rename(H_Fonct = Fonct_H_moy) %>%
  rename(H_Vert = Vert_H_moy ) %>%
  rename(H_An1 = An1_H_moy) %>%
  rename(H_An2 = An2_H_moy ) %>%
  rename(H_An3 = An3_H_moy) %>%
  rename(pH = pH_moy) %>%
  rename(m_frais_tot = m_frais) %>%
  rename(m_vert_tot = m_vert) %>%
  rename(m_brun_tot = m_brun) %>%
  rename(LA_tot = LA) %>%
  rename(SC13 = dNormC13) %>%
  rename(SN15 = dNNorm15) %>%
  rename(C = '%C') %>%
  rename(N = '%N' ) %>%
  mutate(m_sec_tot = m_vert_tot + m_brun_tot) %>% # calculer la masse sec (g)
  mutate(m_frais = m_frais_tot/nb_ind) %>%  # calculer la masse fraîche par individu (g)
  mutate(m_vert = m_vert_tot/nb_ind) %>%  # calculer la masse verte par individu (g)
  mutate(m_brun = m_brun_tot/nb_ind) %>% # calculer la masse brune par individu (g)
  mutate(m_sec = m_sec_tot/nb_ind) %>% # calculer la masse sec par individu (g)
  mutate(LDMC = m_sec_tot/m_frais_tot) %>% # Calculer LDMC  (g)
  mutate(WHC = (m_frais_tot-m_sec_tot)/m_sec_tot) %>% #Calculer water holding capacity, standardise par masse (g/g)
  mutate(LA = LA_tot/nb_ind) %>% # (cm2)
  mutate(SLA = LA_tot/m_sec_tot) %>% # Calculer SLA (cm2/g)
  mutate(P_CNETE_C = P_CNETE*0.0001) %>% # Calculer P tot en % (Au départ en mg/Kg)
  mutate(P_Ulaval_C = 0.893*P_Ulaval-0.013) %>% # Correction des valeurs de P Ulaval pour quelles correspondent a celle du CNETE (correction en fonction de l'equation P_CNETE~P_Ulaval, plus bas)
  mutate(P = coalesce(P_CNETE_C, P_Ulaval_C)) # creation d'une colonne avec les valeurs finales de P_CNETE corrigees et Remplir les NA avec les valeurs de Ulaval corrigees

## Select variables of interest and add treatments
Moss_Traits_Plot <- Moss_Traits_Plot %>%
  select(Parcelle, Traitement, Exclos, Grazing, Sp,
         m_vert, m_frais, m_sec, m_brun,
         H_Fonct, H_Vert,
         H_An1, H_An2, H_An3,
         LDMC, WHC, LA, SLA,
         C, N, SC13, SN15, pH, Si, Ratio_CN) %>%
  add.treatments()

usethis::use_data(Moss_Traits_Plot, overwrite = TRUE)
