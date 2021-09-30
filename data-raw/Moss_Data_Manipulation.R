  
library(tidyverse)
library(readxl)    
library(openxlsx)  

#### Import data -------------------------------------------------------------

# Import raw mass data
Mass_raw <- read_xlsx("Data/Data_abondance_densite.xlsx") %>%
  mutate(Traitement = replace(Traitement, Traitement == "1.0", "1")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "4.0", "4")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "6.0", "6")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "10.0", "10")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "14.0", "14")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "26.0", "26")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "33.0", "33")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "35.0", "35")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "36.0", "36")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "43.0", "43")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "71.0", "71")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "72.0", "72")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Brouté", "Broute")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Non Brouté", "Non Broute")) %>%
  mutate(Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement))
  
# Import raw WRC data
WRC_raw <- read_xlsx("Data/Data_WRC.xlsx")%>%
  mutate(Traitement = replace(Traitement, Traitement == "1.0", "1")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "4.0", "4")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "6.0", "6")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "10.0", "10")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "14.0", "14")) %>%
  mutate(Parcelle = replace(Parcelle, Parcelle == "26.0", "26")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "33.0", "33")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "35.0", "35")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "36.0", "36")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "43.0", "43")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "71.0", "71")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "72.0", "72")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Brouté", "Broute")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Non Brouté", "Non Broute")) %>%
  mutate(Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement))  
  
# Import raw traits data
Traits_raw <- read_xlsx("Data/Data_Traits.xlsx") %>% 
  mutate(Traitement = replace(Traitement, Traitement == "1.0", "1")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "4.0", "4")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "6.0", "6")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "10.0", "10")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "14.0", "14")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "26.0", "26")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "33.0", "33")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "35.0", "35")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "36.0", "36")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "43.0", "43")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "71.0", "71")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "72.0", "72")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Brouté", "Broute")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Non Brouté", "Non Broute")) %>%
  mutate(Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement)) 
  
# Import raw melange data
Melange_raw <- read_xlsx("Data/Data_abondance_melange.xlsx") %>% 
  mutate(Traitement = replace(Traitement, Traitement == "1.0", "1")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "4.0", "4")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "6.0", "6")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "10.0", "10")) %>% 
  mutate(Traitement = replace(Traitement, Traitement == "14.0", "14")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "26.0", "26")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "33.0", "33")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "35.0", "35")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "36.0", "36")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "43.0", "43")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "71.0", "71")) %>% 
  mutate(Parcelle = replace(Parcelle, Parcelle == "72.0", "72")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Brouté", "Broute")) %>%
  mutate(Herbivorie_enveloppe = replace(Herbivorie_enveloppe, Herbivorie_enveloppe == "Non Brouté", "Non Broute")) %>%
  mutate(Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement)) 

# Import raw Data enviro
Enviro <- read_csv("Data/Enviro_Amelie.csv") %>%
  mutate(Exclos= replace(Exclos,Exclos == "Exclos", "Non Broute")) %>% 
  mutate(Exclos = replace(Exclos,Exclos == "Temoin", "Broute")) %>%
  rename(Herbivorie_enveloppe = Exclos) 


#### Manipulation Data mass --------------------------------------------------

# Créer un tableau avec le poids de chaque motte
Mass_motte <- WRC_raw %>%
  filter(complete.cases(Biom_sec)) %>% # Garder une ligne par motte
  select(Parcelle, Traitement, Herbivorie_enveloppe, Motte, 
         Hauteur_motteMoy., Biom_sec) %>% 
  rename(Masse_motte = Biom_sec) 

# Créer un tableau avec le poids de chaque sous-motte
Mass_sousmotte <- Mass_raw %>% filter(complete.cases(Masse_tot)) %>% 
  select(Parcelle, Traitement, Herbivorie_enveloppe, Motte, Masse_tot) %>% 
  rename(Masse_sousmotte = Masse_tot) 

# Joindre les masses de sous-motte aux masses d'espèces
Mass <- Mass_raw %>%
  left_join(Mass_sousmotte) %>% 
  left_join(Mass_motte) %>% 
  mutate(Exclos = ifelse(Herbivorie_enveloppe == "Non Broute", "Exclos", "Temoin")) %>%
  rename(Biomasse_sousmotte = Masse)  
  
# Créer un poid pour une motte dont on avait que les espèces
## Rempli les valeurs de ROC3 14 Exclos Motte 4
Mass$Masse_sousmotte[is.na(Mass$Masse_motte)] <- sum(Mass$Biomasse_sousmotte[is.na(Mass$Masse_motte)])
Mass$Masse_motte[is.na(Mass$Masse_motte)] <- sum(Mass$Biomasse_sousmotte[is.na(Mass$Masse_motte)])
## Rempli les valeurs de PR C1 Témoin motte 1 et 4
Mass$Masse_sousmotte[is.na(Mass$Masse_sousmotte)] <- Mass$Masse_motte[is.na(Mass$Masse_sousmotte)]/2

# Renomer sp. qui sont associees ensemble dans Data Traits
Mass <- Mass %>%
  mutate(Sp = ifelse (Parcelle == "26" & Traitement == "PAS" & Herbivorie_enveloppe == "Non Broute" & Sp %in% c("Sco.sco", "Bra.alb"), "Bra.alb+Sco.sco", Sp)) %>%
  mutate(Sp = ifelse (Parcelle == "ROC8" & Traitement == "14" & Herbivorie_enveloppe == "Broute" & Sp %in% c("Lep.rip", "Bra.alb"), "Lep.rip+Bra.alb", Sp))

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
  subset(., Parcelle != "H1" | Traitement != "PO" | Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC3" | Traitement != "6" | Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC3" | Traitement != "6" | Herbivorie_enveloppe != "Non Broute" | Motte != "2") %>%
  subset(., Parcelle != "ROC8" | Traitement != "14" | Herbivorie_enveloppe != "Non Broute" | Motte != "3") 

# Calculer la biomasse de chaque espèce dans la motte première
Biomass_motte <- Mass %>% 
  filter(.,Sp != "PV", Sp != "Lichen", Sp != "Mélange", Sp != "Mélange_org") %>% 
  fill(Air_motte)  %>% 
  mutate(Biomasse_motte = Biomasse_sousmotte * Masse_motte / Masse_sousmotte) %>% 
  mutate(Biomasse_g_cm2 = Biomasse_motte / Air_motte,        
         Biomasse_g_m2 = Biomasse_g_cm2*10000) %>% 
  subset(., Parcelle != "72" | Traitement != "PA" | Herbivorie_enveloppe != "Broute" | Motte != "3" | Sp != "NI18") 
  

# Obtenir biomasse par plot
Biomass_plot <- Biomass_motte %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe) %>% 
  mutate(Nb_motte = length(unique(Motte))) %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe, Sp) %>% 
  summarise(Biomasse_plot = sum(Biomasse_motte),
              Nb_motte = unique(Nb_motte)) %>% 
  left_join(.,Air_plot, by = c("Parcelle", "Traitement","Herbivorie_enveloppe")) %>% 
  ungroup() %>% 
  mutate(Biomasse_g_cm2 = Biomasse_plot/Air_plot,
         Biomasse_g_m2 = Biomasse_g_cm2*10000) 

# Obtenir abondance relative par plot 
Biomass_plot <- Biomass_plot %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe) %>% 
  summarise(Biomasse_tot_plot = sum(Biomasse_g_m2)) %>% 
  left_join(Biomass_plot) %>%  
  mutate(Ab_rel = Biomasse_g_m2/Biomasse_tot_plot) %>%  # Abondance relative
  group_by(Parcelle, Traitement, Herbivorie_enveloppe)  


#### Manipulation data WRC ----------------------------------------------------

# WRC pour la motte (standardiser par la masse)
WRC_motte <- WRC_raw %>%
  filter(complete.cases(Biom_sec)) %>% # Garder une ligne par motte
  select(Parcelle, Traitement, Herbivorie_enveloppe, Motte, 
         Hauteur_motteMoy.,Eau_motte_t, Biom_sec, WRC) %>%
  rename(H_motte = Hauteur_motteMoy.) %>%
  mutate(WHC_motte = Eau_motte_t/Biom_sec) %>%
  mutate(WRC_motte = WRC/Biom_sec) %>% # standardiser par la masse seche de la motte
  subset(., Parcelle != "35" | Traitement != "PS" | Herbivorie_enveloppe != "Non Broute" | Motte != "2") %>%  # pas de cylindre de densite pour ces mottes
  subset(., Parcelle != "35" | Traitement != "PS" | Herbivorie_enveloppe != "Non Broute" | Motte != "3") %>%
  subset(., Parcelle != "H1" | Traitement != "PO" | Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC7" | Traitement != "14" | Herbivorie_enveloppe != "Non Broute" | Motte != "1") %>%
  subset(., Parcelle != "ROC7" | Traitement != "14" | Herbivorie_enveloppe != "Non Broute" | Motte != "3") %>%
  subset(., Parcelle != "ROC7" | Traitement != "14" | Herbivorie_enveloppe != "Non Broute" | Motte != "4") 
  
 
# WRC pour la parcelle (community) 
WRC_plot <- WRC_motte %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, H_motte,
         WRC_motte, WHC_motte) %>%  
  group_by(Parcelle, Traitement, Herbivorie_enveloppe) %>% 
  mutate(H_plot = mean (H_motte)) %>%
  mutate(WRC_plot = mean(WRC_motte)) %>% 
  mutate(WHC_plot = mean (WHC_motte, na.rm = TRUE)) %>% 
  select(-c(H_motte, WRC_motte, WHC_motte)) %>% 
  distinct(Parcelle, Traitement, Herbivorie_enveloppe, .keep_all = T)
 

#### Manipulation data Traits ------------------------------------------------
# Creation Data Traits
Traits <- Traits_raw %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Sp, nb_ind, m_frais, 
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

# Comparer P Ulaval (%)  et CNETE_C (%)
#regP <-lm(P_CNETE_C ~ P_Ulaval, data = Traits) 
#coeff=coefficients(regP) 
#eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1)) 
#r2 <- summary(regP)$r.squared 
#regP #obtenir intercept + slope pour droite d'equation

#plot(P_CNETE_C~ P_Ulaval,data=Traits ,main="Comparaison analyses P",
#     xlab="P Ulaval (%)", ylab="P CNETE (%)",
#     xlim = c(0,0.15), ylim = c(0,0.15), bty='L') 
#abline(regP, col="red") 
#legend("topright",bty="n",legend = bquote(R^2 == .(r2))) 
#options(digits=2)

# Verification P Ulaval_C et CNETE_C
#regP <-lm(P_CNETE_C ~ P_Ulaval_C, data = Traits) 
#coeff=coefficients(regP) 
#eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1)) 
#r2 <- summary(regP)$r.squared 
#regP #obtenir intercept + slope pour droite d'equation

#plot(P_CNETE_C~ P_Ulaval_C,data=Traits ,main="Comparaison analyses P",
#     xlab="P Ulaval (%)", ylab="P CNETE (%)",
#     xlim = c(0,0.15), ylim = c(0,0.15), bty='L') 
#abline(regP, col="red") 
#legend("topright",bty="n",legend = bquote(R^2 == .(r2))) 
#options(digits=2)

#### Density -----------------------------------------------------------------

# Changer format data melange raw
Melange_motte <- Melange_raw %>%
  select(-Terre, -Somme) %>%
  pivot_longer(.,cols= 6:31, names_to = "Sp", values_to = "p_Mel", values_drop_na = TRUE) 

# calculer nouveau pourcentage des sp.pour melange organique (sans terre)
DSum_Melo <-  Melange_motte   %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe, Motte) %>%
  summarise(Sum_Melo = sum(p_Mel))

# obtenir biomasse de melange organique 
DB_Melo <- Mass_raw %>%
  filter(.,Sp == "Mélange_org") %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Motte, Masse) %>%
  rename(B_Melo = Masse)

# Ajouter Sum_Melo a data melange et calculer nouveau pourcentage pour m org.
Melange_motte  <- Melange_motte  %>%
  left_join(DSum_Melo) %>%
  mutate(p_Melo = p_Mel/Sum_Melo) %>%
  left_join(DB_Melo) %>%
  mutate(Bsp_Melo = p_Melo * B_Melo) %>%
  subset(., Parcelle != "E1" | Traitement != "PO" | Herbivorie_enveloppe != "Broute" | Motte != "3") #pas de mesure d'abondance pour cette motte

# Obtenir Biomasse sp dans melange par plot 
Melange_plot <- Melange_motte  %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe, Sp) %>%
  summarise(Bsp_Melo = sum(Bsp_Melo)) 


#Calculer le poids moyen d'un individu de chaque sp dans chaque parcelle 
D1 <- Traits %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Sp,
         nb_ind, m_vert_tot, m_brun_tot) %>% 
  mutate(B_indMoy1 = (m_vert_tot + m_brun_tot) / nb_ind) %>% 
  select(-m_vert_tot,-m_brun_tot, -nb_ind)

D2 <- Mass_raw %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Motte, Sp,
         Masse_ind, Nb_ind) %>% 
  mutate(B_indMoy2 = (Masse_ind / Nb_ind)) %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe, Sp) %>%
  subset(B_indMoy2!='NA') %>% 
  select(-Motte,-Masse_ind,-Nb_ind)

# Monter Data density
Density <- Biomass_plot %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Sp) %>%
  filter(.,Sp != "PV", Sp != "Lichen", Sp != "Mélange", Sp != "Mélange_org") %>% 
  left_join(D2) %>% 
  left_join(D1, by=c("Parcelle","Traitement","Herbivorie_enveloppe","Sp")) %>%
  mutate(B_indMoy = ifelse(is.na(B_indMoy1), B_indMoy2, B_indMoy1)) %>%
  left_join(Biomass_plot)  %>%
  left_join(Melange_plot) %>%
  mutate(Bsp_Melo= ifelse(is.na(Bsp_Melo),0,Bsp_Melo))

# Correction data density - remplir Na
B_indMoy_sp <- Density %>%
  select(Parcelle, Traitement, Herbivorie_enveloppe, Sp, B_indMoy) %>%
  group_by(Sp) %>%
  summarise(B_indMoy_sp = mean(B_indMoy, na.rm = T)) # calcul de la biomass/ind moyenne pour chq sp

Density <- Density %>%
  mutate(B_indMoy = ifelse (Parcelle == "26" & Traitement == "PAS" & Herbivorie_enveloppe == "Non Broute" & Sp == "NI1", 0.0015794444, B_indMoy)) %>% #valeur moy Poh.sp
  mutate(B_indMoy = ifelse (Parcelle == "43" & Traitement == "PAS" & Herbivorie_enveloppe == "Non Broute" & Sp == "Poh.nut+Aul.pal",0.00030 , B_indMoy)) %>% #valeur moy Poh.nut+Aul.pal+Bra.alb
  mutate(B_indMoy = ifelse (Parcelle == "71" & Traitement == "PA" & Herbivorie_enveloppe == "Non Broute" & Sp == "NI1",0.0038666667, B_indMoy)) %>% # "" Bra.alb
  mutate(B_indMoy = ifelse (Parcelle == "A1" & Traitement == "PR" & Herbivorie_enveloppe == "Non Broute" & Sp == "Dis.sp", 0.0006286667, B_indMoy)) %>% # "" Dis.sp
  mutate(B_indMoy = ifelse (Parcelle == "D1" & Traitement == "PR" & Herbivorie_enveloppe == "Broute" & Sp == "NI9",0.0009557302, B_indMoy)) %>% # ""Hep.sp
  mutate(B_indMoy = ifelse (Parcelle == "ROC3" & Traitement == "1" & Herbivorie_enveloppe == "Non Broute" & Sp == "NI2",0.0016535598, B_indMoy)) %>% # "" Bry.pse
  mutate(B_indMoy = ifelse (Parcelle == "ROC3" & Traitement == "6" & Herbivorie_enveloppe == "Non Broute" & Sp == "Sco.cos",0.0017509465, B_indMoy)) %>% # "" Sco.cos
  mutate(B_indMoy = ifelse (Parcelle == "ROC3" & Traitement == "14" & Herbivorie_enveloppe == "Broute" & Sp == "NI5",0.0016535598, B_indMoy)) %>% # "" Bry.pse
  mutate(B_indMoy = ifelse (Parcelle == "ROC6" & Traitement == "10" & Herbivorie_enveloppe == "Non Broute" & Sp == "NI20",0.0016535598, B_indMoy)) %>% # "" Bry.pse
  mutate(B_indMoy = ifelse (Parcelle == "ROC6" & Traitement == "14" & Herbivorie_enveloppe == "Non Broute" & Sp == "Aul.pal",0.0122055523, B_indMoy)) %>% # "" Aul.pal
  mutate(B_indMoy = ifelse (Parcelle == "ROC7" & Traitement == "14" & Herbivorie_enveloppe == "Broute" & Sp == "NI1", 0.0015794444, B_indMoy)) %>% # "" Poh.sp
  mutate(B_indMoy = ifelse (Parcelle == "ROC8" & Traitement == "10" & Herbivorie_enveloppe == "Non Broute" & Sp == "NI1",0.0027895533, B_indMoy)) # ""Cal.ric


#calcul de densite 
Density <- Density %>%
  mutate(Btot_g_cm2 = (Biomasse_plot + Bsp_Melo)/ Air_plot) %>%
  mutate(Density_sp_cm2 = Btot_g_cm2 / B_indMoy) 

Density_plot <- Density %>%
  group_by(Parcelle, Traitement, Herbivorie_enveloppe) %>%
  summarise(Density_plot_cm2 = sum(Density_sp_cm2))


# voir cas ou manque de l'information 
#sum(is.na(Density$B_indMoy))

#A_Verifier <- Density %>%
# subset(is.na(Density_sp_cm2))

#write.xlsx(x = A_Verifier,                       
#           file = "C:/Users/ameli/OneDrive/Documents/1-Maîtrise/R-Maîtrise/Résultats",      
#           sheetName = "A_Verifier")

#write.xlsx(x = Density_plot,                       
 #          file = "C:/Users/ameli/OneDrive/Documents/1-Maîtrise/R-Maîtrise/Résultats",      
#           sheetName = "Ensity_plot")
           