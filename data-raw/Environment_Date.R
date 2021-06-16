## code to prepare `Environment_Date` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

# Load packages
library(tidyverse)


# Import dataset containing thermal conductivity measurements -------------------------
E2017 <- readr::read_csv2("data-raw/Environment/2017_Bylot_Environment.csv")
E2018 <- readr::read_csv2("data-raw/Environment/2018_Bylot_Environment.csv")
E2019 <- readr::read_csv2("data-raw/Environment/2019_Bylot_Environment.csv")


## Uniformize column names
E2018 <- E2018 %>% rename(Hobo_out = Hobo_2017, Hobo_In = Hobo_2018) %>%
  mutate(EC_Smart = NA, Humidity_Smart = NA, Temp_Smart = NA, Flux_CH4 = NA, Flux_CO2 = NA, Flux_H2O = NA,
         Eau = NA, Niveau_Eau = NA, Remarque = NA)

## Clean dates
E2017_date <- E2017 %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
lubridate::hour(E2017_date$Date) = 0
E2018_date <- E2018 %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
lubridate::hour(E2018_date$Date) = 0
E2019_date <- E2019 %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%y"))
lubridate::hour(E2018_date$Date) = 0

## Bind data frames
E_date <- bind_rows(E2017_date, E2018_date, E2019_date) %>%
  mutate(Date = format(Date, "%y-%m-%d"))

## Clean horizons characters
E_clean <- data.cleaning(E_date)


# Import dataset containing surface radiation ----------------------
Optic <- readr::read_csv2("data-raw/Environment/Bylot_Optic.csv") %>%
  select(-X27, -X28)

## Clean date
Optic_date <- Optic %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  mutate(Date = format(Date, "%y-%m-%d"))
lubridate::hour(Optic_date$Date) = 0

## Clean characters
Optic_clean <- data.cleaning(Optic_date)


# Create finale dataset ---------------------------------------------------
## Join datasets
Environment_Date <- E_clean %>%
  left_join(Optic_clean)

## Remove dates without thaw depth measurements
Environment_Date <- Environment_Date %>%
  filter(complete.cases(Front_degel))

## Calibrate soil volumetric water content measurements
### Back compute permittivity from wet sensor measurements
Environment_Date <- Environment_Date %>%
  mutate(Humidite_pourc = Humidite_pourc/100) %>%
  ## Compute permittivity for wet sensor
  mutate(Permittivity_Wet = ifelse(
    Type_sol == "Organique", permi.f(Humidite_pourc, b0 = 1.4, b1 = 8.4),
    permi.f(Humidite_pourc, b0 = 1.8, b1 = 10.1)
  )
  )
### Compute permittivity for SmartChamber
#### Extract data for stan
Stan <- Environment_Date %>% select(Date, Parcelle, Traitement, Exclos, Medaille, Humidity_Smart) %>%
  filter(complete.cases(Humidity_Smart)) %>%
  mutate(Theta = Humidity_Smart * 100)

StanData <- list(Theta = Stan$Theta, N = nrow(Stan),
                 A = -13.04, B = 3.819,
                 C = -9.129e-2, D = 7.342e-4)

#### Compile the model
mod_permi <- cmdstanr::cmdstan_model("data-raw/Environment/Permittivity_SmartChamber.stan")

#### Sample from the posterior
fit <- mod_permi$optimize(data = StanData, seed = 999, iter = 10000)

# bayesplot::mcmc_areas(fit$draws(c("P[1]", "P[2]", "P[3]", "P[26]")))

### Extract solution
Stan$Permittivity_Smart <- fit$draws("P") %>%
  as.vector()

#### Add solution to the original data frame
Environment_Date <- Environment_Date %>% left_join(Stan) %>%
  mutate(Permittivity = ifelse(is.na(Permittivity_Smart), Permittivity_Wet,
                               Permittivity_Smart * 1.0011 / (1.045 - 0.00225 * Temp_Smart))
  ) %>%
  # Compute soil water content
  mutate(SVWC = wet.f(Permittivity, b0 = 1.6, b1 = 8.4)) %>%
  # Join the temperature
  mutate(Soil_temp = ifelse(is.na(Temp), Temp_Smart, Temp))

### Compute optic indices
Environment_Date <- Environment_Date %>%
  mutate(NDVI = NDVI.fun(NIRi = NIR_i, NIRr = NIR_r,
                         Redi = Red_i, Redr = Red_r),
         PRI = PRI.fun(`570r` = `570_r`, `570i` = `570_i`,
                       `531r` = `531_r`, `531i` = `531_i`),
         Reflectance_blue = Blue_r/Blue_i,
         Reflectance_red = Red_r/Red_i,
         Reflectance_green = Green_r/Green_i,
         Reflectance_NIR = NIR_r/NIR_i,
         Albedo = microclima::albedo(Reflectance_blue,
                                     Reflectance_green,
                                     Reflectance_red,
                                     Reflectance_NIR,
                                     maxval = 1),
         Reflectance_visible = (Blue_r + Red_r + Green_r)/(Blue_i + Red_i + Green_i))

# Finalize the dataset
Environment_Date  <- Environment_Date %>%
  # Select relevant variables
  select(Date, Parcelle, Traitement, Exclos, Herbivorie,
         Sous_parcelle, Medaille,
         Front_degel, Soil_temp, SVWC, Niveau_Eau, pH,
         NDVI, PRI,
         Reflectance_blue, Reflectance_green, Reflectance_red,
         Reflectance_visible,Reflectance_NIR, Albedo) %>%
  # Rename variables
  rename(Thaw_depth = Front_degel,
         WaterTable_depth = Niveau_Eau) %>%
  ## Summarise by date
  group_by(Date, Parcelle, Traitement, Exclos) %>%
  summarise_at(vars(Thaw_depth:Albedo), .funs = mean, na.rm = T) %>%
  # Complete treatments
  add.treatments()

## Check the alignment
Environment_Date %>% select(Date, Parcelle, Traitement, Exclos, Albedo) %>% View


usethis::use_data(Environment_Date, overwrite = TRUE)
