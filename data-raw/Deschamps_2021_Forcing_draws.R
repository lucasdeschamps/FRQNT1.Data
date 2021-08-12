## code to prepare `Deschamps_2021_Forcing` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)
library(lubridate)
library(microclima)
library(tidybayes)
library(data.table)


# Function to compute k' (eq. 3 Microclima Maclean et al. MEE) ------------
k.prime <- function(x, saltitude){
  zen <- 90 - saltitude
  k_prime <- sqrt((x^2 + (tan(zen * (pi/180))^2)))/
    (x + 1.774 *(x + 1.182)^(-0.733))
  return(k_prime)
}

# Function to compute k* --------------------------------------------------
k.star <- function(x, LAI){
  x_t <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)
  p1_t <- c(-25.287, -25.254, -25.196, -24.932, -24.278,
            -22.088, -19.097, -15.255, -10.159, -7.105)
  p2_t <- c(86.439, 86.420, 86.399, 86.306, 86.078,
            85.517, 85.168, 85.228, 85.963, 86.708)

  p1 <- approx(x_t, p1_t, x)$y
  p2 <- approx(x_t, p2_t, x)$y
  A_star <- p1 * LAI^(1/3) + p2
  k_star <- sqrt((x^2 + (tan(A_star)^2)))/
    (x + 1.774 * (x + 1.182)^(-0.733))
  return(k_star)
}


# Function to compute net longwave radiation ------------------------------
Net.Longwave <- function (R_lsky, tc,  x, LAI, alpha_c)
{
  ## Temperature in Kelvin
  tk <- tc + 237.3
  ## Stefan-Boltzman constant
  sigma <- 2.043e-10
  ## Compute k_star
  k_star <- k.star(x, LAI)

  ## Compute radiations reaching the ground
  # e0 <- 0.6108 * exp(17.27 * tc/tk)
  # ws <- 0.622 * e0/pk
  # rh <- (h/ws) * 100
  # rh <- ifelse(rh > 100, 100, rh)
  # ea <- e0 * rh/100
  # epsilon <- (0.23 + 0.433*(ea-tk)^(1/8)) * (1-n^2) + (0.976 * n^2)
  # R_lsky <- epsilon * sigma * (tk)^4
  R_lwg <- exp(-k_star * LAI) * R_lsky

  ## Compute radiation scattered downward from leaves
  lr <- (2/3) * log(x + 1)
  r <-  r <- 1/(1 + exp(-1 * lr))
  R_lwl <- (1-alpha_c) * (1-r) * (1- exp(-k_star * LAI)) * R_lsky

  ## Compute radiation emitted by the canopy
  R_lwc = 0.51 * (1-alpha_c) * (1-exp(-k_star * LAI)) * sigma * tk^4
  ## Compute longwave budget
  R_lw = sigma * (tk)^4 - R_lwg + R_lwl + R_lwc

  return(R_lw)
}

# Import dataset containing atmospheric data -------------------------
Meteo <- readr::read_csv2("data-raw/Climate/Domine_2021_ESSD_Bylot_driving_data.csv")

# Import dataset containing surface conditions -------------------------
Surface <- readr::read_csv2("data-raw/Climate/Domine_2021_ESSD_Bylot_validation_snow_soil_radiation.csv")

# Join temporal forcing data sets -----------------------------------------
Domine_date <- bind_cols(Meteo, Surface) %>%
  # Reformat dates
  mutate(Datetime = as_datetime(DATE, format = "%d/%m/%y %H:%M")) %>%
  mutate(Date = as_date(Datetime),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         DOY = yday(Date))

# Compute observed albedo
Domine <- Domine_date %>%
  mutate(Albedo_Dom_CNR4 = `Short Wave Upwelling radiation W m-2`/
           (`Short Wave Downwell, CNR4 W m-2`)) %>%
  mutate(Albedo_Dom_CNR4 = ifelse(is.na(Albedo_Dom_CNR4), 0, Albedo_Dom_CNR4 ))

# Import soil data at 11m -------------------------------------------------
DepthTemp <- read_csv("data-raw/Climate/ds_000592164_Jour_DLY.csv")

## Format date
DepthTemp_date <- DepthTemp %>%
  mutate(Date = as_date(paste(year, month, day, sep = "-")))

## Clen temp a depth
DepthTemp_clean <- DepthTemp_date %>%
  # Filter only temp at 11m
  filter(depth == "1100_CM") %>%
  # Select only columns of interest
  select(Date, temp) %>%
  # Rename columns
  rename(Temp_11m = temp)

# Join Domine and depth temp ----------------------------------------------
Forcing <- left_join(Domine, DepthTemp_clean) %>%
  select(-DATE, - TUN) %>%
  # select(-Datetime) %>%
  # group_by(Date) %>%
  # summarise_all(.funs = mean, na.rm = T) %>%
  filter(Year == 2018, `Snow depth m` > 0) %>%
  data.table::as.data.table()

# Compute a raw estimate of soil albedo -----------------------------------
Albedo_s <- FRQNT1.Data::Environment_Date %>%
  filter(Parcelle  %in% c("ROC3", "ROC6", "ROC7", "ROC8"), complete.cases(Albedo), Exclos == "Temoin") %>%
  mutate(Year = lubridate::year(Date)) %>%
  filter(Year == 19) %>%
  group_by(Parcelle, Traitement) %>%
  mutate(min_Date = min(Date)) %>%
  filter(Date == min_Date) %>%
  group_by(Fertilization) %>%
  summarise(Albedo_surface = mean(Albedo)
  )

# Compute a raw estimate of veg albedo -----------------------------------
Albedo_v <- FRQNT1.Data::Environment_Date %>%
  filter(Parcelle  %in% c("ROC3", "ROC6", "ROC7", "ROC8"), complete.cases(Albedo)) %>%
  group_by(Parcelle, Traitement, Exclos) %>%
  mutate(max_Date = max(Date)) %>%
  filter(Date == max_Date) %>%
  group_by(Fertilization, Exclos) %>%
  summarise(Albedo_veg = mean(Albedo)
  )

# Add predictions of surface properties -----------------------------------------------------
## LAI
fit_LAI <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_LAI.RDS")

options(contrasts=c('contr.sum','contr.poly'))

em_LAI <- emmeans::emmeans(fit_LAI, spec = ~Fertilization:Exclos,
                              type = "response", level = 0.89) %>%
gather_emmeans_draws(value = "logit_LAI") %>%
  select(-.chain:-.draw) %>%
  group_by(Fertilization, Exclos) %>%
  do(sample_n(.,50)) %>%
  mutate(draw = 1:50) %>%
  ungroup()

## Dead
fit_Dead <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_Dead.RDS")

options(contrasts=c('contr.sum','contr.poly'))

em_Dead <- emmeans::emmeans(fit_Dead, spec = ~Fertilization:Exclos,
                           type = "response", level = 0.89) %>%
  gather_emmeans_draws(value = "logit_Dead") %>%
  select(-.chain:-.draw) %>%
  group_by(Fertilization, Exclos) %>%
  do(sample_n(.,50)) %>%
  mutate(draw = 1:50) %>%
  ungroup()


## Join together
em_Veg <- left_join(em_LAI, em_Dead)

## Join predictions to forcing data
Deschamps_2021_Forcing <- Forcing %>%
  modelr::data_grid(Date,
                    Fertilization = em_Veg$Fertilization,
                    Exclos = em_Veg$Exclos,
                    draw = em_Veg$draw)

Deschamps_2021_Forcing <- left_join(Forcing, Deschamps_2021_Forcing,
                                           allow.cartesian = T)
rm(list = c("DepthTemp", "DepthTemp_clean", "DepthTemp_date",
            "Domine", "Domine_date", "Meteo", "Surface", "Forcing",
            "em_Dead", "em_LAI"))
gc()
Deschamps_2021_Forcing <- left_join(Deschamps_2021_Forcing,
                                           em_Veg)
gc()

# Create time series of surface properties for the model ----------------------------------------
Deschamps_2021_Forcing <- Deschamps_2021_Forcing %>%
  mutate(LAI_pred = brms::inv_logit_scaled(logit_LAI),
         Dead_pred = brms::inv_logit_scaled(logit_Dead)) %>%
  ## Create LAI and dead material time series
  mutate(Dead_pred = ifelse(Exclos == "Exclos", LAI_pred, 0.1),
         Shade_pred = LAI_pred + Dead_pred,
         Shade_mod = ifelse(`Snow depth m` == 0, Dead_pred, 0),
         Shade_mod = ifelse(Month %in% c(7:9), Shade_pred, Shade_mod)) %>%
  group_by(draw) %>%
  ## Choose a value for x
  mutate(x = abs(rnorm(1, 1, 0.5)),
         Albedo_veg = 0.15) %>%
  ungroup() %>%
  ## Compute solar altitude
  mutate(Lat = 73.171425, Long =  -79.886344,
         SolAlt = solalt(hour(Datetime), Lat, Long, DOY)) %>%
  ## Join ground albedo estimates
  left_join(Albedo_s) %>%
  ## Compute the max of the day
  group_by(Date) %>%
  mutate(max_Albedo = max(Albedo_Dom_CNR4)) %>%
  ungroup()

gc()

# Compute net radiation ---------------------------------------------------
Deschamps_2021_Forcing <- Deschamps_2021_Forcing %>%
  ## Compute net shortwave radiations
  mutate(k_prime = k.prime(x, SolAlt),
         Net_shortwave = exp(-k_prime*Shade_pred) *
           `Short Wave Downwell, CNR4 W m-2`) %>%
  ## Compute net long wave radiations
  mutate(Net_longwave = Net.Longwave(R_lsky = `Long Wave Downwell, ERA5 W m-2`,
                                     tc = `Air Temp, degC`,
                                     x = x, LAI = Shade_pred,
                                     alpha_c = Albedo_veg)) %>%
  ## Compute net radiations
  mutate(Net_rad = Net_shortwave + Net_longwave) %>%
  ## Compute in MJ
  mutate(Net_shortwave_MJ = 0.0036 * Net_shortwave,
         Net_longwave_MJ = 0.0036 * Net_longwave,
         Net_rad_MJ = 0.0036 * Net_rad) %>%
  ## Compute cumulative sum
  group_by(Year, Fertilization, Exclos, draw) %>%
  mutate(cum_Net_shortwave_MJ = cumsum(Net_shortwave_MJ),
         cum_Net_longwave_MJ = cumsum(Net_longwave_MJ),
         cum_Net_rad_MJ = cumsum(Net_rad_MJ)) %>%
  ungroup() %>%
  as.data.table()

gc()

Deschamps_2021_Forcing %>%
  group_by(Fertilization, Exclos, Datetime, Year, DOY) %>%
  summarise(med_cum_Net_shortwave_MJ = median(cum_Net_shortwave_MJ),
            lower = quantile(cum_Net_shortwave_MJ, 0.05),
            upper = quantile(cum_Net_shortwave_MJ, 0.95)) %>%
  ggplot(aes(x = DOY, y = med_cum_Net_shortwave_MJ)) +
  geom_line(aes(color = Fertilization)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Fertilization),
              alpha = 0.2) +
  facet_wrap(~Exclos)

Deschamps_2021_Forcing %>%
  group_by(Fertilization, Exclos, draw) %>%
  summarise(sum_Net_shortwave_MJ = sum(Net_shortwave_MJ)) %>%
  group_by(Fertilization, Exclos) %>%
  summarise(med_sum_Net_shortwave_MJ = median(sum_Net_shortwave_MJ),
            lower = quantile(sum_Net_shortwave_MJ, 0.05),
            upper = quantile(sum_Net_shortwave_MJ, 0.95)) %>%
  ggplot(aes(x = Fertilization, y = med_sum_Net_shortwave_MJ)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(~Exclos)

Deschamps_2021_Forcing %>%
  group_by(Fertilization, Exclos, Datetime, Year, DOY) %>%
  summarise(cum_Net_longwave_MJ = median(cum_Net_longwave_MJ)) %>%
  ggplot(aes(x = DOY, y = cum_Net_longwave_MJ)) +
  geom_line(aes(color = Fertilization)) +
  facet_wrap(~Exclos)

Deschamps_2021_Forcing %>%
  group_by(Fertilization, Exclos, Datetime, Year, DOY) %>%
  summarise(cum_Net_rad_MJ = median(cum_Net_rad_MJ)) %>%
  ggplot(aes(x = DOY, y = cum_Net_longwave_MJ)) +
  geom_line(aes(color = Fertilization)) +
  facet_wrap(~Exclos)
#
# Deschamps_2021_Forcing %>% group_by(Year, Fertilization, Exclos) %>%
#   summarise(Net_ShortWave = sum(Net_ShortWave))

# Make dataset its final form ---------------------------------------------
Deschamps_2021_Forcing <- Deschamps_2021_Forcing %>%
  ## Summarise by date
  select(Date, Datetime, Fertilization, Exclos, Date,
         Year, Month, Day, DOY,
         `WindSpeed m/s`, `Air Temp, degC`,
         `Relative Humidity`, `Specific humidity g/kg`,
         `Long Wave Downwell, ERA5 W m-2`,
         `Short Wave Downwell, CNR4 W m-2`, `Short Wave downwell, ERA5 W m-2`,
         Net_ShortWave, Net_LongWave,
         `Pressure kPa`,
         `Precip, Total  mm/h`, `Precip, Rain`, `Precip, Snow`, `Precip  season cumul`,
         `Snow depth m`,
         Temp_11m,
         Shade_mod, x , FCC_mod, Albedo_surface)

# usethis::use_data(Deschamps_2021_Forcing, overwrite = TRUE)

