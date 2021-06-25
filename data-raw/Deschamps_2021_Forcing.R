## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)
library(lubridate)
library(microclima)


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
           (`Short Wave Downwell, CNR4 W m-2` + `Long Wave Downwell, ERA5 W m-2`))

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
Forcing <- left_join(Domine, DepthTemp_clean)

# Add predictions of surface properties -----------------------------------------------------
## Albedo
fit_Albedo <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_Albedo.RDS")

options(contrasts=c('contr.sum','contr.poly'))

em_Albedo <- emmeans::emmeans(fit_Albedo, spec = ~Fertilization:Exclos,
                              type = "response", level = 0.89) %>%
  as_tibble()

## LAI
fit_LAI <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_LAI.RDS")

options(contrasts=c('contr.sum','contr.poly'))

em_LAI <- emmeans::emmeans(fit_LAI, spec = ~Fertilization:Exclos,
                              type = "response", level = 0.89) %>%
  as_tibble()

## Dead
fit_Dead <- readRDS("/home/lucasd/OneDrive/Projects/Active_projects/Doctorat_Lucas/Analysis/R/Results/Chap2/ANOVA/ANOVA_Dead.RDS")

options(contrasts=c('contr.sum','contr.poly'))

em_Dead <- emmeans::emmeans(fit_Dead, spec = ~Fertilization:Exclos,
                           type = "response", level = 0.89) %>%
  as_tibble()

## Add mean prediction to the data frame
Fert <- c(0,0,14,14)
Excl <- c("Exclos", "Temoin", "Exclos", "Temoin")

i=1
for(i in 1:4){
  Forcing_i <- Forcing %>%
    mutate(Fertilization = Fert[i], Grazing = Excl[i],
           Albedo_pred = em_Albedo %>% filter(Fertilization == Fert[i], Exclos == Excl[i]) %>%
             pull(response),
           Albedo_pred_ground = em_Albedo %>% filter(Fertilization == Fert[i], Exclos == "Temoin") %>%
             pull(response),
           LAI_pred = em_LAI %>% filter(Fertilization == Fert[i], Exclos == Excl[i]) %>%
             pull(response),
           Dead_pred = em_Dead %>% filter(Fertilization == Fert[i], Exclos == Excl[i]) %>%
             pull(response),
           )
  if(i == 1) Deschamps_2021_Forcing <- Forcing_i
  if(i > 1) Deschamps_2021_Forcing <- bind_rows(Deschamps_2021_Forcing, Forcing_i)
}


# Create time series of surface properties for the model ----------------------------------------
Deschamps_2021_Forcing <- Deschamps_2021_Forcing %>%
  ## Create LAI and dead material time series
  mutate(Shade_pred = LAI_pred + Dead_pred,
         Shade_mod = ifelse(`Snow depth m` == 0, Dead_pred, 0),
         Shade_mod = ifelse(Month %in% c(7:9), Shade_pred, Shade_mod)) %>%
  ## Choose a value for x
  mutate(x = 5) %>%
  ## Create fractional canopy cover time serie
  mutate(FCC_pred = canopy(as.matrix(Shade_pred))[,1],
         FCC_mod =ifelse(`Snow depth m` == 0,Albedo_pred, 0)) %>%
  ## Compute ground and veg albedo time series
  mutate(Albedo_ground = ifelse(`Snow depth m` == 0, Albedo_pred_ground,
                                Albedo_Dom_CNR4),
         Albedo_veg = ifelse(`Snow depth m` == 0, Albedo_pred,
                                0)) %>%
  ## Compute sky view
  mutate(Alt = 10,
         Skyview_mod = 1)

# Compute net radiation ---------------------------------------------------
Deschamps_2021_Forcing <- Deschamps_2021_Forcing %>%
  ## Prepare columns
  mutate(Diffuse_Radiation = 0, Alt = 10, Lat = 73.171425, Long =  -79.886344,
         Albr = 0.02) %>%
  ## Compute net shortwave radiations
  mutate(
    Net_ShortWave = shortwaveveg(
      dni = as.matrix(`Short Wave Downwell, CNR4 W m-2`),
      dif = as.matrix(Diffuse_Radiation),
      julian = julday(Year, Month, Day),
      localtime = as.matrix(hour(Datetime)),
      lat = as.matrix(Lat),
      long = as.matrix(Long),
      dtm = as.matrix(Alt),
      svv = as.matrix(Skyview_mod),
      alb = as.matrix(Albedo_ground),
      fr = as.matrix(FCC_mod),
      albr = as.matrix(Albr),
      shadow = F,
      x = as.matrix(x),
      l = as.matrix(LAI_pred))[,1]) %>%
  ## Compute net long wave radiations
  mutate(Net_LongWave = `Long Wave Downwell, ERA5 W m-2`)

# Deschamps_2021_Forcing %>%
#   ggplot(aes(x = DOY, y = Net_ShortWave)) +
#   geom_line(aes(color = factor(Year))) +
#   facet_grid(Fertilization~Grazing)
#
# Deschamps_2021_Forcing %>% group_by(Year, Fertilization, Grazing) %>%
#   summarise(Net_ShortWave = sum(Net_ShortWave))

# Make dataset its final form ---------------------------------------------
Deschamps_2021_Forcing_Day <- Deschamps_2021_Forcing %>%
  ## Summarise by date
  group_by(Fertilization, Grazing, Date) %>%
  summarise_at(vars(Year, Month, Day, DOY,
                    `WindSpeed m/s`, `Air Temp, degC`,
                    `Relative Humidity`, `Specific humidity g/kg`,
                    `Long Wave Downwell, ERA5 W m-2`,
                    `Short Wave Downwell, CNR4 W m-2`, `Short Wave downwell, ERA5 W m-2`,
                    Net_ShortWave, Net_LongWave,
                    `Pressure kPa`,
                    `Precip, Total  mm/h`, `Precip, Rain`, `Precip, Snow`, `Precip  season cumul`,
                    `Snow depth m`,
                    Temp_11m,
                    Shade_mod, x , FCC_mod, Albedo_ground),
               .funs = mean)

usethis::use_data(Deschamps_2021_Forcing_Day, overwrite = TRUE)
