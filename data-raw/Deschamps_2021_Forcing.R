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
  mutate(Albedo_Domine_CNR4 = ifelse(is.na(Albedo_Dom_CNR4), 0, Albedo_Dom_CNR4 ))

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
  rename(SoilTemp_11m = temp)

# Join Domine and depth temp ----------------------------------------------
Forcing <- left_join(Domine, DepthTemp_clean) %>%
  select(-DATE, - TUN) %>%
  data.table::as.data.table()

# Make dataset its final form ---------------------------------------------
Deschamps_2021_Forcing <- Forcing %>%
  ## Select columns
  select(Date, Datetime,
         Year, Month, Day, DOY,
         `WindSpeed m/s`, `Air Temp, degC`,
         `Relative Humidity`, `Specific humidity g/kg`,
         `Long Wave Downwell, ERA5 W m-2`,
         `Short Wave Downwell, CNR4 W m-2`, `Short Wave downwell, ERA5 W m-2`,
         `Pressure kPa`,
         `Precip, Total  mm/h`, `Precip, Rain`, `Precip, Snow`, `Precip  season cumul`,
         `Snow depth m`,
         SoilTemp_11m,
         Albedo_Domine_CNR4) %>%
  ## Rename columns for commodity
  rename(WindSpeed_m_s = `WindSpeed m/s`, AirTemp_degC = `Air Temp, degC`,
         RelHumidity = `Relative Humidity`,
         SpecHumidity_g_kg = `Specific humidity g/kg`,
         LongWave_Downwell_ERA5_W_m2 = `Long Wave Downwell, ERA5 W m-2`,
         ShortWave_Downwell_CNR4_W_m2 = `Short Wave Downwell, CNR4 W m-2`,
         Pressure_kPa = `Pressure kPa`,
         Precip_Total_mm_h = `Precip, Total  mm/h`,
         Precip_Rain = `Precip, Rain`,
         Precip_Snow = `Precip, Snow`,
         Precip_Season_Cumul = `Precip  season cumul`,
         Snow_Depth_m = `Snow depth m`,
         ) %>%
  filter(complete.cases(SoilTemp_11m))

usethis::use_data(Deschamps_2021_Forcing, overwrite = TRUE)

