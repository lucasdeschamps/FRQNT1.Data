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
  mutate(Albedo_Domine_CNR4 = `Short Wave Upwelling radiation W m-2`/
           (`Short Wave Downwell, CNR4 W m-2`)) # %>%
  # # mutate(Albedo_Domine_CNR4 = ifelse(is.na(Albedo_Dom_CNR4), 0, Albedo_Dom_CNR4 )) %>%
  # group_by(Date) %>%
  # summarise(Year = unique(Year),
  #           Month = unique(Month),
  #           Day = unique(Day),
  #           DOY = unique(DOY),
  #           WindSpeed_m_s = mean(`WindSpeed m/s`, na.rm = T),
  #           AirTemp_degC = mean(`Air Temp, degC`, na.rm = T),
  #           RelHumidity = mean(`Relative Humidity`, na.rm = T),
  #           SpecHumidity_g_kg = mean(`Specific humidity g/kg`, na.rm = T),
  #           LongWave_Downwell_ERA5_W_m2 = mean(`Long Wave Downwell, ERA5 W m-2`, na.rm = T),
  #           ShortWave_Downwell_CNR4_W_m2 = mean(`Short Wave Downwell, CNR4 W m-2`, na.rm = T),
  #           Pressure_kPa = mean(`Pressure kPa`, na.rm = T),
  #           Precip_Total_mm_h = sum(`Precip, Total  mm/h`, na.rm = T),
  #           Precip_Rain = sum(`Precip, Rain`, na.rm = T),
  #           Precip_Snow = sum(`Precip, Snow`, na.rm = T),
  #           Snow_Depth_m = mean(`Snow depth m`, na.rm = T),
  #           Albedo_Domine_CNR4 = mean(Albedo_Dom_CNR4, na.rm = T))

# Import soil data at 11m -------------------------------------------------
DepthTemp_11m <- read_csv("data-raw/Climate/ds_000592164_Jour_DLY.csv")

## Format date
DepthTemp_11m_date <- DepthTemp_11m %>%
  mutate(Date = as_date(paste(year, month, day, sep = "-")))

## Clen temp a depth
DepthTemp_11m_clean <- DepthTemp_11m_date %>%
  # Filter only temp at 11m
  filter(depth == "1100_CM") %>%
  # Select only columns of interest
  select(Date, temp) %>%
  # Rename columns
  rename(SoilTemp_11m = temp)

# Import soil data at 3m -------------------------------------------------
DepthTemp_3m <- read_csv("data-raw/Climate/ds_000592160_Jour_DLY.csv")

## Format date
DepthTemp_3m_date <- DepthTemp_3m %>%
  mutate(Date = as_date(paste(year, month, day, sep = "-")))

## Clen temp a depth
DepthTemp_3m_clean <- DepthTemp_3m_date %>%
  # Filter only temp at 3m
  filter(depth == "300_CM") %>%
  # Select only columns of interest
  select(Date, temp) %>%
  # Rename columns
  rename(SoilTemp_3m = temp)

# Join Domine and depth temp ----------------------------------------------
Forcing <- left_join(Domine, DepthTemp_3m_clean) %>%
  left_join(DepthTemp_11m_clean) %>%
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
         SoilTemp_3m, SoilTemp_11m,
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
         )

usethis::use_data(Deschamps_2021_Forcing, overwrite = TRUE)

