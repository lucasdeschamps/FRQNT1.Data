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
  # # mutate(Albedo_Domine_CNR4 = ifelse(is.na(Albedo_Dom_CNR4), 0, Albedo_Dom_CNR4 ))

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
  pivot_wider(names_from = depth, values_from = temp) %>%
  # Select only columns of interest
  select(Date, `10_CM`:`80_CM`) %>%
  # Rename columns
  rename(SoilTemp_10cm = `10_CM`,
         SoilTemp_120cm = `120_CM`,
         SoilTemp_160cm = `160_CM`,
         SoilTemp_20cm = `20_CM`,
         SoilTemp_200cm = `200_CM`,
         SoilTemp_30cm = `30_CM`,
         SoilTemp_300cm = `300_CM`,
         SoilTemp_40cm = `40_CM`,
         SoilTemp_80cm = `80_CM`)

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
         SoilTemp_10cm:SoilTemp_80cm, SoilTemp_11m,
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

