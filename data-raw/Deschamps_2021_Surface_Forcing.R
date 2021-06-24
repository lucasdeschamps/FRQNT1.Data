## code to prepare `Vascular_Abundances_Clod` dataset goes here

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")

library(tidyverse)
library(lubridate)


# Import dataset containing atmospheric data -------------------------
Meteo <- readr::read_csv2("data-raw/Climate/Domine_2021_ESSD_Bylot_driving_data.csv")

# Import dataset containing surface conditions -------------------------
Surface <- readr::read_csv2("data-raw/Climate/Domine_2021_ESSD_Bylot_validation_snow_soil_radiation.csv")

# Join temporal forcing data sets -----------------------------------------
Domine <- bind_cols(Meteo, Surface) %>%
  # Reformat dates
  mutate(Datetime = as_datetime(DATE, format = "%d/%m/%y %H:%M")) %>%
  mutate(Date = as_date(Datetime),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         DOY = yday(Date))

## Summarise by day
Domine <- Domine %>%
  group_by(Date) %>%
  mutate_at(vars(`WindSpeed m/s`:`Precip  season cumul`,
                 `Snow depth m`:`Surf Temp °C`,
                 Year, Month, Day, DOY), .funs = mean)


# Compute observed albedo
Domine <- Domine %>%
  mutate(Albedo_Dom_CNR4 = `Short Wave Upwelling radiation W m-2`/
           (`Short Wave Downwell, CNR4 W m-2` + `Long Wave Downwell, ERA5 W m-2`))

# Import soil data at 11m -------------------------------------------------




usethis::use_data(Soil_ThermalConductivity_Horizon, overwrite = TRUE)
