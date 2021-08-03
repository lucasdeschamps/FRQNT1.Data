## code to prepare time series of shallow soil temperature

# Empty the environment
rm(list = ls())

# Source cleaning function
source("R/data.cleaning.R")
source("R/add.treatments.R")
source("R/misc.R")

# Import libraries
require(tidyverse)
require(lubridate)


# Import dataset containing Hobos informations -------------------------
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
Enviro <- data.cleaning(E_date)

# Define the parent folder
parent_fol <- "data-raw/Soil/Soil_Temp_Hobo_CSV"

# Define the years folder
inifiles <- dir(parent_fol, full.names = T, pattern = ".ini")
year_fol <- dir(parent_fol, full.names = T)
year_fol <- year_fol[!year_fol %in% inifiles]
e = 1

for(e in 1:(length(year_fol))){
  # Define the treatments folders
  inifiles <- dir(year_fol[e], full.names = T, pattern = ".ini")
  treat_fol <- dir(year_fol[e], full.names = T)
  treat_fol <- treat_fol[!treat_fol %in% inifiles]
  t=1
  
  for(t in 1:length(treat_fol)){
    # Define the plot folders
    inifiles <- dir(treat_fol[t], full.names = T, pattern = ".ini")
    plot_fol <- dir(treat_fol[t], full.names = T)
    plot_fol <- plot_fol[!plot_fol %in% inifiles]
    p=1
    for(p in 1:length(plot_fol)){
      # Define the list of csv files for each plot
      plot_files <- dir(plot_fol[p], full.names = T, pattern = ".csv")
      
      if(length(plot_files) > 0){
        i = 1
        
        for(i in 1:length(plot_files)){
          # Importe csv
          x1 <- read.csv(plot_files[i], sep = ",", head = F)
          if(x1[1,3] == "") x2 <- x1[-1,]
          if(x1[1,3] != "") x2 <- x1
          
          # Extract serial number
          serial_list <- strsplit(as.character(x2$V3[1]), " ")[[1]]
          serialp <- serial_list[length(serial_list)]
          serialsplit <- strsplit(serialp, "")[[1]]
          x2$serial <-  noquote(paste(noquote(serialsplit[-length(serialsplit)]), collapse = ""))
          
          # Extract year number
          year_list <- strsplit(as.character(year_fol), "/")[[1]]
          yearlp <- year_list[length(year_list)]
          
          # Extract treatment number
          treat_list <- strsplit(as.character(treat_fol[t]), "/")[[1]]
          treatlp <- treat_list[length(treat_list)]
          
          # Extract plot number
          plot_list <- strsplit(as.character(plot_fol[p]), "/")[[1]]
          plotlp <- plot_list[length(plot_list)]
          
          # Extract herbivory status
          herbi_list <- strsplit(as.character(plot_files[i]), c("/"))[[1]]
          herbilp <- herbi_list[length(herbi_list)]
          herbi_list <- strsplit(as.character(herbilp), c("_"))[[1]]
          herbilp <- herbi_list[length(herbi_list)]
          
          # Add the site and plot identity
          x2$year <- yearlp
          x2$Traitement <- treatlp
          x2$Parcelle <- plotlp
          if(herbilp == "Exclos.csv") x2$Exclos <- "Exclos"
          if(herbilp == "Temoin.csv") x2$Exclos <- "Temoin"
          
          # Remove useless row
          x3 <- x2[-1,]
          
          # Change colnames
          x4 <- x3 %>% select(V2, V3, serial, year, Traitement, Parcelle, Exclos) %>% rename(old_date = V2, temp = V3) %>% 
            mutate(temp = as.numeric(as.character(temp)))
          
          # Transform into lubridate components
          x4$date_h <- as_datetime(as.character(x4$old_date), format = "%m/%d/%y %I:%M:%S %p")
          x4$date_h <- as.POSIXct(x4$date_h)
          x4$date <- as.POSIXct(
            format(
            as.POSIXct(x4$date_h,format='%m/%d/%y %I:%M:%S %p'),format='%m/%d/%Y'),
            format='%m/%d/%Y'
            )
          x5 <- x4 %>% 
            mutate(doy = yday(date_h), date_num = as.numeric(date_h),
                   Year = isoyear(date))
          
          ## Harvest dates
          dates <- Enviro %>% filter(Parcelle == unique(x5$Parcelle), 
                                     Traitement == unique(x5$Traitement), 
                                     Exclos == unique(x5$Exclos)) %>% 
            filter(complete.cases(Hobo_In | Hobo_Out)) %>% 
            pull(Date) %>% 
            unique() %>% 
            sort() %>% 
            as_date()
          
          ## Harvest depth
          Depth <- Enviro %>% filter(Parcelle == unique(x5$Parcelle), 
                                     Traitement == unique(x5$Traitement), 
                                     Exclos == unique(x5$Exclos)) %>% 
            filter(complete.cases(Hobo_In | Hobo_Out)) %>% 
            select(Date, Prof_hobo) %>% 
            mutate(Date = as_date(Date)) %>% 
            unique() %>% 
            arrange(Date)
          
          x6 <- x5 %>% filter(date == "01-01-01")
          
          ## Keep only dates during which the pendant was underground
          if(length(dates) > 0){
            ## Extract dates for each years
            seventeen <- dates[isoyear(dates) == 2017]
            heighteen <- dates[isoyear(dates) == 2018]
            nineteen <- dates[isoyear(dates) == 2019]
            
            ## Filter 2017 data
            if(e == 1 & length(seventeen) == 1 & length(heighteen) == 1){
              x6 <- x5 %>% filter(date > seventeen + 1 & date < heighteen - 1) %>% 
                mutate(Prof_hobo = Depth %>% filter(isoyear(Date) == 2018) %>% pull(Prof_hobo)
                       )
              
            } else if(e == 2 & length(heighteen) == 1 & length(nineteen) ==2){
              ##Filter 2018 data
              x6 <- x5 %>% filter(date > heighteen +1 & date < nineteen[1] - 1) %>% 
                mutate(Prof_hobo = 
                         Depth %>% filter(isoyear(Date) == 2019 & Date < "2019-07-01") %>% pull(Prof_hobo)
                       )
              
            }  else if(e == 2 & length(heighteen) == 1 & length(nineteen) == 1){
              ##Filter 2018 data
              x6 <- x5 %>% filter(date > heighteen +1 & date < nineteen[1] - 1) %>% 
                mutate(Prof_hobo = 
                         Depth %>% filter(isoyear(Date) == 2019) %>% pull(Prof_hobo)
                )
              
            }else if(e == 3 & length(nineteen) == 2){
              ##Filter 2019 data
              x6 <- x5 %>% filter(date > nineteen[1]+1 & date < nineteen[2]-1) %>% 
                mutate(Prof_hobo = 
                         Depth %>% filter(isoyear(Date) == 2019 & Date > "2019-07-01") %>% pull(Prof_hobo)
                       )
              
            } else if(e == 3 & length(nineteen) == 1){
              ##Filter 2019 data
              x6 <- x5 %>% filter(date > min(date+1) & date < nineteen-1) %>% 
                mutate(Prof_hobo = 
                         Depth %>% filter(isoyear(Date) == 2019 & Date > "2019-07-01") %>% pull(Prof_hobo)
                )
            }
          }
          
          if(e == 1 & t == 1 & p == 1 & i == 1){
            y <- x6
          }
          else{
            y <- rbind(y, x6)
          }
        }
      }
    }
  }
}

## Replace NA of 2019 by the values of 2018
Soil_Temp_TimeSeries <- y %>% filter(date == "2018-08-15") %>% 
  group_by(Parcelle, Traitement, Exclos) %>% 
  summarise(Prof_hobo_2018 = unique(Prof_hobo)) %>% 
  right_join(y) %>% 
  mutate(Prof_hobo = ifelse(is.na(Prof_hobo), Prof_hobo_2018, Prof_hobo))

usethis::use_data(Soil_Temp_TimeSeries, overwrite = TRUE)


# Plot individual time series ---------------------------------------------
library(ggforce)

npages <- Soil_Temp_TimeSeries %>% ggplot(aes(date_h, temp)) + 
  geom_line() + 
  theme_minimal() +
  facet_wrap_paginate(~ Traitement + Exclos + Parcelle, nrow = 2, ncol = 2, page = i)
npages <- n_pages(npages)
p<- list()

for(i in seq(npages)){
  p[[i]] <- y %>% ggplot(aes(date_h, temp)) + 
    geom_line() + 
    theme_minimal() +
    # geom_vline(aes(xintercept = as.POSIXct("2019-07-01")), linetype = 2) +
    facet_wrap_paginate(~ Traitement + Exclos + Parcelle, nrow = 2, ncol = 2, page = i)
}

pdf("SoilTemp.pdf")
p
dev.off()




