### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 5 - Extracting Environmental Variables 

# This script extracts mean annual temperature, mean diurnal temperature, and 
# annual precipitation from WordClim version 2.10

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

require("dplyr")
require("raster")
require("terra")
require("geodata")

# Input dataframe 
traits <- read.csv("Fecundity_Data_.csv", head = T, sep = ",", fill = T) # Full data frame 

# Subset full data to get only lat long 
enviro <- traits %>% 
  dplyr::select(interactor1, latitude, longitude)

# Download worlclim data 
worldclim_data <- geodata::worldclim_global(var = 'bio', res = 10, path = tempdir())

# Set points 
coords <- data.frame(enviro$latitude, enviro$longitude)
coords$latitude <- coords$enviro.latitude
coords$longitude <- coords$enviro.longitude

points <- vect(enviro, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")

# Extract the variables

  # BIO1 variable (mean annual temperature)
  MAT_values <- extract(worldclim_data$wc2.1_10m_bio_1, points)
  
  # BIO2 variable (mean diurnal range)
  MDR_values <- extract(worldclim_data$wc2.1_10m_bio_2, points)
  
  # BIO4 variable (temperature seasonality)
  TS_values <- extract(worldclim_data$wc2.1_10m_bio_4, points)
  
  # BIO12 variable (annual precipitation)
  AP_values <- extract(worldclim_data$wc2.1_10m_bio_12, points)
  
  # BIO15 variable (precipitation seasonality)
  PS_values <- extract(worldclim_data$wc2.1_10m_bio_15, points)
  
# Combine the coordinates and extracted temperature data
environment <- cbind(enviro, MAT_values, MDR_values, TS_values, AP_values, PS_values)
environment$mean_annual_temp <- environment$wc2.1_10m_bio_1
environment$mean_diurnal_range <- environment$wc2.1_10m_bio_2
environment$temp_seasonality <- environment$wc2.1_10m_bio_4
environment$annual_precip <- environment$wc2.1_10m_bio_12
environment$precip_seasonality <- environment$wc2.1_10m_bio_15

environment <- environment %>% 
  dplyr::select(-c(wc2.1_10m_bio_1, wc2.1_10m_bio_2, wc2.1_10m_bio_4, 
                   wc2.1_10m_bio_12, wc2.1_10m_bio_15, ID)) %>% 
  rename(species = interactor1)

write.csv(environment, "Environment_Variables.csv")
