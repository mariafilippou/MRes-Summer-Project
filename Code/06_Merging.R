### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 6 - Merging

# This script merges the TPC output, environmental variables, and size data into 
# one file to be used in analysis

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

require("dplyr")

# Input dataframes 
traits <- read.csv("Fecundity_Data_.csv", head = T, sep = ",", fill = T)
params <- read.csv("fecundity_TPC_parameters2.csv", head = T, sep = ",", fill = T)
enviro <- read.csv("Environment_Variables.csv", head = T, sep = ",", fill = T)
size <- read.csv("species_mass_data.csv", head = T, sep = ",", fill = T)

# Merge 

# Add a column with the count of values under the peak value at lower temperatures
traits1 <- traits %>%
  group_by(interactor1) %>%
  mutate(
    peak_rate = max(standardisedtraitvalue),
    peak_temp = interactor1temp[standardisedtraitvalue == peak_rate][1],  # Temperature at peak value
    count_below_peak = sum(standardisedtraitvalue < peak_rate & interactor1temp < peak_temp)
  ) %>%
  ungroup()

doi <- traits1 %>%
  dplyr::select(interactor1, doi, count_below_peak)

params_size <- merge(params, size, by = "species")

params_all <- merge(params_size, enviro, by = "species")

params_complete <- merge(params_all, doi, by.x = "species", by.y = "interactor1")

params_complete <- params_complete %>%
  dplyr::select(-c(X.x, X.y, X))

params_complete <- distinct(params_complete)

write.csv(params_complete, "final_fecundity_parameters2.csv")
