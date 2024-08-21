### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 9 - Calculating Niche Width

# This script calculates niche width and its associated upper and lower estimates
# from the TPC outputs 

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Install packages
require('dplyr')
require('ggplot2')

# Upload data 
params <- read.csv('final_fecundity_parameters2.csv', head = T)
conf_data <- read.csv('Results/Combined_Dataset.csv', head = T)

# Add Topt to data

# Take Topt from the parameters and merge with the df
Topt <- params %>% filter(param == "topt")

Topt <- Topt %>%
  rename(Topt = estimate) %>%
  dplyr::select(species, Topt)

# Merge with fecundity data 
dv1 <- merge(conf_data, Topt, by = 'species')

# Remove all temps above Topt for each species 
dv1 <- dv1 %>%
  group_by(species) %>%
  filter(Topt >= temp) %>% 
  ungroup()

# Mutate to find B_pk 
dv2 <- dv1 %>%
  group_by(species) %>%
  mutate(B_pk = max(predicted_rate, na.rm = TRUE)) %>%
  ungroup()

# Mutate to find B_50
dv3 <- dv2 %>%
  group_by(species) %>%
  mutate(B_50 = B_pk/2) %>%
  ungroup()

# Mutate to find T_50
dv4 <- dv3 %>%
  group_by(species) %>%
  mutate(T_50 = temp[which.min(abs(predicted_rate - B_50))]) %>%
  ungroup()

# Mutate to find T_lower
dv5 <- dv4 %>%
  group_by(species) %>%
  mutate(T_lower = temp[which.min(abs(conf_lower - B_50))]) %>%
  ungroup()

# Mutate to find T_upper
dv6 <- dv5 %>%
  group_by(species) %>%
  mutate(T_upper = temp[which.min(abs(conf_upper - B_50))]) %>%
  ungroup()

# Calculate niche_width
dv7 <- dv6 %>%
  group_by(species) %>%
  mutate(niche_width = Topt - T_50) %>%
  ungroup()

# Calculate niche_lower
dv8 <- dv7 %>%
  group_by(species) %>%
  mutate(niche_lower = Topt - T_lower) %>%
  ungroup()

# Calculate niche_upper
dv9 <- dv8 %>%
  group_by(species) %>%
  mutate(niche_upper = Topt - T_upper) %>%
  ungroup()

# Remove unneccesary columns
niche_data <- dv9 %>%
  dplyr::select(species, niche_width, niche_lower, niche_upper)

# Remove repeat rows
niche_data <- distinct(niche_data)

# Merge and save 
niche_data <- merge(params, niche_data, by = 'species')

write.csv(niche_data, "final_parameters_with_niche_width.csv")

