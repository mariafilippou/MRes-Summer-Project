## Effect of size 

rm(list=ls())

require("dplyr")

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

species <- read.csv("species_size.csv", head = T, sep = ",", fill = T)
size_means <- read.csv("sizeMeans.csv")
mass_all <- read.csv("adjusted_insect_mass.csv", head = T, sep = ",", fill = T)

# Merge
species <- species %>% select(species)

size_means <- size_means %>% rename(species = interactor1, mean_mass = avg)

mass <- mass_all %>%
  select(Species, Adjusted_Wet_Value) %>%
  group_by(Species) %>%
  mutate(mean_mass = mean(Adjusted_Wet_Value)) %>%
  select(Species, mean_mass) %>%
  rename(species = Species)

mass <- distinct(mass)

species_mass <- merge(size_means, species, by = "species", all.y = T)

all_mass <- left_join(species_mass, mass, by = "species", 
                      suffix = c("", ".all"))

all_mass <- all_mass %>% 
  mutate(mean_mass = coalesce(mean_mass, mean_mass.all)) %>%
  select(-mean_mass.all)

species_mass_data <- distinct(all_mass)

species_mass_data <- species_mass_data %>%
  select(species, mean_mass) %>%
  mutate(units = "mg")

write.csv(species_mass_data, "species_mass_data.csv")

