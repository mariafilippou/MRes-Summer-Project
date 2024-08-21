### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 2 - Calculating Wet:Dry Mass Ratios 

# This script calculates clade-specific wet-to-dry mass ratios to convert wet mass
# to dry mass 

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Input dataframe 
mass <- read.csv("wet_dry_conversion_data.csv", head = T, sep = ",", fill = T)

# Load necessary library
library(dplyr)

# Convert wet mass to mg 
mass$Wet <- mass$Wet * 1000

# Create a function to calculate the WD ratios for species, genera, etc. for conversions
calculate_WD_ratio <- function(data) {
  # Calculate Wet / Dry ratio 
  data <- data %>% 
    mutate(WD_ratio = Wet / Dry)
  # Calculate the Wet / Dry ratio for species
  data <- data %>% 
    group_by(Species) %>%
    mutate(species_WD_ratio = mean(WD_ratio, na.rm = T)) %>%
    ungroup()
  # Calculate the Wet / Dry ratio for genus
  data <- data %>%
    group_by(Genus) %>% 
    mutate(genus_WD_ratio = mean(species_WD_ratio, na.rm = T)) %>% 
    ungroup() 
  # Calculate the Wet / Dry ratio for family 
  data <- data %>%
    group_by(Family) %>% 
    mutate(family_WD_ratio = mean(genus_WD_ratio, na.rm = T)) %>% 
    ungroup() 
  # Calculate the Wet / Dry ratio for order 
  data <- data %>%
    group_by(Order) %>% 
    mutate(order_WD_ratio = mean(family_WD_ratio, na.rm = T)) %>% 
    ungroup() 
}

# Extract genus from species column
mass$Genus <- sub("^(\\w+).*", "\\1", mass$Species)

# Apply function to calculate taxon-specific ratios 
mass2 <- calculate_WD_ratio(mass)

# Subset 
  # Species
species_ratio <- mass2 %>% select(Species, species_WD_ratio)
  # Genus
genus_ratio <- mass2 %>% select(Genus, genus_WD_ratio)
  # Family 
family_ratio <- mass2 %>% select(Family, family_WD_ratio)
  # Order
order_ratio <- mass2 %>% select(Order, order_WD_ratio)

# Upload other data to convert 
insect_mass <- read.csv("Insect_Masses2.csv", head = T, sep = ",", fill = T)
insect <- subset(insect_mass, Class == "Insecta")

# Merge datasets on taxonomy columns
  # Species 
species_mass <- left_join(insect, species_ratio, by = "Species")
genus_mass <- left_join(species_mass, genus_ratio, by = "Genus")
family_mass <- left_join(genus_mass, family_ratio, by = "Family")
complete_mass <- left_join(family_mass, order_ratio, by = "Order")

# Calculate adjusted_wet_value and adjusted_wet for the entire dataset
convert_mass <- complete_mass %>%
  mutate(
    Adjusted_Wet_Value = case_when(
      Trait == "mass" & Body_State_at_Measurement %in% c("Dry", "NA") & !is.na(species_WD_ratio) ~ Final_Value * species_WD_ratio,
      Trait == "mass" & Body_State_at_Measurement %in% c("Dry", "NA") & is.na(species_WD_ratio) & !is.na(genus_WD_ratio) ~ Final_Value * genus_WD_ratio,
      Trait == "mass" & Body_State_at_Measurement %in% c("Dry", "NA") & is.na(species_WD_ratio) & is.na(genus_WD_ratio) & !is.na(family_WD_ratio) ~ Final_Value * family_WD_ratio,
      Trait == "mass" & Body_State_at_Measurement %in% c("Dry", "NA") & is.na(species_WD_ratio) & is.na(genus_WD_ratio) & is.na(family_WD_ratio) & !is.na(order_WD_ratio) ~ Final_Value * order_WD_ratio,
      TRUE ~ Final_Value  # If no conversion is needed, keep the original value
    ),
    Adjusted_Wet = if_else(
      Trait == "mass" & Body_State_at_Measurement %in% c("Dry", "NA") & 
        (!is.na(species_WD_ratio) | !is.na(genus_WD_ratio) | !is.na(family_WD_ratio) | !is.na(order_WD_ratio)),
      "yes",
      "no"
    )
  ) %>%
  select(-species_WD_ratio, -genus_WD_ratio, -family_WD_ratio, -order_WD_ratio)  # Remove the ratio columns, don't need them anymore

# Write to CSV
write.csv(convert_mass, 'adjusted_insect_mass.csv')
