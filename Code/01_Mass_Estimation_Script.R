### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 1 - Estimating Masses for Species 

# This script standardises and esimtates masses for arthropod species and was written 
# by George Kalogiannis (MRes Computational Methods in Ecology, Evolution & Conservation, 2024)
# for a GitHub repository

# Load Packages
library(readxl)
library(tidyverse)
library(taxizedb)
library(parallel)

# Body Mass Data Loading
animaltraits = read.csv("../data/mass_data/animaltraits.csv") # AnimalTraits Data https://animaltraits.org/
coleoptera = read_xlsx("../data/mass_data/S1_File.xlsx", sheet = 1) # Canadian Coleoptera Data https://doi.org/10.7717/peerj.12799/supp-1
kuhsel = read.csv("../data/mass_data/Kuhsel_2016_Insect_data.csv") # Surface Area-Volume Data (Contains body mass) https://doi.org/10.1111/1744-7917.12362
orthoptera = read.csv("../data/mass_data/orthoptera_mass.csv", sep = "\t") # Figure 1 mass data on orthoptera species https://doi.org/10.1665/1082-6467-17.2.301
bruckner = read.csv("../data/mass_data/bruckner_2017_arthropods.csv") # Pollinator data https://doi.org/10.1371/journal.pone.0175001
kinsella = read.csv("../data/mass_data/KinsellaBiomass.csv") # Predicted Lepidoptera data https://doi.org/10.1002%2Fece3.6546 
horne = read.csv("../data/mass_data/Horne_2018_Arthropods.csv") # Arthropod data collation https://doi.org/10.1111/1365-2435.13031
load("../data/mass_data/kendall_2019_pollimetry_dataset.rdata") # https://doi.org/10.1002/ece3.4835
leiva = read.csv("../data/mass_data/Leiva_et_al_DataBase_PhilTransB_update.csv") # Ectotherm data collation and critical thermal limits https://doi.org/10.1098/rstb.2019.0035
woodcock = read.csv("../data/mass_data/woodcock.csv") # https://doi.org/10.5285/78408af3-452f-41af-95f3-ffc13b05c232
hagge = read.csv("../data/mass_data/haggesaproxyl.csv") # https://doi.org/10.5061/dryad.2fqz612p3

# Body Size Data Loading
gtdrift = read.csv("../data/mass_data/GTDrift_life_history_traits.tab", sep = "\t", encoding="UTF-8") # Animal Genetic Drift database (pre-print) https://doi.org/10.1101/2024.01.23.576799
opdb = read.csv("../data/size_data/opdb.csv") # Odonate Phenotypic Database https://doi.org/10.1038/s41597-019-0318-9
brose = read.csv("../data/size_data/bodysizes_2008.txt", sep = "\t", encoding="UTF-8") # Consumer Body Sizes https://doi.org/10.1890/05-0379
arthropodtraits = read.csv("../data/size_data/ArthropodSpeciesTraits.txt", sep = "\t", encoding="UTF-8") # Arthropod Sizes https://doi.org/10.1038/sdata.2015.13
mwelling = read_xlsx("../data/size_data/European_&_Maghreb_Butterfly_Trait_data_v1.2.xlsx", sheet = 1); columns = names(mwelling)[4:224]; mwelling[mwelling == "NA"] <- NA; mwelling[columns] <- lapply(mwelling[columns], as.numeric); mwelling$mean_row <- rowMeans(mwelling[, c("FoL_var_male_average", "FoL_var_female_average", "FoL_HR_average", "FoL_Ten_average")], na.rm = TRUE) # https://doi.org/10.1038/s41597-020-00697-7
leptraits <- read.csv("../data/size_data/leptraits.csv"); columns <- c("WS_U_Fem", "WS_U_Mal", "WS_U"); leptraits$Row_Means <- rowMeans(leptraits[, columns], na.rm = TRUE); leptraits[leptraits == "NaN"] = NA
gillespie = read_xlsx("../data/size_data/gillespie2017beetles.xlsx"); colnames(gillespie) = gillespie[2,]; gillespie = gillespie[-c(1:2),] # https://doi.org/10.1002%2Fece3.2732


# Paul Huxley Data
white = read.csv("../data/paul_huxley_data/arthropod-data-from-White-et-al-Metabolic-Scaling.csv")
ehnes = read.csv("../data/paul_huxley_data/Ehnes.csv")
meehan = read.csv("../data/paul_huxley_data/Meehan.csv")
dillon = read.csv("../data/paul_huxley_data/dillon.csv")

# Diorhabda carinulata mass
diorhabda <- data.frame(Species = "Diorhabda carinulata", Value = 0.01089, Trait = "mass", Metric = "g", State = "Live", Estimate = "No", doi = "https://hdl.handle.net/10539/25023")

# Aphis craccivora (body length in mm)
aphis <- data.frame(Species = "Aphis craccivora", Value = 2.10, Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "https://doi.org/10.1007/s11756-022-01216-2")

# Bemisia tabacci (body length in um)
bemisia <- data.frame(Species = c("Bemisia tabaci", "Bemisia tabaci", "Bemisia tabaci", "Bemisia tabaci", "Bemisia tabaci", "Bemisia tabaci"), 
                      Value = c(855.35, 866.46, 742.96, 744.60, 748.52, 738.20), 
                      Trait = c("size", "size", "size", "size", "size", "size"), 
                      Metric = c("um", "um", "um", "um", "um", "um"), 
                      State = c("Live", "Live", "Live", "Live", "Live", "Live"), 
                      Estimate = c("No", "No", "No", "No", "No", "No"), 
                      doi = c("10.1016/S2095-3119(13)60303-2", "10.1016/S2095-3119(13)60303-2", "10.1016/S2095-3119(13)60303-2", 
                              "10.1016/S2095-3119(13)60303-2", "10.1016/S2095-3119(13)60303-2", "10.1016/S2095-3119(13)60303-2"))

# Brevicoryne brassicae (body length in mm)
brevicoryne <- data.frame(Species = "Brevicoryne brassicae", Value = 1.913, Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "https://doi.org/10.1590/1806-9665-RBENT-2023-0045")

# Drepanosiphum acerinum (body length mm)
drepanosiphum_ace <- data.frame(Species = "Drepanosiphum acerinum", Value = 2.9 , Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "https://doi.org/10.1016/j.jcz.2016.04.005")

# Drepanosiphum platanoids (body length mm)
drepanosiphum_plat <- data.frame(Species = "Drepanosiphum platanoids", Value = 3.52, Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "https://doi.org/10.1016/j.jcz.2016.04.005")

# Hyalopterus pruni (body length in mm)
hyalopterus <- data.frame(Species = "Hyalopterus pruni", Value = 2.05 , Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "10.33642/jotaf.1286792")

# Melanaphis saccahri (body length in um)
melanaphis <- data.frame(Species = "Melanaphis sacchari", Value = 1338, Trait = "size", Metric = "um", State = "Live", Estimate = "No", doi = "https://doi.org/10.1371/journal.pone.0241881")

# Sipha flava (body length in um)
sipha <- data.frame(Species = "Sipha flava", Value = 17.1, Trait = "mass", Metric = "mg", State = "Live", Estimate = "No", doi = "https://doi.org/10.1603/0046-225X-33.3.546")

# Sitobion avenae (body length in mm)
sitobion <- data.frame(Species = "Sitobion avenae", Value = 2.3, Trait = "size", Metric = "mm", State = "Live", Estimate = "No", doi = "http://dx.doi.org/10.1093/biolinnean/blz194")

# Genome Data Loading
genomes = read.csv("../data/genome_data/speciessummary.csv"); genomes[1] = NULL # Genome summary data and change column name

# Data Standardisation & Organisation Functions
check_data = function(data){
  data = data %>%
    filter(complete.cases(data$Species),
           !grepl("[0-9]|(sp|spp)$|(sp.|spp.)", data$Species),
           Value > 0)
  data$Species <- str_replace_all(data$Species, "\\s*\\([^\\)]+\\)", "")
  return(data)
}
bodymass = function(file, species, value, trait, metric, state, estimate, doi){
  if (!state %in% c("Dry", "Live", "NA")){
    stop("Invalid collection type.")
  }
  if (!estimate %in% c("Yes", "No", "NA")){
    stop("Invalid estimate type.")
  }
  if (!metric %in% c("mg", "g", "kg", "cm", "mm", "um")){
    stop("Invalid metric.")
  }
  if (!trait %in% c("mass", "size")){
    stop("Invalid trait")
  }
  tmp  = data.frame(Species = species, # Combine inputs into data frame
                      Trait = rep(trait, length(species)),
                      Value = as.numeric(value),
                      Metric = rep(metric, length(species)),
                      Collection = rep(state, length(species)), 
                      Estimate = rep(estimate, length(species)),
                      doi = rep(doi, length(species))
                      ) 
  tmp$Species = sub("_", " ", tmp$Species)
  tmp = na.omit(check_data(data = tmp))
  output = rbind(file, tmp)
  return (output)
}
adjust_values <- function(data) {
  data %>% mutate(
    Adjusted_Value = 
      case_when(
        Original_Metric == "kg" ~ 1000000 * Original_Value,
        Original_Metric == "g" ~ 1000 * Original_Value,
        Original_Metric == "cm" ~ 10 * Original_Value,
        Original_Metric == "m" ~ 1000 * Original_Value,
        Original_Metric == "mm" ~ Original_Value,
        Original_Metric == "um" ~ Original_Value/1000,
        TRUE ~ NA
      ),
    Adjusted_Metric = 
      case_when(
        Original_Metric %in% c("kg", "g") ~ "mg",
        Original_Metric %in% c("cm", "m", "mm", "um") ~ "mm",
        TRUE ~ NA
      ),
  Converted_Value = 
    case_when(
      Order == "Lepidoptera" & Trait == "size" ~ -2.137 + 2.772 * Adjusted_Value,
      Order == "Odonata" & Suborder == "Zygoptera" & Trait == "size" ~ 10^(-0.854) * Adjusted_Value^1.855,
      Order == "Odonata" & Suborder == "Epiprocta" & Trait == "size" ~ 10^(-0.979) * Adjusted_Value^2.218,
      Order == "Coleoptera" & Trait == "size" ~ 0.0389 * Adjusted_Value^2.492,
      Order == "Orthoptera" & Trait == "size" ~ 0.0488 * Adjusted_Value^2.515,
      Suborder == "Heteroptera" & Trait == "size" ~ 0.0084 * Adjusted_Value^3.075,
      (Suborder == "Auchenorrhyncha" | Suborder == "Sternorrhyncha") & Trait == "size" ~ 0.0594 * Adjusted_Value^2.225,
      Order == "Blattodea" & Trait == "size" ~ 0.0494 * Adjusted_Value^2.344,
      Order == "Hymenoptera" & Trait == "size" ~ 0.0138 * Adjusted_Value^2.696,
      Order == "Diptera" & Trait == "size" ~ 0.0414 * Adjusted_Value^2.213,
      Order == "Ephemeroptera" & Trait == "size" ~ 0.007 * Adjusted_Value^2.88,
      Order == "Thysanoptera" & Trait == "size" ~ 0.0071 * Adjusted_Value^2.537,
      TRUE ~ Adjusted_Value  # Replace NA with Adjusted_Value
    ),
  Converted_Metric = 
    case_when(
      TRUE ~ "mg"
    ),
  Conversion_Citation = 
    case_when(
      Order == "Lepidoptera" & Trait == "size" ~ "Garcia_Barros_2015",
      Order == "Odonata" & (Suborder == "Zygoptera" | Suborder == "Epiprocta") & Trait == "size" ~ "Aromaa_et_al_2019",
      (Order == "Coleoptera" | Suborder == "Heteroptera" | Suborder == "Auchenorrhyncha" | Suborder == "Sternorrhyncha" | Order == "Hymenoptera" | Order == "Diptera" ) & Trait == "size" ~ "Sample_et_al_1993",
      Order == "Orthoptera" & Trait == "size" ~ "Rogers_et_al_1977",
      (Order == "Blattodea" | Order == "Thysanoptera") & Trait == "size" ~ "Hodar_1996",
      Order == "Ephemeroptera" & Trait == "size" ~ "Smock_1980",
      TRUE ~ NA_character_ # Keep NA in column where conversion was not applied
    )
  ) %>% select(doi, Class, Order, Suborder, Family, Genus, Species, Accession, Level, Trait, Collection, Estimate, Value, Metric, Adjusted_Value, Adjusted_Metric, Converted_Value, Converted_Metric, Conversion_Citation)
}

# Data entry
file = c()
file = bodymass(file, coleoptera$Species, coleoptera$`Mass (mg)`, "mass", "mg", "Live", "Yes", "https://doi.org/10.7717/peerj.12799/supp-1")
# file = bodymass(file, animaltraits$species[animaltraits$class == "Insecta"], animaltraits$body.mass[animaltraits$class == "Insecta"], "mass","kg", "NA", "No", "https://animaltraits.org/") # Excluded due to poor data origins (wikipedia etc.)
file = bodymass(file, kuhsel$Species, kuhsel$Dry.mass..mg., "mass","mg", "Dry", "No", "https://doi.org/10.1111/1744-7917.12362")
file = bodymass(file, orthoptera$Species, orthoptera$Live.Weight..g., "mass","g", "Live", "No", "https://doi.org/10.1665/1082-6467-17.2.301")
file = bodymass(file, bruckner$Species, bruckner$body.mass, "mass","mg", "Dry", "NA", "https://doi.org/10.1371/journal.pone.0175001")
file = bodymass(file, kinsella$SPECIES, kinsella$PRED_DRY_MASS, "mass","mg", "Dry", "Yes", "https://doi.org/10.1002%2Fece3.6546")
file = bodymass(file, horne$Species, horne$Dry.Mass..mg., "mass","mg", "Dry", "No", "https://doi.org/10.1111/1365-2435.13031")
file = bodymass(file, pollimetry_dataset$Species, pollimetry_dataset$Spec.wgt, "mass","mg", "Dry", "NA", "https://doi.org/10.1002/ece3.4835")
file = bodymass(file, leiva$species[leiva$class == "Insecta"], leiva$body_mass[leiva$class == "Insecta"], "mass","mg", "NA", "NA", "https://doi.org/10.1098/rstb.2019.0035")
# file = bodymass(file, woodcock$Species, woodcock$Mass, "mass", "mg", "Dry", "No", "https://doi.org/10.5285/78408af3-452f-41af-95f3-ffc13b05c232") # Data differs significantly within species - unsure of quality
# file = bodymass(file, brose$Taxonomy.consumer[brose$Metabolic.category.consumer == "invertebrate"], brose$Mean.mass..g..consumer[brose$Metabolic.category.consumer == "invertebrate"], "mass", "g", "Live", "Yes", "https://doi.org/10.1890/05-0379")
# file = bodymass(file, gtdrift$species[gtdrift$life_history_traits == "weight_kg"], gtdrift$value[gtdrift$life_history_traits == "weight_kg"], "mass","kg", "NA", "NA", "https://doi.org/10.1101/2024.01.23.576799") # Excluded due to poor data origins (wikipedia etc.)

# file = bodymass(file, gtdrift$species[gtdrift$life_history_traits == "length_cm"], gtdrift$value[gtdrift$life_history_traits == "length_cm"], "size","cm", "NA", "NA", "https://doi.org/10.1101/2024.01.23.576799") # Excluded due to poor data origins (wikipedia etc.)
file = bodymass(file, opdb$GenusSpecies, opdb$body_lengths, "size", "mm", "NA", "Yes", "https://doi.org/10.1038/s41597-019-0318-9")
file = bodymass(file, mwelling$`Taxa name`, mwelling$mean_row, "size", "mm", "NA", "Yes", "https://doi.org/10.1038/s41597-020-00697-7")
file = bodymass(file, leptraits$Species, leptraits$FW_U, "size", "cm", "NA", "Yes", "https://doi.org/10.1038/s41597-022-01473-5")
file = bodymass(file, hagge$species, hagge$body_length, "size", "mm", "NA", "No", "https://doi.org/10.5061/dryad.2fqz612p3")
file = bodymass(file, gillespie$Species, gillespie$`Mean body size (mm)`, "size", "mm", "NA", "No", "https://doi.org/10.1002%2Fece3.2732")
# file = bodymass(file, arthropodtraits$SpeciesID, arthropodtraits$Body_Size, "size", "mm", "NA", "NA", "https://doi.org/10.1038/sdata.2015.13") # Excluded due to poor data origins (wikipedia etc.)

file = bodymass(file, white$Species, white$max.mass.g, "mass", "g", "Live", "No", "https://doi.org/10.1126/science.abm7649")
file = bodymass(file, ehnes$Species, ehnes$weight..mg., "mass", "mg", "Live", "No", "https://doi.org/10.1111/j.1461-0248.2011.01660.x")
file = bodymass(file, meehan$Species, meehan$Live.Mass..mg., "mass", "mg", "Live", "No", "https://doi.org/10.1086/505997")
file = bodymass(file, dillon$X.order.species, dillon$dry.mass.mg., "mass", "mg", "Dry", "No", "https://doi.org/10.1371/journal.pone.0084308")

file = bodymass(file, diorhabda$Species, diorhabda$Value, diorhabda$Trait, diorhabda$Metric, diorhabda$State, diorhabda$Estimate, diorhabda$doi)
file = bodymass(file, aphis$Species, aphis$Value, aphis$Trait, aphis$Metric, aphis$State, aphis$Estimate, aphis$doi)
file = bodymass(file, bemisia$Species, bemisia$Value, "size", "um", "Live", "No", "10.1016/S2095-3119(13)60303-2")
file = bodymass(file, brevicoryne$Species, brevicoryne$Value, brevicoryne$Trait, brevicoryne$Metric, brevicoryne$State, brevicoryne$Estimate, brevicoryne$doi)
file = bodymass(file, drepanosiphum_ace$Species, drepanosiphum_ace$Value, drepanosiphum_ace$Trait, drepanosiphum_ace$Metric, drepanosiphum_ace$State, drepanosiphum_ace$Estimate, drepanosiphum_ace$doi)
file = bodymass(file, drepanosiphum_plat$Species, drepanosiphum_plat$Value, drepanosiphum_plat$Trait, drepanosiphum_plat$Metric, drepanosiphum_plat$State, drepanosiphum_plat$Estimate, drepanosiphum_plat$doi)
file = bodymass(file, hyalopterus$Species, hyalopterus$Value, hyalopterus$Trait, hyalopterus$Metric, hyalopterus$State, hyalopterus$Estimate, hyalopterus$doi)
file = bodymass(file, melanaphis$Species, melanaphis$Value, melanaphis$Trait, melanaphis$Metric, melanaphis$State, melanaphis$Estimate, melanaphis$doi)
file = bodymass(file, sipha$Species, sipha$Value, sipha$Trait, sipha$Metric, sipha$State, sipha$Estimate, sipha$doi)
file = bodymass(file, sitobion$Species, sitobion$Value, sitobion$Trait, sitobion$Metric, sitobion$State, sitobion$Estimate, sitobion$doi)

# Combine genome and body mass/size data
merged = adjust_values(merge(file, genomes, by = "Species")); length(unique(merged$Species)) # Merge genome and species data sets - this tells us which & how many species we have data for.
merged = subset(merged, Level == "Chromosome") # Keep only highest-quality genomes (Chromosome-level)

dir.create(file.path("../results"), showWarnings = FALSE) # Create an output directory relative to "code/", suppress warnings if it already exists
write.csv(merged, "../results/Insect_Masses_with_Genome_Information.csv") # output to results file


plotting <- merged %>% # Create subset to visualise dataset
  group_by(Species) %>%
  summarize(Mean_Converted_Value = mean(Converted_Value, na.rm = TRUE)) %>%
  left_join(merged, by = "Species") %>%
  distinct(Species, .keep_all = TRUE) %>%
  select(Class, Order, Suborder, Family, Genus, Species, Accession, Level, Mean_Converted_Value)

summary = plotting %>% group_by(Order) %>% summarise(n = n()) # Number of species within each insect order

ggplot(plotting, aes(x = log(Mean_Converted_Value)))+ # Plot of mass-genome data species frequency
  geom_histogram(bins = 80)+
  facet_wrap(~Order, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Log-Transformed Mass-Genome Data")

# Create output of insect mass
mass = file; colnames(mass)[c(1,7)] = c("species", "source_doi") # Create a duplicate file 
ids <- name2taxid(mass$species, out_type="summary") # Get ncbi id for taxonomy from name
classes = classification(ids$id) # output taxonomy from id

process_df <- function(df) { 
  df %>%
    t() %>%
    as_tibble() %>%
    slice(-c(2, 3)) %>%
    set_names(make.unique(as.character(df$rank))) %>%
    as.data.frame(stringsAsFactors = FALSE)
} # Function to reorder classification data
processed_data <- mclapply(classes, process_df, mc.cores = detectCores()) # Apply function using multi-core lapply (it's faster)
taxonomy <- bind_rows(processed_data) # Bind output rows

taxonomy = taxonomy[, c("class", "order", "suborder", "family", "genus", "species")] # subset taxonomy
mass = merge(taxonomy, mass, by = "species") # merge taxonomy with data
colnames(mass) = c("Species", "Class", "Order", "Suborder", "Family", "Genus", "Trait", "Original_Value", "Original_Metric", "Body_State_at_Measurement", "Estimated_from_Regression", "Source_DOI") # rename columns appropriately
adjust_values_mass <- function(data) {
  data %>% 
    mutate(
      Adjusted_Value = 
        case_when(
          Original_Metric == "kg" ~ 1000000 * Original_Value,
          Original_Metric == "g" ~ 1000 * Original_Value,
          Original_Metric == "cm" ~ 10 * Original_Value,
          Original_Metric == "m" ~ 1000 * Original_Value,
          Original_Metric == "mm" ~ Original_Value,
          Original_Metric == "um" ~ Original_Value/1000,
          TRUE ~ NA
        ),
      Adjusted_Metric = 
        case_when(
          Original_Metric %in% c("kg", "g") ~ "mg",
          Original_Metric %in% c("cm", "m", "mm", "um") ~ "mm",
          TRUE ~ NA
        ),
      Converted_Value = 
        case_when(
          Order == "Lepidoptera" & Trait == "size" ~ -2.137 + 2.772 * Adjusted_Value,
          Order == "Odonata" & Suborder == "Zygoptera" & Trait == "size" ~ 10^(-0.854) * Adjusted_Value^1.855,
          Order == "Odonata" & Suborder == "Epiprocta" & Trait == "size" ~ 10^(-0.979) * Adjusted_Value^2.218,
          Order == "Coleoptera" & Trait == "size" ~ 0.0389 * Adjusted_Value^2.492,
          Order == "Orthoptera" & Trait == "size" ~ 0.0488 * Adjusted_Value^2.515,
          Suborder == "Heteroptera" & Trait == "size" ~ 0.0084 * Adjusted_Value^3.075,
          (Suborder == "Auchenorrhyncha" | Suborder == "Sternorrhyncha") & Trait == "size" ~ 0.0594 * Adjusted_Value^2.225,
          Order == "Blattodea" & Trait == "size" ~ 0.0494 * Adjusted_Value^2.344,
          Order == "Hymenoptera" & Trait == "size" ~ 0.0138 * Adjusted_Value^2.696,
          Order == "Diptera" & Trait == "size" ~ 0.0414 * Adjusted_Value^2.213,
          Order == "Ephemeroptera" & Trait == "size" ~ 0.007 * Adjusted_Value^2.88,
          Order == "Thysanoptera" & Trait == "size" ~ 0.0071 * Adjusted_Value^2.537,
          TRUE ~ NA  # Replace NA with Adjusted_Value
        ),
      Converted_Metric = 
        case_when(!is.na(Converted_Value) ~ "mg",
          TRUE ~ NA
        ),
      Conversion_Citation = 
        case_when(
          Order == "Lepidoptera" & Trait == "size" & Adjusted_Metric == "mm" ~ "Garcia_Barros_2015",
          Order == "Odonata" & (Suborder == "Zygoptera" | Suborder == "Epiprocta") & Trait  == "size" & Adjusted_Metric == "mm" ~ "Aromaa_et_al_2019",
          (Order == "Coleoptera" | Suborder == "Heteroptera" | Suborder == "Auchenorrhyncha" | Suborder == "Sternorrhyncha" | Order == "Hymenoptera" | Order == "Diptera" ) & Trait == "size" & Adjusted_Metric == "mm" ~ "Sample_et_al_1993",
          Order == "Orthoptera" & Trait == "size" & Adjusted_Metric == "mm" ~ "Rogers_et_al_1977",
          (Order == "Blattodea" | Order == "Thysanoptera") & Trait == "size" & Adjusted_Metric == "mm" ~ "Hodar_1996",
          Order == "Ephemeroptera" & Trait == "size" & Adjusted_Metric == "mm" ~ "Smock_1980",
          TRUE ~ NA # Keep NA in column where conversion was not applied
        ),
      Final_Value = 
        case_when(
          !is.na(Converted_Value) ~ Converted_Value,
          !is.na(Adjusted_Value) ~ Adjusted_Value,
          TRUE ~ Original_Value
        ),
      Final_Metric = "mg"
    ) %>% 
    select(Source_DOI, Class, Order, Suborder, Family, Genus, Species, Trait, Body_State_at_Measurement, Estimated_from_Regression, Original_Value, Original_Metric, Adjusted_Value, Adjusted_Metric, Converted_Value, Converted_Metric, Conversion_Citation, Final_Value, Final_Metric)
} # function to adjust & convert values
mass = adjust_values_mass(mass) # apply function
mass = subset(mass, Class = Insecta)

write.csv(mass, "../results/Insect_Masses.csv") # output to results file
