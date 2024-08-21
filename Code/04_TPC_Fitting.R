### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 4 - Thermal Performance Curve (TPC) fitting for maximum fecundity rate 

# This script produces TPC's for each species to be able to extract values for
# activation energy (e), niche width (breadth), and optimal temperture (Topt)

rm(list=ls())

# Load packages
require('tidyverse')
require('nls.multstart')
require('broom')
require('rTPC')
require('data.table')
require('patchwork')
require('minpack.lm')
require('boot')
require('doMC')
require('foreach')
require('ggpubr')
require('ggtext')
require('cowplot')
require('car')

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Upload athropod fecundity trait data 
df <- as_tibble(read.csv('Fecundity_Data_.csv', head = T)) 

# Prepare the data for the curves by removing NA, using easier titles, and adding a numeric curve ID for each species
dv <- df %>% rename(temp = interactor1temp, species = interactor1, rate = standardisedtraitvalue) %>%
  dplyr::select(species, temp, standardisedtraitname, rate) %>%
  filter(standardisedtraitname == "bmax", rate != 'NA') %>% 
  mutate(curve_ID = as.numeric(factor(species))) %>%
  mutate(temp = as.numeric(temp)) %>%
  arrange(curve_ID) %>% 
  filter(curve_ID != 'NA') 

dv %>% distinct(species) %>% print(n = 64)
  
# Fit TPC model for each species

start_vals <- get_start_vals(dv$temp, dv$rate, model_name = 'pawar_2018')
low_lims <-   get_lower_lims(dv$temp, dv$rate, model_name = 'pawar_2018')
upper_lims <- get_upper_lims(dv$temp, dv$rate, model_name = 'pawar_2018')

dv_fits <- nest(dv, data = c(temp, rate)) %>%
  mutate(fit = map(data, ~ nls_multstart(rate~pawar_2018(temp = temp, r_tref,e,eh,topt, tref = 15),
                                        data = .x,
                                        iter = c(3, 3, 3, 3),
                                        start_lower = start_vals - 10,
                                        start_upper = start_vals + 10,
                                        lower = low_lims,
                                        upper=upper_lims,
                                        supp_errors = 'Y',
                                        convergence_count = FALSE)))


dv_preds <- mutate(dv_fits, new_data = map(data, ~tibble(temp = seq(min(0), max(45), length.out = 1000)))) %>%
  # get rid of original data column
  dplyr::select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(fit)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  dplyr::select(curve_ID, species, standardisedtraitname, preds) %>%
  # unlist the preds list column
  unnest(preds)

glimpse(dv_preds)

ggplot(dv_preds) +
  geom_line(aes(temp, .fitted, colour = factor(curve_ID))) +  
  geom_point(aes(temp, rate), dv) +  
  facet_wrap(~species, scales = 'free_y', ncol = 3) +  # Facet the plot by species
  scale_color_manual(values = rainbow(length(unique(dv_preds$curve_ID)))) +  
  theme(legend.position = 'none', 
        axis.text = element_text(size = 12, face = "bold", family = "Times New Roman"), 
        axis.title = element_text(size = 15, face = "bold", family = "Times New Roman"), 
        title = element_text(size = 15, face = "italic", family = "Times New Roman")) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  labs(title = paste(df$species[1], sep = ""),
       y = expression(bolditalic(b[max])), x = expression(plain(paste(" Temperature (", degree, "C)"))))

  # Save as PDF and embed fonts
  ggsave("TPC.png", plot = last_plot(), dpi = 1300, width = 15, height = 21)

# Fit models
FitModel <- function(ID){
  print(paste("Starting model fit for curve ID:", ID))
  
  #browser()
  
  ID <<- ID
  df <- dv %>% filter(curve_ID == ID)
  df <<- df
  
  print(paste("Data filtered for curve ID:", ID))
  
  Model <- tryCatch({
    minpack.lm::nlsLM(rate ~ pawar_2018(temp = temp, r_tref, e, eh, topt, tref = 15),
                      data = df,
                      start = coef(dv_fits$fit[[which(dv_fits$curve_ID == ID)]]),
                      lower = get_lower_lims(df$temp, df$rate, model_name = 'pawar_2018') - 10,
                      upper = get_upper_lims(df$temp, df$rate, model_name = 'pawar_2018') + 10,
                      weights = rep(1, times = nrow(df)))
  }, error = function(error) {
    print(paste("Error fitting model for curve ID:", ID, "-", error$message))
    minpack.lm::nlsLM(rate ~ pawar_2018(temp = temp, r_tref, e, eh, topt, tref = 15),
                      data = df,
                      start = coef(dv_fits$fit[[which(dv_fits$curve_ID == ID)]]),
                      weights = rep(1, times = nrow(df)))
  })
  
  print(paste("Model fitting complete for curve ID:", ID))
  
  extra_params <- calc_params(Model) %>%
    pivot_longer(everything(), names_to = 'param', values_to = 'estimate')
  
  print(paste("Extra parameters calculated for curve ID:", ID))
  
  ci_extra_params <- tryCatch({
    Boot(Model, f = function(x) { unlist(calc_params(x)) }, labels = names(calc_params(Model)), R = 200, method = 'residual') %>%
      confint(., method = 'bca') %>%
      as.data.frame() %>%
      rename(conf_lower = 1, conf_upper = 2) %>%
      rownames_to_column(., var = 'param') %>%
      mutate(method = 'residual bootstrap')
  }, error = function(error) {
    print(paste("Bootstrapping failed for curve ID:", ID, "-", error$message))
    return(data.frame(param = character(), conf_lower = numeric(), conf_upper = numeric(), method = character()))
  })
  
  ci_extra_params <- left_join(ci_extra_params, extra_params, by = "param")
  
  print(paste("Confidence intervals calculated for curve ID:", ID))
  
  # Get other parameters
  params <- broom::tidy(Model) %>% dplyr::select(param = term, estimate)
  BootOut <- Boot(Model, method = 'residual')
  
  # Get the param Names that has multiple values
  paramName <- colnames(BootOut[[2]])[which(apply(BootOut[[2]], 2, function(x) length(unique(x)) > 1))]
  
  params_cis <- tryCatch({
    BootOut %>%
      confint(., parm = paramName, method = 'bca') %>%
      as.data.frame() %>%
      rename(conf_lower = 1, conf_upper = 2) %>%
      rownames_to_column(., var = 'param') %>%
      mutate(method = 'residual bootstrap')
  }, error = function(error) {
    print(paste("Bootstrapping for parameters failed for curve ID:", ID, "-", error$message))
    return(data.frame(param = character(), conf_lower = numeric(), conf_upper = numeric(), method = character()))
  })
  
  params_cis <- bind_rows(params_cis) %>%
    left_join(., params, by = "param") %>%
    filter(param != "topt")
  
  topt <- as_tibble(rbind(ci_extra_params, params_cis))
  topt$species <- as.character(df$species[1])
  
  print(paste("Other parameters calculated for curve ID:", ID))
  
  # Plot fit
  Boot_conf <- BootOut$t %>%
    as.data.frame() %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(0), max(45), length.out = 1000))) %>%
    ungroup() %>%
    mutate(pred = pawar_2018(temp = temp, r_tref, e, eh, topt, tref = 15))
  
  # Calculate bootstrapped confidence intervals
  boot_conf_preds <- tryCatch({
    group_by(Boot_conf, temp) %>%
      summarise(conf_lower = quantile(pred, 0.025),
                conf_upper = quantile(pred, 0.975)) %>%
      ungroup()
  }, error = function(error) {
    print(paste("Bootstrapped confidence intervals calculation failed for curve ID:", ID, "-", error$message))
    return(data.frame(temp = numeric(), conf_lower = numeric(), conf_upper = numeric()))
  })
  
  print(paste("Bootstrapped confidence intervals calculated for curve ID:", ID))
  
  # Plot
  counter <- which(dv_fits$curve_ID == ID)
  print(paste("Counter determined for curve ID:", ID, "-", counter))
  
  plotData <- filter(dv_preds, dv_preds$curve_ID == ID)
  plot <- ggplot(data = df, aes(x = temp, y = rate)) +
    geom_point(size = 1, alpha = 0.4) +
    geom_line(data = plotData, mapping = aes(x = temp, y = .fitted)) +
    geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds, fill = "#E25306", alpha = 0.3, inherit.aes = F) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"), 
          axis.title = element_text(size = 18, face = "bold", family = "Times New Roman"), 
          title = element_text(size = 18, face = "italic", family = "Times New Roman")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    labs(title = paste(df$species[1], sep = ""),
         y = expression(bolditalic(b[max])), x = expression(plain(paste(" Temperature (", degree, "C)"))))
  
  ggsave(plot, file = paste("Results/TPC_Plots/B_", ID, ".pdf", sep = ""), 
         height = 10, width = 15, units = "cm")
  
  print(paste("Plot saved for curve ID:", ID))
  
  BetaPlots[[counter]] <<- plot
  
  print(paste("Fit model complete for curve ID:", ID))
  
  return(topt)
}


BetaPlots <- vector(mode="list", length=length(unique(dv$curve_ID)))

# Run one-by-one for debugging
# ModelOut <- sapply(unique(dv$curve_ID), FitModel2)
# ModelOutDFList <- apply(ModelOut, 2, function(x) as.data.frame(do.call(cbind, x)))
# ModelOutDF <- do.call(rbind, ModelOut)
# ModelOutDF$trait <- "fecundity rate"


# Run in parallel
doMC::registerDoMC(cores = detectCores())
ModelOutList <- foreach(ID = unique(dv$curve_ID)) %dopar%{ FitModel(ID)}
ModelOutDF <- do.call(rbind, ModelOutList)
ModelOutDF$trait <- "fecundity rate"
write.csv(ModelOutDF, "~/Desktop/Masters/Summer Project/Data/fecundity_TPC_parameters2.csv")


###

# Load extrafont package
if (!require("extrafont")) install.packages("extrafont", dependencies = TRUE)
library(extrafont)
font_import(pattern = "Times New Roman", prompt = FALSE)
loadfonts(device = "pdf")

### Plots for introduction 

# Thermal sensitivity chnages with Ea gradient 
species_list <- c("Phenacoccus solenopsis", "Bemisia argentifolii")
df_filtered <- dv[dv$species %in% species_list, ]
dv_filtered <- dv_preds[dv_preds$species %in% species_list, ]

species_colors <- c("Phenacoccus solenopsis" = "#C22C2C",
                    "Bemisia argentifolii" = "darkorange")

sensitivity_plot <- ggplot(df_filtered, aes(x = temp, y = rate, colour = species)) +
  geom_line(data = dv_filtered, mapping = aes(x = temp, y = .fitted), size = 1.25) +
  scale_color_manual(values = species_colors) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_blank(),
        legend.position = "none", 
        axis.title.x = element_text(size=25, face="bold", family = "Times New Roman"), # Increase size and make x-axis title bold
        axis.title.y = element_text(size=25, face="bold", family = "Times New Roman")) +
  labs(x = "Temperature (°C)", y = "Trait Performance") +
  geom_segment(aes(x = 17.9, y = 4.5, xend = 18.9, yend = 6.5),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "darkorange", size = 0.75) +
  geom_segment(aes(x = 21.4, y = 4.5, xend = 23.2, yend = 6.5),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "#C22C2C", size = 0.75) +
  geom_text(x = 16, y = 5.5, label = expression(italic(E[a])), color = "darkorange", size = 8, fontface = "bold", family = "Times New Roman") +
  geom_text(x = 25, y = 5.0, label = expression(italic(E[a])), color = "#C22C2C", size = 8, fontface = "bold", family = "Times New Roman")
sensitivity_plot

# TPC functions
df_ <- subset(df_filtered, species == "Phenacoccus solenopsis")
dv_ <- subset(dv_filtered, species == "Phenacoccus solenopsis")

dv_adapt <- dv_ %>% mutate(fitted_adapt = .fitted * 1.5)

TPC_plot <- ggplot(dv_, aes(x = temp, y = rate)) +
  geom_line(data = dv_, mapping = aes(x = temp, y = .fitted), size = 1.25) +
  scale_color_manual(values = species_colors) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_blank(),
        legend.position = "none", 
        axis.title.x = element_text(size=25, face="bold", family = "Times New Roman"), # Increase size and make x-axis title bold
        axis.title.y = element_text(size=25, face="bold", family = "Times New Roman")) +
  labs(x = "Temperature (°C)", y = "Trait Performance") +
  geom_segment(aes(x = 21.0, y = 4.5, xend = 23.7, yend = 7.5),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "darkorange", size = 0.75) +
  geom_segment(aes(x = 29.1, y = 12.45, xend = 29.1, yend = 0.3),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "#C22C2C", size = 0.75, lty = 1) +
  geom_segment(aes(x = 24.0, y = 6.1, xend = 29.0, yend = 6.1),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "#42A5F5", size = 0.75, lty = 1) +
  geom_segment(aes(x = 29.0, y = 6.1, xend = 24.0, yend = 6.1),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "#42A5F5", size = 0.75, lty = 1) +
  geom_segment(aes(x = 24.4, y = 6.6, xend = 32.3, yend = 6.6),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "blue3", size = 0.75, lty = 1) +
  geom_segment(aes(x = 32.3, y = 6.6, xend = 24.4, yend = 6.6),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "blue3", size = 0.75, lty = 1) +
  geom_text(x = 19.5, y = 6, label = expression(bold(italic(E[a]))), color = "darkorange", size = 8, fontface = "bold", family = "Times New Roman") +
  geom_text(x = 28.6, y = -0.0, label = expression(bold(italic(T[opt]))), color = "#C22C2C", size = 8, fontface = "bold", family = "Times New Roman") +
  geom_text(x = 26.0, y = 5, label = expression(bold(italic(W[op]))), color = "#42A5F5", size = 7, fontface = "bold", family = "Times New Roman") +
  geom_text(x = 36.0, y = 6.6, label = expression(bold(italic(W[full]))), color = "blue3", size = 7, fontface = "bold", family = "Times New Roman") 
TPC_plot

# Thermal adaptability
dv_adapt <- dv_ %>% mutate(fitted_adapt = .fitted * 1.5)

adapt_plot <- ggplot(dv_adapt, aes(x = temp, y = rate)) +
  geom_line(data = dv_adapt, mapping = aes(x = temp, y = .fitted), size = 1.25, colour = "darkorange") +
  geom_line(data = dv_adapt, mapping = aes(x = temp, y = fitted_adapt), size = 1.25, colour = "#C22C2C") +
  scale_color_manual(values = species_colors) +
  scale_x_continuous(limits = c(15, 40)) + 
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_blank(),
        legend.position = "none", 
        axis.title.x = element_text(size=25, face="bold", family = "Times New Roman"), # Increase size and make x-axis title bold
        axis.title.y = element_text(size=25, face="bold", family = "Times New Roman")) +
  labs(x = "Temperature (°C)", y = "Trait Performance") +
  geom_segment(aes(x = 29.1, y = 18.65, xend = 29.1, yend = 12.6),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black", size = 0.75, lty = 1) +
  geom_segment(aes(x = 29.1, y = 12.6, xend = 29.1, yend = 18.65),
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black", size = 0.75, lty = 1) +
  geom_text(x = 29.1, y = 10.5, label = expression(bold(italic(B[pk]))), color = "darkorange", size = 8, fontface = "bold", family = "Times New Roman") +
  geom_text(x = 32.0, y = 18.0, label = expression(bold(italic(B[pk]))), color = "#C22C2C", size = 8, fontface = "bold", family = "Times New Roman") 
adapt_plot


plot_grid(TPC_plot, sensitivity_plot, adapt_plot, labels = c("a)", "b)", "c)"), label_size = 20, nrow = 1,
          label_fontfamily = "Times New Roman")

# Save as PDF and embed fonts
ggsave("plot.png", plot = last_plot(), dpi = 2000, width = 15, height = 5)

