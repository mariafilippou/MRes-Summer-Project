### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 11 - Modelling Niche Width against niche width 

# This script produces models to see the relationship between Ea and niche width

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Install packages
require('dplyr')
require('ggplot2')
require('RColorBrewer')

# Upload data 
params <- read.csv('final_parameters_with_niche_width.csv', head = T)

# Remove curves with poor fits
poor_fits <- c("Callosobruchus analis", "Callosobruchus chinensis", 
               "Callosobruchus maculatus", "Callosobruchus rhodesianus",
               "Plutella xylostella")

op_niche_width <- params %>% 
  filter(!(species %in% poor_fits))

# Remove any curves that have < 3 data points below the peak
op_niche_width <- op_niche_width[op_niche_width$count_below_peak >= 3, ]

# Remove any NA
op_niche_width <- op_niche_width %>% 
  filter(!is.na(species))

## Relationship with activation energy

op_niche_width <- op_niche_width %>% filter(param == "e")

# Remove any values above 2.5
op_niche_width <- op_niche_width[op_niche_width$estimate <= 2.5, ]

# Calculate the weights as the inverse of the CI width 
op_niche_width$Ea_weights <- 1 / ((op_niche_width$conf_upper - op_niche_width$conf_lower)^2)

# Visualise
plot(op_niche_width$niche_width, op_niche_width$estimate)

# Simple linear model
set.seed(123)
model1 <- lm(estimate ~ log10(niche_width), data = op_niche_width)
summary(model1)

# Weighted linear model 
set.seed(123)
model2 <- lm(estimate ~ log10(niche_width), data = op_niche_width, weights = Ea_weights)
summary(model2)

# Simple quadratic model
set.seed(123)
model3 <- lm(estimate ~  poly(log10(niche_width), 2, raw = T), data = op_niche_width)
summary(model3)

# Weighted quadratic model
set.seed(123)
model4 <- lm(estimate ~  poly(log10(niche_width), 2, raw = T), data = op_niche_width, weights = Ea_weights)
summary(model4)

# Compare the models to determine the best one  

  # List of models
  models <- list(
    model1, 
    model2, 
    model3, 
    model4
  )
  
  # Function to extract comparison metrics
  compare_models <- function(models) {
    results <- data.frame(
      Model = character(),
      R_squared = numeric(),
      Adj_R_squared = numeric(),
      AIC = numeric(),
      BIC = numeric(),
      RMSE = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(models)) {
      model <- models[[i]]
      r_squared <- summary(model)$r.squared
      adj_r_squared <- summary(model)$adj.r.squared
      aic <- AIC(model)
      bic <- BIC(model)
      rmse <- sqrt(mean(residuals(model)^2))
      
      results[i, ] <- c(paste("Model", i), r_squared, adj_r_squared, aic, bic, rmse)
    }
    
    return(results)
  }
  
  # Compare models
  model_comparison <- compare_models(models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)

# Create a data frame 
plot_onw <- op_niche_width %>%
  mutate(
    log_niche_width = log10(niche_width),
    fitted_values = predict(model4, newdata = op_niche_width)
  )

plot_onw <- plot_onw %>%
  mutate(
    residuals = estimate - fitted_values
  )

# Estimate the sd of the residuals as a measure of error
residual_sd_error <- sd(plot_onw$residuals)

# Calculate error bounds using the residual sd 
plot_onw <- plot_onw %>%
  mutate(
    lower_bound = fitted_values - residual_sd_error, 
    upper_bound = fitted_values + residual_sd_error
  )

r_squared <- 0.855
p_value <- "< 0.0001"

# Create the plot
Ea_NW_plot <- ggplot(plot_onw, aes(x = log10(niche_width), y = estimate)) +
  geom_point(aes(size = Ea_weights,  colour = order), alpha = 5) +  # Scatter plot with point size proportional to weights
  scale_color_brewer(palette = "YlOrRd") +
  geom_smooth(aes(y = fitted_values), color = "#E25306", linewidth = 1, lty = 1, method = "lm",
              formula = y ~ poly(x, 2), se = T) +# Fitted line for weighted model
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
  labs(x = bquote(bold(log[10] * " Operational Niche Width (" * degree * "C)")),
       y = "Activation Energy (eV)") +
  annotate("text", x = Inf, y = Inf, 
           label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))), 
           hjust = 1.5, vjust = 2.1,
           color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.title.x = element_text(size = 20, face = "bold", family = "Times New Roman"), 
        axis.title.y = element_text(size = 20, face = "bold", vjust = 1.5, family = "Times New Roman"), 
        axis.text = element_text(size = 18, face = "bold", colour = "black", family = "Times New Roman"), 
        legend.position = "none") +
  scale_size_continuous(range = c(3, 8)) 
Ea_NW_plot


# Calculate Pearson correlation coefficient
correlation_pearson <- cor(op_niche_width$niche_width, log10(op_niche_width$estimate), method = "pearson")
print(paste("Pearson correlation coefficient:", correlation_pearson))


# Calculate Spearman correlation coefficient
correlation_spearman <- cor(op_niche_width$niche_width, log10(op_niche_width$estimate), method = "spearman")
print(paste("Spearman correlation coefficient:", correlation_spearman))