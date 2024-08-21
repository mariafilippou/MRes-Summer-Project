### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 10 - Modelling Niche Width against size and the environment 

# This script produces models to see the effects of body size and the environment
# on niche width and compares these models

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Install packages
require('dplyr')
require('ggplot2')

# Upload data 
params <- read.csv('final_parameters_with_niche_width.csv', head = T)

# Remove any curves that have < 3 data points below the peak
op_niche_width <- params[params$count_below_peak >= 3, ]

# Remove curves with poor fits
poor_fits <- c("Callosobruchus analis", "Callosobruchus chinensis", 
               "Callosobruchus maculatus", "Callosobruchus rhodesianus",
               "Plutella xylostella")

op_niche_width <- op_niche_width %>% 
  filter(!(species %in% poor_fits))

# Remove any NA
op_niche_width <- op_niche_width %>% 
  filter(!is.na(species))

hist(op_niche_width$niche_width)

# Model without weights 

  # Simple linear
  set.seed(123)
  model_linear1 <- lm(log10(niche_width) ~ log10(mean_mass), data = op_niche_width)
  summary(model_linear1)
  
  # Linear with latitude
  set.seed(123)
  model_linear2 <- lm(log10(niche_width) ~ log10(mean_mass) + abs(latitude), data = op_niche_width)
  summary(model_linear2)  
  
  # Linear with mean annual temp
  set.seed(123)
  model_linear3 <- lm(log10(niche_width) ~ log10(mean_mass) + mean_annual_temp, data = op_niche_width)
  summary(model_linear3)  
  
  # Linear with mean diurnal range 
  set.seed(123)
  model_linear4 <- lm(log10(niche_width) ~ log10(mean_mass) + mean_diurnal_range, data = op_niche_width)
  summary(model_linear4)  
  
  # Linear with order 
  set.seed(123)
  model_linear5 <- lm(log10(niche_width) ~ log10(mean_mass) + order, data = op_niche_width)
  summary(model_linear5)  
  
  # Linear with order (interactor)
  set.seed(123)
  model_linear6 <- lm(log10(niche_width) ~ log10(mean_mass) * order, data = op_niche_width)
  summary(model_linear6) 
  
  
  # Quadratic
  set.seed(123)
  model_quadratic1 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2), data = op_niche_width)
  summary(model_quadratic1)
  
  # Quadratic with latitude
  set.seed(123)
  model_quadratic2 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + abs(latitude), data = op_niche_width)
  summary(model_quadratic2)  
  
  # Quadratic with mean annual temp
  set.seed(123)
  model_quadratic3 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_annual_temp, data = op_niche_width)
  summary(model_quadratic3)  
  
  # Quadratic with mean diurnal temp
  set.seed(123)
  model_quadratic4 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_diurnal_range, data = op_niche_width)
  summary(model_quadratic4)  
  
  # Quadratic with order
  set.seed(123)
  model_quadratic5 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + order, data = op_niche_width)
  summary(model_quadratic5)  
  
  # Quadratic with order (interactor)
  set.seed(123)
  model_quadratic6 <- lm(log10(niche_width) ~ poly(log10(mean_mass), 2, raw = T) * order, data = op_niche_width)
  summary(model_quadratic6)  

# Do weighted regression to determine best model 

  # Calculate the weights as the inverse of the CI width 
    op_niche_width$weights <- 1 / ((op_niche_width$niche_upper - op_niche_width$niche_lower)^2)
  
  # Simple weighted
  set.seed(123)
  model_weighted1 <- lm(log10(niche_width) ~ log10(mean_mass), data = op_niche_width, weights = weights)
  summary(model_weighted1)
  
  # Simple weighted with latitude
  set.seed(123)
  model_weighted2 <- lm(log10(niche_width) ~ log10(mean_mass) + abs(latitude), data = op_niche_width, weights = weights)
  summary(model_weighted2)
  
  # Simple weighted with mean annual temperature
  set.seed(123)    
  model_weighted3 <- lm(log10(niche_width) ~ log10(mean_mass) + mean_annual_temp, data = op_niche_width, weights = weights)
  summary(model_weighted3)  
  
  # Simple weighted with mean diurnal range 
  set.seed(123)   
  model_weighted4 <- lm(niche_width ~ log10(mean_mass) + mean_diurnal_range, data = op_niche_width, weights = weights)
  summary(model_weighted4)  
  
  # Simple weighted with order
  set.seed(123)   
  model_weighted5 <- lm(log10(niche_width) ~ log10(mean_mass) + order, data = op_niche_width, weights = weights)
  summary(model_weighted5)  
  
  # Simple weighted with order (interactor)
  set.seed(123)   
  model_weighted6 <- lm(log10(niche_width) ~ log10(mean_mass) * order, data = op_niche_width, weights = weights)
  summary(model_weighted6)  
  
  # Quadratic weighted
  set.seed(123)
  model_weighted7 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2), data = op_niche_width, weights = weights)
  summary(model_weighted7)
  
  # Quadratic weighted with latitude
  set.seed(123)
  model_weighted8 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + abs(latitude), data = op_niche_width, weights = weights)
  summary(model_weighted8)
  
  # Quadratic weighted with mean annual temperature
  set.seed(123)    
  model_weighted9 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_annual_temp, data = op_niche_width, weights = weights)
  summary(model_weighted9)  
  
  # Quadratic weighted with mean diurnal range 
  set.seed(123)   
  model_weighted10 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_diurnal_range, data = op_niche_width, weights = weights)
  summary(model_weighted10)  
  
  # Quadratic weighted with order
  set.seed(123)   
  model_weighted11 <- lm(log10(niche_width) ~ log10(mean_mass) + I(log10(mean_mass)^2) + order, data = op_niche_width, weights = weights)
  summary(model_weighted11)  
  
  # Quadratic weighted with order (interactor)
  set.seed(123)   
  model_weighted12 <- lm(log10(niche_width) ~ poly(log10(mean_mass), 2, raw = T) * order, data = op_niche_width, weights = weights)
  summary(model_weighted12)  
  
# Compare the models to determine the best one  

# List of models
models <- list(
  model_linear1,
  model_linear2,
  model_linear3, 
  model_linear4,
  model_linear5,
  model_linear6,
  model_quadratic1, 
  model_quadratic2,
  model_quadratic3,
  model_quadratic4, 
  model_quadratic5,
  model_quadratic6,
  model_weighted1,
  model_weighted2,
  model_weighted3,
  model_weighted4,
  model_weighted5,
  model_weighted6,
  model_weighted7,
  model_weighted8, 
  model_weighted9, 
  model_weighted10, 
  model_weighted11,
  model_weighted12
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

# Plot the best model

  # Create a data frame 
  plot_data <- op_niche_width %>%
    mutate(
      log10_mean_mass = log10(mean_mass),
      fitted_values = predict(model_weighted12, newdata = op_niche_width)
    )
  
  plot_data <- plot_data %>%
    mutate(
      residuals = log10(niche_width) - fitted_values
    )

# Estimate the sd of the residuals as a measure of error
residual_sd_error <- sd(plot_data$residuals)

# Calculate error bounds using the residual sd 
plot_data <- plot_data %>%
  mutate(
    lower_bound = fitted_values - residual_sd_error, 
    upper_bound = fitted_values + residual_sd_error
  )

r_squared <- 0.812
p_value <- "< 0.0001"

# Plot 
NW_plot <- ggplot(plot_data, aes(x = log10(mean_mass), y = log10(niche_width))) +
  geom_point(aes(size = weights, color = order), alpha = 0.6) +  # Raw data points with size based on weights
  scale_color_brewer(palette = "YlOrRd") +
  geom_smooth(aes(y = fitted_values), colour = "#E25306", size = 1, method = "lm", formula = y ~ poly(x, 2), se = T) + # Fitted line
  labs(x = bquote(bold(log[10] * " Mass (mg)")),
       y = bquote(bold(log[10] * " Operational Niche Width (" * degree * "C)"))) +
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
NW_plot  

### Environment Plots

# Mean annual temperature 
  
  # Linear model
  set.seed(123)
  NW_mat_model1 <- lm(log10(niche_width) ~ mean_annual_temp, data = op_niche_width)
  summary(NW_mat_model1) 
  
  # Linear model with order 
  set.seed(123)
  NW_mat_model2 <- lm(log10(niche_width) ~ mean_annual_temp + order, data = op_niche_width)
  summary(NW_mat_model2)
  
  # Linear model with order (interactor)
  set.seed(123)
  NW_mat_model3 <- lm(log10(niche_width) ~ mean_annual_temp * order , data = op_niche_width)
  summary(NW_mat_model3)
  
  # Weighted model 
  set.seed(123)
  NW_mat_model4 <- lm(log10(niche_width) ~ mean_annual_temp, data = op_niche_width, weights = weights)
  summary(NW_mat_model4)
  
  # Weighted model with order 
  set.seed(123)
  NW_mat_model5 <- lm(log10(niche_width) ~ mean_annual_temp + order, data = op_niche_width, weights = weights)
  summary(NW_mat_model5)
  
  # Weighted model with order (interactor)
  set.seed(123)
  NW_mat_model6 <- lm(log10(niche_width) ~ mean_annual_temp * order, data = op_niche_width, weights = weights)
  summary(NW_mat_model6)
  
  # Quadratic model
  set.seed(123)
  NW_mat_model7 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE), data = op_niche_width)
  summary(NW_mat_model7)
  
  # Quadratic model with order 
  set.seed(123)
  NW_mat_model8 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE) + order, data = op_niche_width)
  summary(NW_mat_model8)
  
  # Quadratic model with order (interactor)
  set.seed(123)
  NW_mat_model9 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE) * order, data = op_niche_width)
  summary(NW_mat_model9)
  
  # Weighted quadratic model 
  set.seed(123)
  NW_mat_model10 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE), data = op_niche_width, weights = weights)
  summary(NW_mat_model10)
  
  # Weighted quadratic model with order 
  set.seed(123)
  NW_mat_model11 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE) + order, data = op_niche_width, weights = weights)
  summary(NW_mat_model11)
  
  # Weighted quadratic model with order (interactor)
  set.seed(123)
  NW_mat_model12 <- lm(log10(niche_width) ~ poly(mean_annual_temp, 2, raw = TRUE) * order, data = op_niche_width, weights = weights)
  summary(NW_mat_model12)  
  
  # Compare models
  mat_models <- list (
    NW_mat_model1, 
    NW_mat_model2, 
    NW_mat_model3, 
    NW_mat_model4, 
    NW_mat_model5, 
    NW_mat_model6, 
    NW_mat_model7, 
    NW_mat_model8, 
    NW_mat_model9, 
    NW_mat_model10,
    NW_mat_model11, 
    NW_mat_model12
  )
  
  model_comparison <- compare_models(mat_models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)
  
  # Plot the best model
  
  # Create a data frame with latitude and fitted values
  plot_data_3 <- op_niche_width %>%
    mutate(
      mean_annual_temp = mean_annual_temp,
      fitted_values = predict(NW_mat_model12, newdata = op_niche_width)
    )
  
  plot_data_3 <- plot_data_3 %>%
    mutate(
      residuals = niche_width - fitted_values
    )
  
  # Estimate the sd of the residuals as a measure of error
  residual_sd_error <- sd(plot_data_3$residuals)
  
  # Calculate error bounds using the residual sd 
  plot_data_3 <- plot_data_3 %>%
    mutate(
      lower_bound = fitted_values - residual_sd_error, 
      upper_bound = fitted_values + residual_sd_error
    )
  
  r_squared <- 0.81
  p_value <- "< 0.0001"
  
  # Plot 
  NW_mat_plot <- ggplot(plot_data_3, aes(x = mean_annual_temp, y = log10(niche_width), colour = op_niche_width$order)) +
    geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
    scale_color_brewer(palette = "YlOrRd") +
    geom_smooth(aes(y = fitted_values), method = "lm", formula = y ~ poly(x, 2), se = T, 
                color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
    labs(x = expression(bold(paste("Mean Annual Temperature (", degree, "C)"))),
         y = bquote(bold(log[10] * " Operational Niche Width (" * degree * "C)"))) +
    annotate("text", x = Inf, y = Inf, 
             label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
             hjust = 1.5, vjust = 2.5,
             color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          axis.title.x = element_text(size=22, face = "bold", family = "Times New Roman"), 
          axis.title.y = element_text(size=22, face = "bold", vjust = 1.8, family = "Times New Roman"), 
          axis.text = element_text(size = 20, face = "bold", colour = "black", family = "Times New Roman"), 
          legend.position = "none") +
    scale_size_continuous(range = c(3, 8)) + # Adjust size range for better visualization
    scale_x_continuous(labels = label_number(accuracy = 0.1))
  NW_mat_plot  
  
# Mean diurnal range
  
  # Linear model
  set.seed(123)
  NW_mdr_model1 <- lm(log10(niche_width) ~ mean_diurnal_range, data = op_niche_width)
  summary(NW_mdr_model1) 
  
  # Linear model with order 
  set.seed(123)
  NW_mdr_model2 <- lm(log10(niche_width) ~ mean_diurnal_range + order, data = op_niche_width)
  summary(NW_mdr_model2)
  
  # Linear model with order (interactor)
  set.seed(123)
  NW_mdr_model3 <- lm(log10(niche_width) ~ mean_diurnal_range * order , data = op_niche_width)
  summary(NW_mdr_model3)
  
  # Weighted model 
  set.seed(123)
  NW_mdr_model4 <- lm(log10(niche_width) ~ mean_diurnal_range, data = op_niche_width, weights = weights)
  summary(NW_mdr_model4)
  
  # Weighted model with order 
  set.seed(123)
  NW_mdr_model5 <- lm(log10(niche_width) ~ mean_diurnal_range + order, data = op_niche_width, weights = weights)
  summary(NW_mdr_model5)
  
  # Weighted model with order (interactor)
  set.seed(123)
  NW_mdr_model6 <- lm(log10(niche_width) ~ mean_diurnal_range * order, data = op_niche_width, weights = weights)
  summary(NW_mdr_model6)
  
  # Quadratic model
  set.seed(123)
  NW_mdr_model7 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE), data = op_niche_width)
  summary(NW_mdr_model7)
  
  # Quadratic model with order 
  set.seed(123)
  NW_mdr_model8 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE) + order, data = op_niche_width)
  summary(NW_mdr_model8)
  
  # Quadratic model with order (interactor)
  set.seed(123)
  NW_mdr_model9 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE) * order, data = op_niche_width)
  summary(NW_mdr_model9)
  
  # Weighted quadratic model 
  set.seed(123)
  NW_mdr_model10 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE), data = op_niche_width, weights = weights)
  summary(NW_mdr_model10)
  
  # Weighted quadratic model with order 
  set.seed(123)
  NW_mdr_model11 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE) + order, data = op_niche_width, weights = weights)
  summary(NW_mdr_model11)
  
  # Weighted quadratic model with order (interactor)
  set.seed(123)
  NW_mdr_model12 <- lm(log10(niche_width) ~ poly(mean_diurnal_range, 2, raw = TRUE) * order, data = op_niche_width, weights = weights)
  summary(NW_mdr_model12)  
  
  # Compare models
  mdr_models <- list (
    NW_mdr_model1,
    NW_mdr_model2,
    NW_mdr_model3,
    NW_mdr_model4,
    NW_mdr_model5,
    NW_mdr_model6,
    NW_mdr_model7,
    NW_mdr_model8,
    NW_mdr_model9,
    NW_mdr_model10,
    NW_mdr_model11,
    NW_mdr_model12
  )
  
  model_comparison <- compare_models(mdr_models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)
  
# Plot the best model
  
  # Create a data frame with latitude and fitted values
  plot_data_4 <- op_niche_width %>%
    mutate(
      mean_diurnal_range = mean_diurnal_range,
      fitted_values = predict(NW_mdr_model12, newdata = op_niche_width)
    )
  
  plot_data_4 <- plot_data_4 %>%
    mutate(
      residuals = niche_width - fitted_values
    )
  
  # Estimate the sd of the residuals as a measure of error
  residual_sd_error <- sd(plot_data_4$residuals)
  
  # Calculate error bounds using the residual sd 
  plot_data_4 <- plot_data_4 %>%
    mutate(
      lower_bound = fitted_values - residual_sd_error, 
      upper_bound = fitted_values + residual_sd_error
    )
  r_squared <- 0.719
  p_value <- " < 0.0001"
  
  # Plot
  NW_mdr_plot <- ggplot(plot_data_4, aes(x = mean_diurnal_range, y = log10(niche_width), colour = op_niche_width$order)) +
    geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
    scale_color_brewer(palette = "YlOrRd") +
    stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ poly(x, 2),  se = T, 
                color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
    labs(x = expression(bold(paste("Mean Diurnal Range (", degree, "C)"))), 
         y = NULL) +
    annotate("text", x = Inf, y = Inf, 
             label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
             hjust = 1.5, vjust = 2.5,
             color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          axis.title.x = element_text(size=22, face = "bold", family = "Times New Roman"), 
          axis.title.y = element_text(size=22, face = "bold", vjust = 1.8,  family = "Times New Roman"), 
          axis.text = element_text(size = 20, face = "bold", colour = "black",  family = "Times New Roman"), 
          legend.position = "none") +
    scale_size_continuous(range = c(3, 8)) + # Adjust size range for better visualization
    scale_x_continuous(limits = c(6, 15))
  NW_mdr_plot  
  
# Latitude 
  
  # Linear model
  set.seed(123)
  NW_lat_model1 <- lm(log10(niche_width) ~ abs(latitude), data = op_niche_width)
  summary(NW_lat_model1) 
  
  # Linear model with order 
  set.seed(123)
  NW_lat_model2 <- lm(log10(niche_width) ~ abs(latitude) + order, data = op_niche_width)
  summary(NW_lat_model2)
  
  # Linear model with order (interactor)
  set.seed(123)
  NW_lat_model3 <- lm(log10(niche_width) ~ abs(latitude) * order , data = op_niche_width)
  summary(NW_lat_model3)
  
  # Weighted model 
  set.seed(123)
  NW_lat_model4 <- lm(log10(niche_width) ~ abs(latitude), data = op_niche_width, weights = weights)
  summary(NW_lat_model4)
  
  # Weighted model with order 
  set.seed(123)
  NW_lat_model5 <- lm(log10(niche_width) ~ abs(latitude) + order, data = op_niche_width, weights = weights)
  summary(NW_lat_model5)
  
  # Weighted model with order (interactor)
  set.seed(123)
  NW_lat_model6 <- lm(log10(niche_width) ~ abs(latitude) * order, data = op_niche_width, weights = weights)
  summary(NW_lat_model6)
  
  # Quadratic model
  set.seed(123)
  NW_lat_model7 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE), data = op_niche_width)
  summary(NW_lat_model7)
  
  # Quadratic model with order 
  set.seed(123)
  NW_lat_model8 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE) + order, data = op_niche_width)
  summary(NW_lat_model8)
  
  # Quadratic model with order (interactor)
  set.seed(123)
  NW_lat_model9 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE) * order, data = op_niche_width)
  summary(NW_lat_model9)
  
  # Weighted quadratic model 
  set.seed(123)
  NW_lat_model10 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE), data = op_niche_width, weights = weights)
  summary(NW_lat_model10)
  
  # Weighted quadratic model with order 
  set.seed(123)
  NW_lat_model11 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE) + order, data = op_niche_width, weights = weights)
  summary(NW_lat_model11)
  
  # Weighted quadratic model with order (interactor)
  set.seed(123)
  NW_lat_model12 <- lm(log10(niche_width) ~ poly(abs(latitude), 2, raw = TRUE) * order, data = op_niche_width, weights = weights)
  summary(NW_lat_model12)  
  
  # Compare models
  lat_models <- list (
    NW_lat_model1, 
    NW_lat_model2,
    NW_lat_model3,
    NW_lat_model4,
    NW_lat_model5,
    NW_lat_model6,
    NW_lat_model7,
    NW_lat_model8,
    NW_lat_model9,
    NW_lat_model10,
    NW_lat_model11,
    NW_lat_model12
  )
  
  model_comparison <- compare_models(lat_models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)
  
  # Plot the best model
  
  # Create a data frame with latitude and fitted values
  plot_data_5 <- op_niche_width %>%
    mutate(
      abs_lat = abs(latitude),
      fitted_values = predict(NW_lat_model12, newdata = op_niche_width)
    )
  
  plot_data_5 <- plot_data_5 %>%
    mutate(
      residuals = estimate - fitted_values
    )
  
  # Estimate the sd of the residuals as a measure of error
  residual_sd_error <- sd(plot_data_5$residuals)
  
  # Calculate error bounds using the residual sd 
  plot_data_5 <- plot_data_5 %>%
    mutate(
      lower_bound = fitted_values - residual_sd_error, 
      upper_bound = fitted_values + residual_sd_error
    )
  
  r_squared <- 0.778
  p_value <- " < 0.0001"
  
  # Plot
  NW_lat_plot <- ggplot(plot_data_5, aes(x = abs_lat, y = log10(niche_width), color = op_niche_width$order)) +
    geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
    scale_color_brewer(palette = "YlOrRd") +
    stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
    labs(x = expression(bold(paste("Latitude (", degree, "N or S"))), 
         y = bquote(bold(log[10] * " Operational Niche Width (" * degree * "C)"))) +
    annotate("text", x = Inf, y = Inf, 
             label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
             hjust = 1.5, vjust = 2.5,
             color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          axis.title.x = element_text(size=22, face = "bold", family = "Times New Roman"), 
          axis.title.y = element_text(size=22, face = "bold", vjust = 1.8,  family = "Times New Roman"), 
          axis.text = element_text(size = 20, face = "bold", colour = "black",  family = "Times New Roman"), 
          legend.position = "none") +
    scale_size_continuous(range = c(3, 8))# Adjust size range for better visualization
  NW_lat_plot
  
# Annual precipitation 
  
  # Linear model
  set.seed(123)
  NW_ap_model1 <- lm(log10(niche_width) ~ annual_precip, data = op_niche_width)
  summary(NW_ap_model1) 
  
  # Linear model with order 
  set.seed(123)
  NW_ap_model2 <- lm(log10(niche_width) ~ annual_precip + order, data = op_niche_width)
  summary(NW_ap_model2)
  
  # Linear model with order (interactor)
  set.seed(123)
  NW_ap_model3 <- lm(log10(niche_width) ~ annual_precip * order , data = op_niche_width)
  summary(NW_ap_model3)
  
  # Weighted model 
  set.seed(123)
  NW_ap_model4 <- lm(log10(niche_width) ~ annual_precip, data = op_niche_width, weights = weights)
  summary(NW_ap_model4)
  
  # Weighted model with order 
  set.seed(123)
  NW_ap_model5 <- lm(log10(niche_width) ~ annual_precip + order, data = op_niche_width, weights = weights)
  summary(NW_ap_model5)
  
  # Weighted model with order (interactor)
  set.seed(123)
  NW_ap_model6 <- lm(log10(niche_width) ~ annual_precip * order, data = op_niche_width, weights = weights)
  summary(NW_ap_model6)
  
  # Quadratic model
  set.seed(123)
  NW_ap_model7 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE), data = op_niche_width)
  summary(NW_ap_model7)
  
  # Quadratic model with order 
  set.seed(123)
  NW_ap_model8 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE) + order, data = op_niche_width)
  summary(NW_ap_model8)
  
  # Quadratic model with order (interactor)
  set.seed(123)
  NW_ap_model9 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE) * order, data = op_niche_width)
  summary(NW_ap_model9)
  
  # Weighted quadratic model 
  set.seed(123)
  NW_ap_model10 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE), data = op_niche_width, weights = weights)
  summary(NW_ap_model10)
  
  # Weighted quadratic model with order 
  set.seed(123)
  NW_ap_model11 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE) + order, data = op_niche_width, weights = weights)
  summary(NW_ap_model11)
  
  # Weighted quadratic model with order (interactor)
  set.seed(123)
  NW_ap_model12 <- lm(log10(niche_width) ~ poly(annual_precip, 2, raw = TRUE) * order, data = op_niche_width, weights = weights)
  summary(NW_ap_model12)  
  
  # Compare models
  ap_models <- list (
    NW_ap_model1,
    NW_ap_model2,
    NW_ap_model3,
    NW_ap_model4,
    NW_ap_model5,
    NW_ap_model6,
    NW_ap_model7,
    NW_ap_model8,
    NW_ap_model9,
    NW_ap_model10,
    NW_ap_model11,
    NW_ap_model12
  )
  
  model_comparison <- compare_models(ap_models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)
  
  # Plot the best model
  
  # Create a data frame with latitude and fitted values
  plot_data_6 <- op_niche_width %>%
    mutate(
      annual_precip = annual_precip,
      fitted_values = predict(NW_ap_model12, newdata = op_niche_width)
    )
  
  plot_data_6 <- plot_data_6 %>%
    mutate(
      residuals = niche_width - fitted_values
    )
  
  # Estimate the sd of the residuals as a measure of error
  residual_sd_error <- sd(plot_data_6$residuals)
  
  # Calculate error bounds using the residual sd 
  plot_data_6 <- plot_data_6 %>%
    mutate(
      lower_bound = fitted_values - residual_sd_error, 
      upper_bound = fitted_values + residual_sd_error
    )
  
  r_squared <- 0.674
  p_value <- " < 0.0001"
  
  # Plot
  NW_ap_plot <- ggplot(plot_data_6, aes(x = annual_precip, y = log10(niche_width), color = op_niche_width$order)) +
    geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
    scale_color_brewer(palette = "YlOrRd") +
    stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
    labs(x = expression(bold("Annual Precipitation (mm "*year^{-1}*")")), 
         y = bquote(bold(log[10] * " Operational Niche Width (" * degree * "C)"))) +
    annotate("text", x = Inf, y = Inf, 
             label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
             hjust = 1.5, vjust = 2.5,
             color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          axis.title.x = element_text(size=22, face = "bold", family = "Times New Roman"), 
          axis.title.y = element_text(size=22, face = "bold", vjust = 1.8,  family = "Times New Roman"), 
          axis.text = element_text(size = 20, face = "bold", colour = "black",  family = "Times New Roman"), 
          legend.position = "none") +
    scale_size_continuous(range = c(3, 8))# Adjust size range for better visualization
  NW_ap_plot     
  
  # Plot forest plot and histogram side by side
  plot_grid(NW_lat_plot, NW_ap_plot,
            labels = c("a)", "b)"),
            label_size = 20, 
            label_fontfamily = "Times New Roman", 
            nrow = 1)
  
  # Save as PDF and embed fonts
  ggsave("NW_extra_plots.png", plot = last_plot(), dpi = 1300, width = 15, height = 7.5)    
