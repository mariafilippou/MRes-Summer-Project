### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 5 - Modelling Ea against size and the environment 

# This script produces models to see the effects of body size and the environment
# on Ea and compares these models

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

# Install packages
require('dplyr')
require('ggplot2')
require('RColorBrewer')

# Input dataframe 
params <- read.csv("final_fecundity_parameters2.csv", head = T, sep = ",", fill = T)

# Subset
  act_energy <- params %>% filter(param == "e")
  
  # Remove any curves that have < 3 data points below the peak
  Ea_est <- act_energy[act_energy$count_below_peak >= 3, ]

  # Remove curves with poor fits
  poor_fits <- c("Callosobruchus analis", "Callosobruchus chinensis", 
                 "Callosobruchus maculatus", "Callosobruchus rhodesianus", 
                 "Pemphigus populitransversus", "Plutella xylostella")
  
  Ea_good_fits <- Ea_est %>% 
    filter(!(species %in% poor_fits))
  
  # Remove any values above 2.5
  Ea_2.5 <- Ea_good_fits[Ea_good_fits$estimate <= 2.5, ]
  
  # Remove any NA
  Ea_values <- Ea_2.5 %>% 
    filter(!is.na(species))

  Ea_values$log10_mass <- log10(Ea_values$mean_mass)
  
# Model without weights 
  
  # Simple linear
  set.seed(123)
  model_linear1 <- lm(estimate ~ log10(mean_mass), data = Ea_values)
  summary(model_linear1)
  
  # Linear with absolute latitude 
  set.seed(123)
  model_linear2 <- lm(estimate ~ log10(mean_mass) + abs(latitude), data = Ea_values)
  summary(model_linear2)  
  
  # Linear with mean annual temp
  set.seed(123)
  model_linear3 <- lm(estimate ~ log10(mean_mass) + mean_annual_temp, data = Ea_values)
  summary(model_linear3)  
  
  # Linear with mean diurnal range 
  set.seed(123)
  model_linear4 <- lm(estimate ~ log10(mean_mass) + mean_diurnal_range, data = Ea_values)
  summary(model_linear4)  
  
  # Linear with order 
  set.seed(123)
  model_linear5 <- lm(estimate ~ log10(mean_mass) + order, data = Ea_values)
  summary(model_linear5)  
  
  # Linear with order (interactor)
  set.seed(123)
  model_linear6 <- lm(estimate ~ log10(mean_mass) * order, data = Ea_values)
  summary(model_linear6)  
  
  
  # Quadratic
  set.seed(123)
  model_quadratic1 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2), data = Ea_values)
  summary(model_quadratic1)
  
  # Quadratic with latitude
  set.seed(123)
  model_quadratic2 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + abs(latitude), data = Ea_values)
  summary(model_quadratic2)  
  
  # Quadratic with mean annual temp
  set.seed(123)
  model_quadratic3 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_annual_temp, data = Ea_values)
  summary(model_quadratic3)  
  
  # Quadratic with mean diurnal temp
  set.seed(123)
  model_quadratic4 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_diurnal_range, data = Ea_values)
  summary(model_quadratic4)  
  
  # Quadratic with order
  set.seed(123)
  model_quadratic5 <- lm(estimate ~ poly(log10(mean_mass), 2, raw = TRUE) + order, data = Ea_values)
  summary(model_quadratic5) 
  
  # Quadratic with order (interactor)
  set.seed(123)
  model_quadratic6 <- lm(estimate ~ poly(log10(mean_mass), 2, raw = TRUE) * order, data = Ea_values)
  summary(model_quadratic6)  
  
# Do weighted regression to determine best model 
  
  # Calculate the weights as the inverse of the CI width 
  Ea_values$weights <- 1 / ((Ea_values$conf_upper - Ea_values$conf_lower)^2)
  
  # Simple weighted
  set.seed(123)
  model_weighted1 <- lm(estimate ~ log10(mean_mass), data = Ea_values, weights = weights)
  summary(model_weighted1)
  
  # Simple weighted with latitude
  set.seed(123)
  model_weighted2 <- lm(estimate ~ log10(mean_mass) + abs(latitude), data = Ea_values, weights = weights)
  summary(model_weighted2)
  
  # Simple weighted with mean annual temperature
  set.seed(123)    
  model_weighted3 <- lm(estimate ~ log10(mean_mass) + mean_annual_temp, data = Ea_values, weights = weights)
  summary(model_weighted3)  
  
  # Simple weighted with mean diurnal range 
  set.seed(123)   
  model_weighted4 <- lm(estimate ~ log10(mean_mass) + mean_diurnal_range, data = Ea_values, weights = weights)
  summary(model_weighted4)  
  
  # Simple weighted with order
  set.seed(123)   
  model_weighted5 <- lm(estimate ~ log10(mean_mass) + order, data = Ea_values, weights = weights)
  summary(model_weighted5)  
  
  # Simple weighted with order (interactor)
  set.seed(123)   
  model_weighted6 <- lm(estimate ~ log10(mean_mass) * order, data = Ea_values, weights = weights)
  summary(model_weighted6)  
  
  
  # Quadratic weighted
  set.seed(123)
  model_weighted7 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2), data = Ea_values, weights = weights)
  summary(model_weighted7)
  
  # Quadratic weighted with latitude
  set.seed(123)
  model_weighted8 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + abs(latitude), data = Ea_values, weights = weights)
  summary(model_weighted8)
  
  # Quadratic weighted with mean annual temperature
  set.seed(123)    
  model_weighted9 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_annual_temp, data = Ea_values, weights = weights)
  summary(model_weighted9)  
  
  # Quadratic weighted with mean diurnal range 
  set.seed(123)   
  model_weighted10 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + mean_diurnal_range, data = Ea_values, weights = weights)
  summary(model_weighted10)  
  
  # Quadratic weighted with order
  set.seed(123)   
  model_weighted11 <- lm(estimate ~ log10(mean_mass) + I(log10(mean_mass)^2) + order, data = Ea_values, weights = weights)
  summary(model_weighted11)  
  
  # Quadratic weighted with order (interactor)
  set.seed(123)   
  model_weighted12 <- lm(estimate ~ poly(log10(mean_mass), 2, raw = TRUE) * order, data = Ea_values, weights = weights)
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
  plot_data <- Ea_values %>%
    mutate(
      log10_mean_mass = log10(mean_mass),
      fitted_values = predict(model_linear1, newdata = Ea_values)
    )
  
  plot_data <- plot_data %>%
    mutate(
      residuals = estimate - fitted_values
    )
  
  # Estimate the sd of the residuals as a measure of error
  residual_sd_error <- sd(plot_data$residuals)
  
  # Calculate error bounds using the residual sd 
  plot_data <- plot_data %>%
    mutate(
      lower_bound = fitted_values - residual_sd_error, 
      upper_bound = fitted_values + residual_sd_error
    )
  
  r_squared <- 0.307
  p_value <- "< 0.001"
  
  # Plot 
  Ea_plot <- ggplot(plot_data, aes(x = log10(mean_mass), y = estimate)) +
    geom_point(aes(size = weights, color = order), alpha = 5) +  # Raw data points with size based on weights
    scale_color_brewer(palette = "YlOrRd") +
    geom_smooth(aes(y = fitted_values), colour = "#E25306", size = 1, method = "lm", formula = y ~ x, se = T) + # Fitted line
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
    labs(x = bquote(bold(log[10] * " Mass (mg)")),
         y = "Activation Energy (eV)",
         size = "Weights") +
    annotate("text", x = Inf, y = Inf, 
             label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
             hjust = 1.5, vjust = 6.1,
             color = "#E25306", size = 7, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          axis.title.x = element_text(size = 20, face = "bold", family = "Times New Roman"), 
          axis.title.y = element_text(size = 20, face = "bold", vjust = 1.5, family = "Times New Roman"), 
          axis.text = element_text(size = 18, face = "bold", colour = "black", family = "Times New Roman"), 
          legend.position = "none") +
    scale_size_continuous(range = c(3, 8)) 
  Ea_plot  
  
  plot_grid(Ea_plot, NW_plot, Ea_NW_plot,
            label_fontfamily = "Times New Roman", label_size = 20, nrow = 1)
  
  # Save as PDF and embed fonts
  ggsave("mass_plots.png", plot = last_plot(), dpi = 1500, width = 15, height = 5)
  ggsave("labels.png", plot = last_plot(), dpi = 1500, width = 15, height = 5)
  
### Environment Plots
  
# Mean annual temperature 
  
  # Linear model
  set.seed(123)
  Ea_mat_model1 <- lm(estimate ~ mean_annual_temp, data = Ea_values)
  summary(Ea_mat_model1)
  
  # Linear model with order 
  set.seed(123)
  Ea_mat_model2 <- lm(estimate ~ mean_annual_temp + order, data = Ea_values)
  summary(Ea_mat_model2)
  
  # Linear model with order (interactor)
  set.seed(123)
  Ea_mat_model3 <- lm(estimate ~ mean_annual_temp * order , data = Ea_values)
  summary(Ea_mat_model3)
  
  # Weighted model 
  set.seed(123)
  Ea_mat_model4 <- lm(estimate ~ mean_annual_temp, data = Ea_values, weights = weights)
  summary(Ea_mat_model4)
  
  # Weighted model with order 
  set.seed(123)
  Ea_mat_model5 <- lm(estimate ~ mean_annual_temp + order, data = Ea_values, weights = weights)
  summary(Ea_mat_model5)
  
  # Weighted model with order (interactor)
  set.seed(123)
  Ea_mat_model6 <- lm(estimate ~ mean_annual_temp * order, data = Ea_values, weights = weights)
  summary(Ea_mat_model6)
  
  # Quadratic model
  set.seed(123)
  Ea_mat_model7 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE), data = Ea_values)
  summary(Ea_mat_model7)
  
  # Quadratic model with order 
  set.seed(123)
  Ea_mat_model8 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE) + order, data = Ea_values)
  summary(Ea_mat_model8)
  
  # Quadratic model with order (interactor)
  set.seed(123)
  Ea_mat_model9 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE) * order, data = Ea_values)
  summary(Ea_mat_model9)
  
  # Weighted quadratic model 
  set.seed(123)
  Ea_mat_model10 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE), data = Ea_values, weights = weights)
  summary(Ea_mat_model10)
  
  # Weighted model with order 
  set.seed(123)
  Ea_mat_model11 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE) + order, data = Ea_values, weights = weights)
  summary(Ea_mat_model11)
  
  # Weighted model with order (interactor)
  set.seed(123)
  Ea_mat_model12 <- lm(estimate ~ poly(mean_annual_temp, 2, raw = TRUE) * order, data = Ea_values, weights = weights)
  summary(Ea_mat_model12)  
  
  # Compare models
  mat_models <- list (
    Ea_mat_model1, 
    Ea_mat_model2, 
    Ea_mat_model3, 
    Ea_mat_model4, 
    Ea_mat_model5, 
    Ea_mat_model6, 
    Ea_mat_model7, 
    Ea_mat_model8, 
    Ea_mat_model9, 
    Ea_mat_model10, 
    Ea_mat_model11, 
    Ea_mat_model12
    )
  
  model_comparison <- compare_models(mat_models)
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  print(model_comparison)
  
  # Plot the best model
  
    # Create a data frame with latitude and fitted values
    plot_data_3 <- Ea_values %>%
      mutate(
        mean_annual_temp = mean_annual_temp,
        fitted_values = predict(Ea_mat_model5, newdata = Ea_values),
        order = order
      )
    
    plot_data_3 <- plot_data_3 %>%
      mutate(
        residuals = estimate - fitted_values
      )
    
    # Estimate the sd of the residuals as a measure of error
    residual_sd_error <- sd(plot_data$residuals)
    
    # Calculate error bounds using the residual sd 
    plot_data_3 <- plot_data_3 %>%
      mutate(
        lower_bound = fitted_values - residual_sd_error, 
        upper_bound = fitted_values + residual_sd_error
      )
    
    r_squared <- 0.586
    p_value <- "< 0.0001"
    
    # Plot 
    Ea_mat_plot <- ggplot(plot_data_3, aes(x = mean_annual_temp, y = estimate, color = order)) +
      geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
      scale_colour_brewer(palette = "YlOrRd") +
      geom_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                  color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
      labs(x = NULL, y = "Activation Energy (eV)") +
      annotate("text", x = Inf, y = Inf, 
               label = bquote(atop(R^2 == .(r_squared), italic(p) == .(p_value))),
               hjust = 1.4, vjust = 2.5,
               color = "#E25306", size = 6, fontface = "bold", family = "Times New Roman") +  # Annotation with R² and p-value in italics
      theme(panel.background = element_blank(), 
            axis.line = element_line(color = "black"), 
            axis.title.x = element_text(size=22, face = "bold", family = "Times New Roman"), 
            axis.title.y = element_text(size=22, face = "bold", vjust = 1.8, family = "Times New Roman"), 
            axis.text = element_text(size = 20, face = "bold", colour = "black", family = "Times New Roman"), 
            legend.position = "none") +
      scale_size_continuous(range = c(3, 8)) + # Adjust size range for better visualization
      scale_x_continuous(labels = label_number(accuracy = 0.1))
    Ea_mat_plot  

# Mean diurnal range
    
    # Linear model
    set.seed(123)
    Ea_mdr_model1 <- lm(estimate ~ mean_diurnal_range, data = Ea_values)
    summary(Ea_mdr_model1)
    
    # Linear model with order 
    set.seed(123)
    Ea_mdr_model2 <- lm(estimate ~ mean_diurnal_range + order, data = Ea_values)
    summary(Ea_mdr_model2)
    
    # Linear model with order (interactor)
    set.seed(123)
    Ea_mdr_model3 <- lm(estimate ~ mean_diurnal_range * order , data = Ea_values)
    summary(Ea_mdr_model3)
    
    # Weighted model 
    set.seed(123)
    Ea_mdr_model4 <- lm(estimate ~ mean_diurnal_range, data = Ea_values, weights = weights)
    summary(Ea_mdr_model4)
    
    # Weighted model with order 
    set.seed(123)
    Ea_mdr_model5 <- lm(estimate ~ mean_diurnal_range + order, data = Ea_values, weights = weights)
    summary(Ea_mdr_model5)
    
    # Weighted model with order (interactor)
    set.seed(123)
    Ea_mdr_model6 <- lm(estimate ~ mean_diurnal_range * order, data = Ea_values, weights = weights)
    summary(Ea_mdr_model6)
    
    # Quadratic model
    set.seed(123)
    Ea_mdr_model7 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE), data = Ea_values)
    summary(Ea_mdr_model7)
    
    # Quadratic model with order 
    set.seed(123)
    Ea_mdr_model8 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE) + order, data = Ea_values)
    summary(Ea_mdr_model8)
    
    # Quadratic model with order (interactor)
    set.seed(123)
    Ea_mdr_model9 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE) * order, data = Ea_values)
    summary(Ea_mdr_model9)
    
    # Weighted quadratic model 
    set.seed(123)
    Ea_mdr_model10 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE), data = Ea_values, weights = weights)
    summary(Ea_mdr_model10)
    
    # Weighted model with order with order 
    set.seed(123)
    Ea_mdr_model11 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE) + order, data = Ea_values, weights = weights)
    summary(Ea_mdr_model11)
    
    # Weighted model with order with order (interactor)
    set.seed(123)
    Ea_mdr_model12 <- lm(estimate ~ poly(mean_diurnal_range, 2, raw = TRUE) * order, data = Ea_values, weights = weights)
    summary(Ea_mdr_model12)  
    
    # Compare models
    mdr_models <- list (
      Ea_mdr_model1, 
      Ea_mdr_model2, 
      Ea_mdr_model3, 
      Ea_mdr_model4, 
      Ea_mdr_model5, 
      Ea_mdr_model6, 
      Ea_mdr_model7, 
      Ea_mdr_model8, 
      Ea_mdr_model9, 
      Ea_mdr_model10, 
      Ea_mdr_model11, 
      Ea_mdr_model12
    )
    
    model_comparison <- compare_models(mdr_models)
    model_comparison <- model_comparison %>%
      arrange(AIC)
    
    print(model_comparison)
    
  # Plot the best model
  
    # Create a data frame with latitude and fitted values
    plot_data_4 <- Ea_values %>%
      mutate(
        mean_diurnal_range = mean_diurnal_range,
        fitted_values = predict(Ea_mdr_model5, newdata = Ea_values)
      )
    
    plot_data_4 <- plot_data_4 %>%
      mutate(
        residuals = estimate - fitted_values
      )
    
    # Estimate the sd of the residuals as a measure of error
    residual_sd_error <- sd(plot_data$residuals)
    
    # Calculate error bounds using the residual sd 
    plot_data_4 <- plot_data_4 %>%
      mutate(
        lower_bound = fitted_values - residual_sd_error, 
        upper_bound = fitted_values + residual_sd_error
      )
    
    r_squared <- 0.594
    p_value <- " < 0.0001"
    
    # Plot
    Ea_mdr_plot <- ggplot(plot_data_4, aes(x = mean_diurnal_range, y = estimate, color = Ea_values$order)) +
      geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
      scale_color_brewer(palette = "YlOrRd") +
      stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                  color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
      labs(x = NULL, y = NULL) +
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
    Ea_mdr_plot  

# Latitude
    
    # Linear model
    set.seed(123)
    Ea_lat_model1 <- lm(estimate ~ abs(latitude), data = Ea_values)
    summary(Ea_lat_model1)
    
    # Linear model with order 
    set.seed(123)
    Ea_lat_model2 <- lm(estimate ~ abs(latitude) + order, data = Ea_values)
    summary(Ea_lat_model2)
    
    # Linear model with order (interactor)
    set.seed(123)
    Ea_lat_model3 <- lm(estimate ~ abs(latitude) * order , data = Ea_values)
    summary(Ea_lat_model3)
    
    # Weighted model 
    set.seed(123)
    Ea_lat_model4 <- lm(estimate ~ abs(latitude), data = Ea_values, weights = weights)
    summary(Ea_lat_model4)
    
    # Weighted model with order 
    set.seed(123)
    Ea_lat_model5 <- lm(estimate ~ abs(latitude) + order, data = Ea_values, weights = weights)
    summary(Ea_lat_model5)
    
    # Weighted model with order (interactor)
    set.seed(123)
    Ea_lat_model6 <- lm(estimate ~ abs(latitude) * order, data = Ea_values, weights = weights)
    summary(Ea_lat_model6)
    
    # Quadratic model
    set.seed(123)
    Ea_lat_model7 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE), data = Ea_values)
    summary(Ea_lat_model7)
    
    # Quadratic model with order 
    set.seed(123)
    Ea_lat_model8 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE) + order, data = Ea_values)
    summary(Ea_lat_model8)
    
    # Quadratic model with order (interactor)
    set.seed(123)
    Ea_lat_model9 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE) * order, data = Ea_values)
    summary(Ea_lat_model9)
    
    # Weighted quadratic model 
    set.seed(123)
    Ea_lat_model10 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE), data = Ea_values, weights = weights)
    summary(Ea_lat_model10)
    
    # Weighted model with order 
    set.seed(123)
    Ea_lat_model11 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE) + order, data = Ea_values, weights = weights)
    summary(Ea_lat_model11)
    
    # Weighted model with order (interactor)
    set.seed(123)
    Ea_lat_model12 <- lm(estimate ~ poly(abs(latitude), 2, raw = TRUE) * order, data = Ea_values, weights = weights)
    summary(Ea_lat_model12)  
    
    # Compare models
    lat_models <- list (
      Ea_lat_model1, 
      Ea_lat_model2, 
      Ea_lat_model3,
      Ea_lat_model4,
      Ea_lat_model5,
      Ea_lat_model6,
      Ea_lat_model7,
      Ea_lat_model8,
      Ea_lat_model9,
      Ea_lat_model10,
      Ea_lat_model11,
      Ea_lat_model12
    )
    
    model_comparison <- compare_models(lat_models)
    model_comparison <- model_comparison %>%
      arrange(AIC)
    
    print(model_comparison)
    
    # Plot the best model
    
    # Create a data frame with latitude and fitted values
    plot_data_5 <- Ea_values %>%
      mutate(
        abs_lat = abs(latitude),
        fitted_values = predict(Ea_lat_model5, newdata = Ea_values)
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
    
    r_squared <- 0.601
    p_value <- " < 0.0001"
    
    # Plot
    Ea_lat_plot <- ggplot(plot_data_5, aes(x = abs_lat, y = estimate, color = Ea_values$order)) +
      geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
      scale_color_brewer(palette = "YlOrRd") +
      stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                  color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
      labs(x = expression(bold(paste("Latitude (", degree, "N or S"))), y = "Activation Energy (eV)") +
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
    Ea_lat_plot      

# Annual Precipitation
    
    # Linear model
    set.seed(123)
    Ea_ap_model1 <- lm(estimate ~ annual_precip, data = Ea_values)
    summary(Ea_ap_model1)
    
    # Linear model with order 
    set.seed(123)
    Ea_ap_model2 <- lm(estimate ~ annual_precip + order, data = Ea_values)
    summary(Ea_ap_model2)
    
    # Linear model with order (interactor)
    set.seed(123)
    Ea_ap_model3 <- lm(estimate ~ annual_precip * order , data = Ea_values)
    summary(Ea_ap_model3)
    
    # Weighted model 
    set.seed(123)
    Ea_ap_model4 <- lm(estimate ~ annual_precip, data = Ea_values, weights = weights)
    summary(Ea_ap_model4)
    
    # Weighted model with order 
    set.seed(123)
    Ea_ap_model5 <- lm(estimate ~ annual_precip + order, data = Ea_values, weights = weights)
    summary(Ea_ap_model5)
    
    # Weighted model with order (interactor)
    set.seed(123)
    Ea_ap_model6 <- lm(estimate ~ annual_precip * order, data = Ea_values, weights = weights)
    summary(Ea_ap_model6)
    
    # Quadratic model
    set.seed(123)
    Ea_ap_model7 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE), data = Ea_values)
    summary(Ea_ap_model7)
    
    # Quadratic model with order 
    set.seed(123)
    Ea_ap_model8 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE) + order, data = Ea_values)
    summary(Ea_ap_model8)
    
    # Quadratic model with order (interactor)
    set.seed(123)
    Ea_ap_model9 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE) * order, data = Ea_values)
    summary(Ea_ap_model9)
    
    # Weighted quadratic model 
    set.seed(123)
    Ea_ap_model10 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE), data = Ea_values, weights = weights)
    summary(Ea_ap_model10)
    
    # Weighted model with order 
    set.seed(123)
    Ea_ap_model11 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE) + order, data = Ea_values, weights = weights)
    summary(Ea_ap_model11)
    
    # Weighted model with order (interactor)
    set.seed(123)
    Ea_ap_model12 <- lm(estimate ~ poly(annual_precip, 2, raw = TRUE) * order, data = Ea_values, weights = weights)
    summary(Ea_ap_model12)  
    
    # Compare models
    ap_models <- list (
      Ea_ap_model1, 
      Ea_ap_model2,
      Ea_ap_model3,
      Ea_ap_model4,
      Ea_ap_model5,
      Ea_ap_model6,
      Ea_ap_model7,
      Ea_ap_model8, 
      Ea_ap_model9, 
      Ea_ap_model10,
      Ea_ap_model11,
      Ea_ap_model12
    )
    
    model_comparison <- compare_models(ap_models)
    model_comparison <- model_comparison %>%
      arrange(AIC)
    
    print(model_comparison)
    
    # Plot the best model
    
    # Create a data frame with latitude and fitted values
    plot_data_6 <- Ea_values %>%
      mutate(
        annual_precip = annual_precip,
        fitted_values = predict(Ea_ap_model5, newdata = Ea_values)
      )
    
    plot_data_6 <- plot_data_6 %>%
      mutate(
        residuals = estimate - fitted_values
      )
    
    # Estimate the sd of the residuals as a measure of error
    residual_sd_error <- sd(plot_data_6$residuals)
    
    # Calculate error bounds using the residual sd 
    plot_data_6 <- plot_data_6 %>%
      mutate(
        lower_bound = fitted_values - residual_sd_error, 
        upper_bound = fitted_values + residual_sd_error
      )
    
    r_squared <- 0.586
    p_value <- " < 0.0001"
    
    # Plot
    Ea_ap_plot <- ggplot(plot_data_6, aes(x = annual_precip, y = estimate, color = Ea_values$order)) +
      geom_point(aes(size = weights), alpha = 5) +  # Raw data points with size based on weights
      scale_color_brewer(palette = "YlOrRd") +
      stat_smooth(aes(y = fitted_values), method = "lm", formula = y ~ x, se = T, 
                  color = "#E25306", size = 1, alpha = 0.2) +  # Fitted line
      labs(x = expression(bold("Annual Precipitation (mm "*year^{-1}*")")), y = "Activation Energy (eV)") +
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
    Ea_ap_plot     

    # Plot forest plot and histogram side by side
    plot_grid(Ea_lat_plot, Ea_ap_plot,
      labels = c("a)", "b)"),
      label_size = 20, 
      label_fontfamily = "Times New Roman", 
      nrow = 1)

    # Save as PDF and embed fonts
    ggsave("Ea_extra_plots.png", plot = last_plot(), dpi = 1300, width = 15, height = 7.5)    
    