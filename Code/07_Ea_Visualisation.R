### MRes Summer Project -- Size Matters: The Impact of Body Size on the Thermal Sensitivity of Fecundity

### 4 - Plotting Distributions of Ea estimates 

# This script produces a forest plot and histogram to show the distribution of
# Ea estimates and compare it to the MTE standard

rm(list=ls())

# Set working directory 
setwd("~/Desktop/Masters/Summer Project/Data") 

require('dplyr')
require('ggplot2')
require('scales')
require('cowplot')
require('viridis')
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

# Check for median 
summary(Ea_values$estimate)
summary(log10(Ea_values$mean_mass))

# Remove any NA
Ea_values <- Ea_values %>% 
  filter(!is.na(species))

## Forest plot

# Define color mapping for the orders
color_map <- brewer.pal(length(unique(Ea_values$order)), "YlOrRd")
names(color_map) <- unique(Ea_values$order)

# Convert `species` to a factor, ordered by `order` and `species`
Ea_values <- Ea_values %>%
  arrange(order, species) %>%
  mutate(
    species = factor(species, levels = unique(species)),  # Ensure species are ordered within each order
    species_numeric = as.numeric(factor(species))
  )

# Create a data frame for blocks with color mapping
block_data <- Ea_values %>%
  group_by(order) %>%
  summarize(
    ymin = min(species_numeric) - 0.5,
    ymax = max(species_numeric) + 0.5
  ) %>%
  ungroup()  # Ensure no grouping remains for plotting

# Add color mapping to the block data
block_data$color <- color_map[block_data$order]

# Create the plot
forest_Ea <- ggplot() +
  geom_rect(data = block_data, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = order), alpha = 0.6, color = NA) +  # Add background blocks
  geom_errorbarh(data = Ea_values, aes(xmin = conf_lower, xmax = conf_upper, y = species), height = 0.5, color = "black") +  # Dark grey error bars
  geom_point(data = Ea_values, aes(x = estimate, y = species), size = 3, color = "black") +  # Color dots by order
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Activation Energy (eV)", y = "Species", fill = "Order") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.title.x = element_text(size=25, face = "bold", family = "Times New Roman"), 
        axis.title.y = element_text(size=25, face = "bold", family = "Times New Roman", margin = margin(r = -5)), 
        axis.text.y = element_text(size = 22, color = "black", face = "italic", family = "Times New Roman"), 
        axis.text.x = element_text(size = 22, face = "bold", colour = "black",  family = "Times New Roman"), 
        legend.position = "top",
        legend.margin = margin(l = -200),
        legend.title = element_text(size = 20, face = "bold", family = "Times New Roman"), 
        legend.text = element_text(size = 18, colour = "black", family = "Times New Roman"), 
        legend.box = "horizontal",  # Ensure legend is in one row
        legend.spacing.x = unit(0.5, 'cm'),  # Adjust spacing between legend items if necessary
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(values = color_map, guide = guide_legend(nrow = 1)) +  # Apply the same color mapping to the blocks
  scale_color_manual(values = color_map)  # Apply custom colors for the points

# Add segments with annotate() to avoid data frame issues
forest_Ea <- forest_Ea +
  annotate("segment", x = 0.65, xend = 0.65, y = -Inf, yend = Inf, color = "black", lty = 1, size = 0.5) +
  annotate("segment", x = 1.03, xend = 1.03, y = -Inf, yend = Inf, color = "black", lty = 2, size = 0.5)

forest_Ea

## Histogram of activation energy
hist(Ea_values$estimate, breaks = 15)

hist_Ea <- ggplot(Ea_values, aes(x = estimate)) +
  geom_histogram( bins = 10, fill = "grey", colour = "black", size = 1, alpha = 0.2) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = "Activation Energy (eV)", y = "Count") +
  geom_segment(aes(x = 0.65, y = 0, xend = 0.65, yend = 7), color = "#E25306", lty = 1, size = 1) +
  geom_segment(aes(x = 1.03, y = 0, xend = 1.03, yend = 7), color = "#E25306", lty = 2, size = 1) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.title.x = element_text(size=25, face = "bold", family = "Times New Roman"), 
        axis.title.y = element_text(size=25, face = "bold", family = "Times New Roman"), 
        axis.text = element_text(size = 22, face = "bold", colour = "black", family = "Times New Roman"))
hist_Ea

## Histogram of size 
hist(log10(Ea_values$mean_mass), breaks = 15)

hist_mass <- ggplot(Ea_values, aes(x = log10(mean_mass))) +
  geom_histogram( bins = 20, fill = "grey", colour = "black", size = 1, alpha = 0.2) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = bquote(bold(log[10] * " Mass (mg)")), y = "Count") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.title.x = element_text(size=25, face = "bold", family = "Times New Roman"), 
        axis.title.y = element_text(size=25, face = "bold", family = "Times New Roman"), 
        axis.text = element_text(size = 22, face = "bold", colour = "black", family = "Times New Roman"))
hist_mass

# Create a white spacer plot
white_spacer <- ggdraw() + 
  theme_void() +  # Use a void theme to remove axes and labels
  theme(plot.background = element_rect(fill = "white", color = "white"))  # Set background to white

# Combine plots
plot_grid(
  forest_Ea,  # The forest plot alone on the first row
  plot_grid(
    white_spacer,  # Use white_spacer for white space
    plot_grid(hist_Ea, hist_mass, ncol = 2, labels = c("b)", "c)"), 
              label_size = 20, label_fontfamily = "Times New Roman"),  # Combine histograms side by side on the second row
    ncol = 1,
    rel_heights = c(0.1, 1)  # Adjust heights to add space (0.1 for spacer, 1 for histograms)
  ),
  labels = c("a)", ""),  # Label for forest_Ea and leave blank for the second row
  label_size = 20, 
  label_fontfamily = "Times New Roman", 
  nrow = 2, 
  rel_widths = c(1.5, 0.3), rel_heights = c(2, 1)  # Adjust relative widths: 1.5 for the forest plot, 1 for the histograms
)

# Save as PDF and embed fonts
ggsave("distributions.png", plot = last_plot(), dpi = 1300, width = 15, height = 19.5)

## Bootstrapping to determine significance 

# Set seed 
set.seed(123)

# Set the MTE reference value
MTE_median <- 0.65

# Number of bootstrap samples 
n_bootstrap <- 1000

# Storage for bootstrap medians 
bootstrap_medians <- numeric(n_bootstrap)

# Bootstrap sampling 
for (i in 1:n_bootstrap) {
  resample <- sample(Ea_values$estimate, length(Ea_values), replace = TRUE)
  bootstrap_medians[i] <- median(resample)
}

# Calculate 95% CI for the median 
ci_median <- quantile(bootstrap_medians, c(0.025, 0.975))

# Output results 
cat("Bootstrapped 95% CI for Median:", ci_median, "\n")
cat("Reference Median:", MTE_median, "\n")

# Check if the reference median is outside the CI
if (MTE_median < ci_median[1] || MTE_median > ci_median[2]) {
  cat("The median of the distribution is signficantly different from the 
      reference median. \n")
} else {
  cat("The median of the distribution is not significantly different from the 
      reference median. \n")
}

