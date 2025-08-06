# Visualization Analysis: Coral Bleaching Response and Recovery Patterns
# Comprehensive visualization of bleaching responses, recovery patterns, and predictive relationships

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)
library(viridis)
library(corrplot)

# Load all processed data
extent_means <- read_csv("01_extent_site_means.csv")
temp_metrics <- read_csv("02_temperature_metrics_2023_2024.csv")
recovery_data <- read_csv("03_recovery_with_thermal_data.csv")
predictive_data <- read_csv("04_predictive_dataset.csv")
site_patterns <- read_csv("03_site_response_patterns.csv")

# Set theme for all plots
theme_coral <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold")
  )

# 1. Temporal bleaching trajectories for all sites
temporal_data <- extent_means %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  mutate(
    timepoint = case_when(
      year == 2023 & period == "Annual" ~ 1,
      year == 2024 & period == "PBL" ~ 2,
      year == 2024 & period == "Annual" ~ 3,
      year == 2025 & period == "PBL" ~ 4
    ),
    timepoint_label = case_when(
      timepoint == 1 ~ "2023 Annual",
      timepoint == 2 ~ "2024 PBL",
      timepoint == 3 ~ "2024 Annual", 
      timepoint == 4 ~ "2025 PBL"
    )
  ) %>%
  filter(!is.na(timepoint))

p1 <- ggplot(temporal_data, aes(x = timepoint, y = ext_anybleaching, group = site)) +
  geom_line(alpha = 0.5, color = "steelblue") +
  geom_point(alpha = 0.7, size = 1.5, color = "darkblue") +
  scale_x_continuous(breaks = 1:4, labels = c("2023 Annual", "2024 PBL", "2024 Annual", "2025 PBL")) +
  labs(
    title = "Bleaching Extent Trajectories Across All Sites",
    subtitle = "Individual site trajectories from 2023 to 2025",
    x = "Time Period",
    y = "Bleaching Extent (%)"
  ) +
  theme_coral +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Recovery patterns by period
recovery_comparison <- recovery_data %>%
  mutate(
    period_label = ifelse(period == "2023_to_2024_PBL", "2023 Annual → 2024 PBL", "2024 Annual → 2025 PBL")
  )

p2 <- ggplot(recovery_comparison, aes(x = period_label, y = recovery_rate, fill = period_label)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  labs(
    title = "Recovery Rates by Time Period",
    subtitle = "Distribution of recovery rates across sites",
    x = "Time Period",
    y = "Recovery Rate (% reduction in bleaching)"
  ) +
  theme_coral +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# 3. DHW vs Bleaching Response
p3 <- ggplot(predictive_data, aes(x = dhw_2024, y = response_magnitude)) +
  geom_point(aes(color = predictor_2023_annual), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  scale_color_viridis_c(name = "2023 Bleaching\nExtent (%)") +
  labs(
    title = "2024 DHW vs 2024→2025 Response",
    subtitle = "Points colored by 2023 bleaching extent",
    x = "2024 Maximum DHW",
    y = "Response Magnitude (2025 PBL - 2024 Annual)"
  ) +
  theme_coral

# 4. Previous year bleaching vs current response
p4 <- ggplot(predictive_data, aes(x = predictor_2023_annual, y = response_magnitude)) +
  geom_point(aes(color = dhw_2024), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  scale_color_viridis_c(name = "2024 DHW") +
  labs(
    title = "2023 Bleaching vs 2024→2025 Response", 
    subtitle = "Points colored by 2024 DHW",
    x = "2023 Annual Bleaching Extent (%)",
    y = "Response Magnitude (2025 PBL - 2024 Annual)"
  ) +
  theme_coral

# 5. Site response pattern classification
pattern_summary <- site_patterns %>%
  count(response_pattern) %>%
  mutate(
    response_pattern = factor(response_pattern),
    percentage = round(n / sum(n) * 100, 1)
  )

p5 <- ggplot(pattern_summary, aes(x = reorder(response_pattern, n), y = n, fill = response_pattern)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")), hjust = -0.1, size = 3.5) +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(
    title = "Site Response Pattern Classification",
    subtitle = "Number and percentage of sites in each response category",
    x = "Response Pattern",
    y = "Number of Sites"
  ) +
  theme_coral +
  theme(legend.position = "none")

# 6. Temperature variability vs bleaching response
temp_var_plot_data <- predictive_data %>%
  filter(!is.na(temp_instability))

p6 <- ggplot(temp_var_plot_data, aes(x = temp_instability, y = abs(response_magnitude))) +
  geom_point(aes(color = factor(response_magnitude > 0, labels = c("Recovery", "Worsening"))), 
             size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  scale_color_manual(values = c("Recovery" = "blue", "Worsening" = "red"), name = "Response\nDirection") +
  labs(
    title = "Temperature Instability vs Response Magnitude",
    subtitle = "Temperature variability effect on bleaching response intensity",
    x = "Temperature Instability (2023 + 2024 SD)",
    y = "Absolute Response Magnitude"
  ) +
  theme_coral

# 7. Site-specific detailed analysis - top performers
top_recovery_sites <- predictive_data %>%
  arrange(desc(recovery_achieved)) %>%
  slice_head(n = 8)

p7_data <- extent_means %>%
  filter(site %in% top_recovery_sites$site) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  mutate(
    timepoint = case_when(
      year == 2023 & period == "Annual" ~ 1,
      year == 2024 & period == "PBL" ~ 2,
      year == 2024 & period == "Annual" ~ 3,
      year == 2025 & period == "PBL" ~ 4
    )
  ) %>%
  filter(!is.na(timepoint))

p7 <- ggplot(p7_data, aes(x = timepoint, y = ext_anybleaching, color = site)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:4, labels = c("2023 Annual", "2024 PBL", "2024 Annual", "2025 PBL")) +
  scale_color_viridis_d() +
  labs(
    title = "Top Recovery Sites - Detailed Trajectories",
    subtitle = "Sites with highest recovery rates (2024 Annual → 2025 PBL)",
    x = "Time Period",
    y = "Bleaching Extent (%)",
    color = "Site"
  ) +
  theme_coral +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. Correlation matrix visualization
correlation_vars <- predictive_data %>%
  select(response_magnitude, recovery_achieved, predictor_2023_annual, dhw_2023, dhw_2024, 
         cumulative_dhw, temp_instability, bleaching_persistence) %>%
  rename(
    "2024→2025 Response" = response_magnitude,
    "Recovery Achieved" = recovery_achieved,
    "2023 Bleaching" = predictor_2023_annual,
    "2023 DHW" = dhw_2023,
    "2024 DHW" = dhw_2024,
    "Cumulative DHW" = cumulative_dhw,
    "Temp Instability" = temp_instability,
    "Bleaching Persistence" = bleaching_persistence
  ) %>%
  filter(complete.cases(.))

correlation_matrix <- cor(correlation_vars, use = "complete.obs")

# Save plots
ggsave("05_plot_bleaching_trajectories.png", p1, width = 12, height = 8, dpi = 300)
ggsave("05_plot_recovery_comparison.png", p2, width = 10, height = 6, dpi = 300)
ggsave("05_plot_dhw_vs_response.png", p3, width = 10, height = 7, dpi = 300)
ggsave("05_plot_previous_bleaching_vs_response.png", p4, width = 10, height = 7, dpi = 300)
ggsave("05_plot_response_patterns.png", p5, width = 12, height = 6, dpi = 300)
ggsave("05_plot_temperature_variability.png", p6, width = 10, height = 7, dpi = 300)
ggsave("05_plot_top_recovery_sites.png", p7, width = 12, height = 8, dpi = 300)

# Save correlation matrix plot
png("05_plot_correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix: Bleaching Response Variables", 
         mar = c(0,0,2,0))
dev.off()

# Create summary dashboard
dashboard_plot <- grid.arrange(
  p1, p2, p3, p4,
  ncol = 2, nrow = 2,
  top = "Coral Bleaching Response Analysis Dashboard"
)

ggsave("05_dashboard_summary.png", dashboard_plot, width = 16, height = 12, dpi = 300)

cat("Visualization analysis complete. Generated plots:\n")
cat("- 05_plot_bleaching_trajectories.png\n")
cat("- 05_plot_recovery_comparison.png\n") 
cat("- 05_plot_dhw_vs_response.png\n")
cat("- 05_plot_previous_bleaching_vs_response.png\n")
cat("- 05_plot_response_patterns.png\n")
cat("- 05_plot_temperature_variability.png\n")
cat("- 05_plot_top_recovery_sites.png\n")
cat("- 05_plot_correlation_matrix.png\n")
cat("- 05_dashboard_summary.png\n")