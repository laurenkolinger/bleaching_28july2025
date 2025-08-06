# ============================================================================
# 03: Comprehensive Bleaching Response and Recovery Analysis
# ============================================================================
# Purpose: Analyze coral bleaching response patterns across timepoints,
#          calculate recovery metrics using data-driven quartile classifications,
#          and examine temporal dynamics of coral condition changes.
#          Focus on 2023 Annual → 2024 PBL vs 2024 Annual → 2025 PBL patterns.
# Author: Coral Bleaching Analysis Pipeline
# Date: Analysis of 2023-2025 bleaching events
# Dependencies: Requires outputs from scripts 01 and 02
# ============================================================================

# Load required libraries with explicit purpose for each
library(dplyr)        # Data manipulation and summarization
library(ggplot2)      # Advanced plotting and visualization
library(readr)        # Efficient CSV reading
library(tidyr)        # Data reshaping for analysis
library(viridis)      # Perceptually uniform color scales
library(scales)       # Scale functions for plots
library(gridExtra)    # Multiple plot arrangement
library(corrplot)     # Correlation analysis visualization
library(stringr)      # String manipulation
library(lubridate)    # Date handling if needed

cat("============================================================================\n")
cat("COMPREHENSIVE BLEACHING RESPONSE AND RECOVERY ANALYSIS\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD PROCESSED DATA FROM PREVIOUS ANALYSES
# ============================================================================

cat("STEP 1: Loading processed datasets from previous analyses\n")
cat("---------------------------------------------------------\n")

# Load extent data (primary response variable)
# Rationale: Bleaching extent provides the most direct measure of coral
# response to thermal stress and recovery capacity over time
if(file.exists("01_extent_site_means.csv")) {
  extent_means <- read_csv("01_extent_site_means.csv")
  cat("Loaded extent site means from Step 01\n")
} else {
  stop("Required file 01_extent_site_means.csv not found. Run script 01 first.")
<<<<<<< HEAD
}

# Load prevalence and mortality data for comprehensive analysis
if(file.exists("01_prevalence_site_means.csv")) {
  prevalence_means <- read_csv("01_prevalence_site_means.csv")
  cat("Loaded prevalence site means from Step 01\n")
} else {
  cat("Warning: Prevalence data not found\n")
  prevalence_means <- NULL
}

if(file.exists("01_mortality_site_means.csv")) {
  mortality_means <- read_csv("01_mortality_site_means.csv")
  cat("Loaded mortality site means from Step 01\n")
} else {
  cat("Warning: Mortality data not found\n")
  mortality_means <- NULL
}

# Load thermal stress metrics
if(file.exists("02_comprehensive_thermal_data.csv")) {
  thermal_data <- read_csv("02_comprehensive_thermal_data.csv")
  cat("Loaded comprehensive thermal data from Step 02\n")
} else {
  cat("Warning: Thermal data not found. Analysis will proceed without thermal context.\n")
  thermal_data <- NULL
}

# Load thermal stress thresholds
if(file.exists("02_thermal_stress_thresholds.csv")) {
  thermal_thresholds <- read_csv("02_thermal_stress_thresholds.csv")
  cat("Loaded thermal stress thresholds from Step 02\n")
} else {
  cat("Warning: Thermal thresholds not found\n")
  thermal_thresholds <- NULL
}

cat(sprintf("Primary dataset dimensions: %d rows × %d columns\n", nrow(extent_means), ncol(extent_means)))

# ============================================================================
# KEY TIMEPOINT EXTRACTION AND VALIDATION
# ============================================================================

cat("\nSTEP 2: Extracting and validating key timepoints for response analysis\n")
cat("----------------------------------------------------------------------\n")

# Define the four critical timepoints for response analysis
# Rationale: These timepoints capture two complete bleaching-recovery cycles:
# Cycle 1: 2023 Annual (baseline) → 2024 PBL (post-2023 stress response)
# Cycle 2: 2024 Annual (baseline) → 2025 PBL (post-2024 stress response)
key_timepoints <- c("2023_Annual", "2024_PBL", "2024_Annual", "2025_PBL")

# Extract data for key timepoints
key_timepoint_data <- extent_means %>%
  filter(
    (year == 2023 & period == "Annual") |
      (year == 2024 & period == "PBL") |
      (year == 2024 & period == "Annual") |
      (year == 2025 & period == "PBL")
  ) %>%
  mutate(
    timepoint = paste(year, period, sep = "_"),
    timepoint_ordered = factor(timepoint, levels = key_timepoints)
  )

# Validate data completeness for each timepoint
timepoint_summary <- key_timepoint_data %>%
  group_by(timepoint_ordered) %>%
  summarise(
    n_sites = n(),
    mean_bleaching = mean(ext_anybleaching, na.rm = TRUE),
    median_bleaching = median(ext_anybleaching, na.rm = TRUE),
    sd_bleaching = sd(ext_anybleaching, na.rm = TRUE),
    min_bleaching = min(ext_anybleaching, na.rm = TRUE),
    max_bleaching = max(ext_anybleaching, na.rm = TRUE),
    q25_bleaching = quantile(ext_anybleaching, 0.25, na.rm = TRUE),
    q75_bleaching = quantile(ext_anybleaching, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("Timepoint data summary:\n")
print(timepoint_summary)

# Identify sites with complete temporal coverage
sites_complete_coverage <- key_timepoint_data %>%
  group_by(site) %>%
  summarise(
    timepoints_available = n(),
    available_timepoints = paste(sort(unique(timepoint)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(timepoints_available == 4)

cat(sprintf("\nSites with complete 4-timepoint coverage: %d\n", nrow(sites_complete_coverage)))
cat("These sites will form the core dataset for response analysis.\n")

# ============================================================================
# VISUALIZATION 1: Temporal Bleaching Trajectories
# ============================================================================

# Create comprehensive trajectory visualization
# Justification: Visual inspection of temporal patterns reveals the diversity
# of coral responses and identifies sites with unusual trajectory patterns

# Filter for sites with complete coverage
complete_trajectory_data <- key_timepoint_data %>%
  filter(site %in% sites_complete_coverage$site)

p1 <- ggplot(complete_trajectory_data, aes(x = timepoint_ordered, y = ext_anybleaching)) +
  # Individual site trajectories
  geom_line(aes(group = site), alpha = 0.3, color = "steelblue") +
  geom_point(aes(group = site), alpha = 0.5, size = 1.5, color = "darkblue") +
  
  # Summary statistics overlay
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 2) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red", width = 0.1, size = 1) +
  
  # Add quartile bands
  stat_summary(fun = function(x) quantile(x, 0.25, na.rm = TRUE), 
               geom = "line", aes(group = 1), color = "orange", linetype = "dashed") +
  stat_summary(fun = function(x) quantile(x, 0.75, na.rm = TRUE), 
               geom = "line", aes(group = 1), color = "orange", linetype = "dashed") +
  
  scale_x_discrete(labels = c("2023\nAnnual", "2024\nPBL", "2024\nAnnual", "2025\nPBL")) +
  labs(
    title = "Coral Bleaching Extent Trajectories Across All Sites",
    subtitle = "Individual site trajectories (blue) with mean ± SE (red) and quartiles (orange dashed)",
    x = "Time Period",
    y = "Bleaching Extent (%)",
    caption = "This plot reveals the diversity of coral responses across time.\nEach blue line represents one site's trajectory through bleaching events.\nThe red line shows the mean response with error bars indicating uncertainty.\nOrange dashed lines mark the 25th and 75th percentiles, showing the range\nof typical responses. Divergent patterns may indicate differential resilience."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("03_plot_temporal_trajectories.png", p1, width = 12, height = 8, dpi = 300)
cat("\nSaved: 03_plot_temporal_trajectories.png\n")

# ============================================================================
# RESPONSE METRICS CALCULATION
# ============================================================================

cat("\nSTEP 3: Calculating comprehensive response and recovery metrics\n")
cat("--------------------------------------------------------------\n")

# Create comprehensive response metrics
# Rationale: Multiple metrics capture different aspects of coral responses:
# - Recovery magnitude: absolute change in bleaching extent
# - Recovery efficiency: proportional change relative to initial condition
# - Response direction: improvement vs. deterioration
# - Response consistency: variability across response periods

calculate_response_metrics <- function(initial_bleaching, final_bleaching) {
  # Calculate various response metrics
  # Rationale: Different metrics emphasize different aspects of recovery
  
  # Absolute change (negative = recovery, positive = worsening)
  response_magnitude = final_bleaching - initial_bleaching
  
  # Recovery achieved (always positive, 0 = no recovery)
  recovery_achieved = pmax(0, initial_bleaching - final_bleaching)
  
  # Proportional recovery (relative to initial condition)
  recovery_proportion = ifelse(initial_bleaching > 0, 
                               recovery_achieved / initial_bleaching, 
                               0)
  
  # Persistence of bleaching (what remains after recovery)
  persistence_rate = pmin(initial_bleaching, final_bleaching)
  
  # Worsening magnitude (positive if condition deteriorated)
  worsening_achieved = pmax(0, final_bleaching - initial_bleaching)
  
  return(list(
    response_magnitude = response_magnitude,
    recovery_achieved = recovery_achieved,
    recovery_proportion = recovery_proportion,
    persistence_rate = persistence_rate,
    worsening_achieved = worsening_achieved
  ))
}

=======
}

# Load prevalence and mortality data for comprehensive analysis
if(file.exists("01_prevalence_site_means.csv")) {
  prevalence_means <- read_csv("01_prevalence_site_means.csv")
  cat("Loaded prevalence site means from Step 01\n")
} else {
  cat("Warning: Prevalence data not found\n")
  prevalence_means <- NULL
}

if(file.exists("01_mortality_site_means.csv")) {
  mortality_means <- read_csv("01_mortality_site_means.csv")
  cat("Loaded mortality site means from Step 01\n")
} else {
  cat("Warning: Mortality data not found\n")
  mortality_means <- NULL
}

# Load thermal stress metrics
if(file.exists("02_comprehensive_thermal_data.csv")) {
  thermal_data <- read_csv("02_comprehensive_thermal_data.csv")
  cat("Loaded comprehensive thermal data from Step 02\n")
} else {
  cat("Warning: Thermal data not found. Analysis will proceed without thermal context.\n")
  thermal_data <- NULL
}

# Load thermal stress thresholds
if(file.exists("02_thermal_stress_thresholds.csv")) {
  thermal_thresholds <- read_csv("02_thermal_stress_thresholds.csv")
  cat("Loaded thermal stress thresholds from Step 02\n")
} else {
  cat("Warning: Thermal thresholds not found\n")
  thermal_thresholds <- NULL
}

cat(sprintf("Primary dataset dimensions: %d rows × %d columns\n", nrow(extent_means), ncol(extent_means)))

# ============================================================================
# KEY TIMEPOINT EXTRACTION AND VALIDATION
# ============================================================================

cat("\nSTEP 2: Extracting and validating key timepoints for response analysis\n")
cat("----------------------------------------------------------------------\n")

# Define the four critical timepoints for response analysis
# Rationale: These timepoints capture two complete bleaching-recovery cycles:
# Cycle 1: 2023 Annual (baseline) → 2024 PBL (post-2023 stress response)
# Cycle 2: 2024 Annual (baseline) → 2025 PBL (post-2024 stress response)
key_timepoints <- c("2023_Annual", "2024_PBL", "2024_Annual", "2025_PBL")

# Extract data for key timepoints
key_timepoint_data <- extent_means %>%
  filter(
    (year == 2023 & period == "Annual") |
    (year == 2024 & period == "PBL") |
    (year == 2024 & period == "Annual") |
    (year == 2025 & period == "PBL")
  ) %>%
  mutate(
    timepoint = paste(year, period, sep = "_"),
    timepoint_ordered = factor(timepoint, levels = key_timepoints)
  )

# Validate data completeness for each timepoint
timepoint_summary <- key_timepoint_data %>%
  group_by(timepoint_ordered) %>%
  summarise(
    n_sites = n(),
    mean_bleaching = mean(ext_anybleaching, na.rm = TRUE),
    median_bleaching = median(ext_anybleaching, na.rm = TRUE),
    sd_bleaching = sd(ext_anybleaching, na.rm = TRUE),
    min_bleaching = min(ext_anybleaching, na.rm = TRUE),
    max_bleaching = max(ext_anybleaching, na.rm = TRUE),
    q25_bleaching = quantile(ext_anybleaching, 0.25, na.rm = TRUE),
    q75_bleaching = quantile(ext_anybleaching, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("Timepoint data summary:\n")
print(timepoint_summary)

# Identify sites with complete temporal coverage
sites_complete_coverage <- key_timepoint_data %>%
  group_by(site) %>%
  summarise(
    timepoints_available = n(),
    available_timepoints = paste(sort(unique(timepoint)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(timepoints_available == 4)

cat(sprintf("\nSites with complete 4-timepoint coverage: %d\n", nrow(sites_complete_coverage)))
cat("These sites will form the core dataset for response analysis.\n")

# ============================================================================
# VISUALIZATION 1: Temporal Bleaching Trajectories
# ============================================================================

# Create comprehensive trajectory visualization
# Justification: Visual inspection of temporal patterns reveals the diversity
# of coral responses and identifies sites with unusual trajectory patterns

# Filter for sites with complete coverage
complete_trajectory_data <- key_timepoint_data %>%
  filter(site %in% sites_complete_coverage$site)

p1 <- ggplot(complete_trajectory_data, aes(x = timepoint_ordered, y = ext_anybleaching)) +
  # Individual site trajectories
  geom_line(aes(group = site), alpha = 0.3, color = "steelblue") +
  geom_point(aes(group = site), alpha = 0.5, size = 1.5, color = "darkblue") +
  
  # Summary statistics overlay
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 2) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red", width = 0.1, size = 1) +
  
  # Add quartile bands
  stat_summary(fun = function(x) quantile(x, 0.25, na.rm = TRUE), 
               geom = "line", aes(group = 1), color = "orange", linetype = "dashed") +
  stat_summary(fun = function(x) quantile(x, 0.75, na.rm = TRUE), 
               geom = "line", aes(group = 1), color = "orange", linetype = "dashed") +
  
  scale_x_discrete(labels = c("2023\nAnnual", "2024\nPBL", "2024\nAnnual", "2025\nPBL")) +
  labs(
    title = "Coral Bleaching Extent Trajectories Across All Sites",
    subtitle = "Individual site trajectories (blue) with mean ± SE (red) and quartiles (orange dashed)",
    x = "Time Period",
    y = "Bleaching Extent (%)",
    caption = "This plot reveals the diversity of coral responses across time.\nEach blue line represents one site's trajectory through bleaching events.\nThe red line shows the mean response with error bars indicating uncertainty.\nOrange dashed lines mark the 25th and 75th percentiles, showing the range\nof typical responses. Divergent patterns may indicate differential resilience."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("03_plot_temporal_trajectories.png", p1, width = 12, height = 8, dpi = 300)
cat("\nSaved: 03_plot_temporal_trajectories.png\n")

# ============================================================================
# RESPONSE METRICS CALCULATION
# ============================================================================

cat("\nSTEP 3: Calculating comprehensive response and recovery metrics\n")
cat("--------------------------------------------------------------\n")

# Create comprehensive response metrics
# Rationale: Multiple metrics capture different aspects of coral responses:
# - Recovery magnitude: absolute change in bleaching extent
# - Recovery efficiency: proportional change relative to initial condition
# - Response direction: improvement vs. deterioration
# - Response consistency: variability across response periods

calculate_response_metrics <- function(initial_bleaching, final_bleaching) {
  # Calculate various response metrics
  # Rationale: Different metrics emphasize different aspects of recovery
  
  # Absolute change (negative = recovery, positive = worsening)
  response_magnitude = final_bleaching - initial_bleaching
  
  # Recovery achieved (always positive, 0 = no recovery)
  recovery_achieved = pmax(0, initial_bleaching - final_bleaching)
  
  # Proportional recovery (relative to initial condition)
  recovery_proportion = ifelse(initial_bleaching > 0, 
                              recovery_achieved / initial_bleaching, 
                              0)
  
  # Persistence of bleaching (what remains after recovery)
  persistence_rate = pmin(initial_bleaching, final_bleaching)
  
  # Worsening magnitude (positive if condition deteriorated)
  worsening_achieved = pmax(0, final_bleaching - initial_bleaching)
  
  return(list(
    response_magnitude = response_magnitude,
    recovery_achieved = recovery_achieved,
    recovery_proportion = recovery_proportion,
    persistence_rate = persistence_rate,
    worsening_achieved = worsening_achieved
  ))
}

>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
# Process both response periods
cat("Processing Period 1: 2023 Annual → 2024 PBL (post-2023 stress response)\n")

period1_data <- complete_trajectory_data %>%
  filter(timepoint %in% c("2023_Annual", "2024_PBL")) %>%
  select(site, timepoint, ext_anybleaching) %>%
  pivot_wider(names_from = timepoint, values_from = ext_anybleaching, names_prefix = "bleaching_") %>%
  filter(!is.na(bleaching_2023_Annual) & !is.na(bleaching_2024_PBL)) %>%
  rowwise() %>%
  mutate(
    period = "2023_to_2024_PBL",
    initial_bleaching = bleaching_2023_Annual,
    final_bleaching = bleaching_2024_PBL,
    response_metrics = list(calculate_response_metrics(initial_bleaching, final_bleaching))
  ) %>%
  unnest_wider(response_metrics) %>%
  select(site, period, initial_bleaching, final_bleaching, response_magnitude, 
         recovery_achieved, recovery_proportion, persistence_rate, worsening_achieved)

cat(sprintf("Period 1 analysis: %d sites\n", nrow(period1_data)))

cat("Processing Period 2: 2024 Annual → 2025 PBL (post-2024 stress response)\n")

period2_data <- complete_trajectory_data %>%
  filter(timepoint %in% c("2024_Annual", "2025_PBL")) %>%
  select(site, timepoint, ext_anybleaching) %>%
  pivot_wider(names_from = timepoint, values_from = ext_anybleaching, names_prefix = "bleaching_") %>%
  filter(!is.na(bleaching_2024_Annual) & !is.na(bleaching_2025_PBL)) %>%
  rowwise() %>%
  mutate(
    period = "2024_to_2025_PBL",
    initial_bleaching = bleaching_2024_Annual,
    final_bleaching = bleaching_2025_PBL,
    response_metrics = list(calculate_response_metrics(initial_bleaching, final_bleaching))
  ) %>%
  unnest_wider(response_metrics) %>%
  select(site, period, initial_bleaching, final_bleaching, response_magnitude, 
         recovery_achieved, recovery_proportion, persistence_rate, worsening_achieved)

cat(sprintf("Period 2 analysis: %d sites\n", nrow(period2_data)))

# Combine both periods
combined_response_data <- bind_rows(period1_data, period2_data)

# ============================================================================
# DATA-DRIVEN RESPONSE CATEGORIZATION USING QUARTILES
# ============================================================================

cat("\nSTEP 4: Establishing data-driven response categories using observed quartiles\n")
cat("-----------------------------------------------------------------------------\n")

# Calculate quartiles for key response metrics
# Rationale: Data-driven categories ensure balanced groups and reflect
# the actual distribution of responses in this specific dataset

# Recovery quartiles
recovery_values <- combined_response_data$recovery_achieved[combined_response_data$recovery_achieved > 0]
recovery_quartiles <- quantile(recovery_values, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("Recovery achieved quartiles (among sites showing recovery):\n")
print(recovery_quartiles)

# Response magnitude quartiles (includes both recovery and worsening)
response_mag_quartiles <- quantile(combined_response_data$response_magnitude, 
<<<<<<< HEAD
                                   probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
=======
                                  probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3

cat("\nResponse magnitude quartiles (negative = recovery, positive = worsening):\n")
print(response_mag_quartiles)

# Recovery proportion quartiles (relative recovery)
recovery_prop_values <- combined_response_data$recovery_proportion[combined_response_data$recovery_proportion > 0]
recovery_prop_quartiles <- quantile(recovery_prop_values, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("\nRecovery proportion quartiles (among sites showing recovery):\n")
print(recovery_prop_quartiles)

# Apply data-driven categorizations
combined_response_data <- combined_response_data %>%
  mutate(
    # Recovery magnitude categories (data-driven quartiles)
    recovery_category = case_when(
      recovery_achieved == 0 ~ "No Recovery",
      recovery_achieved <= recovery_quartiles[2] ~ "Minimal Recovery",
      recovery_achieved <= recovery_quartiles[3] ~ "Moderate Recovery",
      recovery_achieved <= recovery_quartiles[4] ~ "Strong Recovery",
      recovery_achieved > recovery_quartiles[4] ~ "Exceptional Recovery",
      TRUE ~ "No Recovery"
    ),
    
    # Response direction categories
    response_direction = case_when(
      response_magnitude < response_mag_quartiles[2] ~ "Strong Recovery",
      response_magnitude < 0 ~ "Moderate Recovery",
      abs(response_magnitude) <= 2 ~ "Stable",
      response_magnitude <= response_mag_quartiles[4] ~ "Moderate Worsening",
      response_magnitude > response_mag_quartiles[4] ~ "Strong Worsening",
      TRUE ~ "Stable"
    ),
    
    # Recovery efficiency categories (proportional)
    recovery_efficiency = case_when(
      recovery_proportion == 0 ~ "No Recovery",
      recovery_proportion <= recovery_prop_quartiles[2] ~ "Low Efficiency",
      recovery_proportion <= recovery_prop_quartiles[3] ~ "Moderate Efficiency", 
      recovery_proportion <= recovery_prop_quartiles[4] ~ "High Efficiency",
      recovery_proportion > recovery_prop_quartiles[4] ~ "Very High Efficiency",
      TRUE ~ "No Recovery"
    ),
    
    # Ordered factors for plotting
    recovery_category = factor(recovery_category, 
<<<<<<< HEAD
                               levels = c("No Recovery", "Minimal Recovery", "Moderate Recovery", 
                                          "Strong Recovery", "Exceptional Recovery")),
    response_direction = factor(response_direction,
                                levels = c("Strong Worsening", "Moderate Worsening", "Stable",
                                           "Moderate Recovery", "Strong Recovery")),
    recovery_efficiency = factor(recovery_efficiency,
                                 levels = c("No Recovery", "Low Efficiency", "Moderate Efficiency",
                                            "High Efficiency", "Very High Efficiency"))
=======
                              levels = c("No Recovery", "Minimal Recovery", "Moderate Recovery", 
                                        "Strong Recovery", "Exceptional Recovery")),
    response_direction = factor(response_direction,
                               levels = c("Strong Worsening", "Moderate Worsening", "Stable",
                                         "Moderate Recovery", "Strong Recovery")),
    recovery_efficiency = factor(recovery_efficiency,
                                levels = c("No Recovery", "Low Efficiency", "Moderate Efficiency",
                                          "High Efficiency", "Very High Efficiency"))
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
  )

# Print category thresholds for documentation
cat("\nData-driven response category thresholds:\n")
cat(sprintf("Recovery categories (based on absolute reduction in bleaching %%):\n"))
cat(sprintf("  Minimal: 0 - %.1f%%\n", recovery_quartiles[2]))
cat(sprintf("  Moderate: %.1f - %.1f%%\n", recovery_quartiles[2], recovery_quartiles[3]))
cat(sprintf("  Strong: %.1f - %.1f%%\n", recovery_quartiles[3], recovery_quartiles[4]))
cat(sprintf("  Exceptional: >%.1f%%\n", recovery_quartiles[4]))

# ============================================================================
# VISUALIZATION 2: Recovery Distribution Analysis
# ============================================================================

# Create comprehensive recovery distribution plots
# Justification: Understanding recovery distributions validates our quartile-based
# categories and reveals patterns between the two time periods

recovery_comparison_data <- combined_response_data %>%
  mutate(
    period_label = ifelse(period == "2023_to_2024_PBL", 
<<<<<<< HEAD
                          "2023 Annual → 2024 PBL", 
                          "2024 Annual → 2025 PBL")
=======
                         "2023 Annual → 2024 PBL", 
                         "2024 Annual → 2025 PBL")
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
  )

p2 <- ggplot(recovery_comparison_data, aes(x = period_label, y = recovery_achieved, fill = period_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2.5) +
  geom_hline(yintercept = recovery_quartiles[2:4], linetype = "dashed", 
             color = c("green", "orange", "red"), alpha = 0.7) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  labs(
    title = "Recovery Magnitude Distribution by Time Period",
    subtitle = "Dashed lines show data-driven category thresholds (green=minimal, orange=moderate, red=strong)",
    x = "Time Period",
    y = "Recovery Achieved (% reduction in bleaching)",
    caption = "This plot compares recovery magnitudes between the two analysis periods.\nData-driven thresholds (dashed lines) are based on observed quartiles,\nensuring balanced category sizes. Differences between periods may reflect\nchanging environmental conditions, coral acclimatization, or cumulative\nstress effects. Outliers may represent sites with exceptional resilience."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("03_plot_recovery_distributions.png", p2, width = 10, height = 7, dpi = 300)
cat("Saved: 03_plot_recovery_distributions.png\n")

# ============================================================================
# VISUALIZATION 3: Response Category Composition
# ============================================================================

# Create stacked bar chart of response categories
# Justification: Shows the composition of response types across periods
# and validates that quartile-based categories create meaningful groups

category_summary <- recovery_comparison_data %>%
  group_by(period_label, recovery_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(period_label) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  )

p3 <- ggplot(category_summary, aes(x = period_label, y = percentage, fill = recovery_category)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = sprintf("n=%d", count)), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_viridis_d(name = "Recovery\nCategory", option = "plasma") +
  labs(
    title = "Recovery Category Composition by Time Period",
    subtitle = "Categories based on observed data quartiles, ensuring balanced representation",
    x = "Time Period",
    y = "Percentage of Sites (%)",
    caption = "This stacked bar chart shows the distribution of recovery categories\nacross time periods. Quartile-based categories ensure each category\nrepresents approximately 25% of recovering sites. Shifts between\nperiods indicate changing recovery dynamics that could reflect\nenvironmental conditions, coral adaptation, or legacy effects."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("03_plot_recovery_categories.png", p3, width = 10, height = 7, dpi = 300)
cat("Saved: 03_plot_recovery_categories.png\n")

# ============================================================================
# SITE-SPECIFIC RESPONSE PATTERN ANALYSIS
# ============================================================================

cat("\nSTEP 5: Analyzing site-specific response patterns across both periods\n")
cat("--------------------------------------------------------------------\n")

# Compare response patterns between periods for each site
# Rationale: Sites may show consistent or variable responses across periods,
# revealing different resilience strategies or stress sensitivities

site_response_patterns <- period1_data %>%
  inner_join(period2_data, by = "site", suffix = c("_p1", "_p2")) %>%
  mutate(
    # Response consistency metrics
    recovery_change = recovery_achieved_p2 - recovery_achieved_p1,
    response_consistency = abs(recovery_achieved_p2 - recovery_achieved_p1),
    
    # Pattern classification based on data-driven thresholds
    pattern_classification = case_when(
      recovery_achieved_p1 > recovery_quartiles[3] & recovery_achieved_p2 > recovery_quartiles[3] ~ "Consistent Strong Recovery",
      recovery_achieved_p1 > recovery_quartiles[2] & recovery_achieved_p2 > recovery_quartiles[2] ~ "Consistent Moderate Recovery",
      recovery_achieved_p1 > recovery_quartiles[3] & recovery_achieved_p2 <= recovery_quartiles[2] ~ "Declining Recovery Capacity",
      recovery_achieved_p1 <= recovery_quartiles[2] & recovery_achieved_p2 > recovery_quartiles[3] ~ "Improving Recovery Capacity",
      response_magnitude_p1 > 0 & response_magnitude_p2 > 0 ~ "Consistent Deterioration",
      abs(response_magnitude_p1) <= 2 & abs(response_magnitude_p2) <= 2 ~ "Consistently Stable",
      TRUE ~ "Variable Response"
    ),
    
    # Recovery trajectory classification
    recovery_trajectory = case_when(
      recovery_change > recovery_quartiles[3] - recovery_quartiles[2] ~ "Accelerating Recovery",
      recovery_change < -(recovery_quartiles[3] - recovery_quartiles[2]) ~ "Decelerating Recovery",
      TRUE ~ "Stable Recovery Rate"
    )
  )

cat("Site response pattern summary:\n")
pattern_summary <- site_response_patterns %>%
  count(pattern_classification) %>%
  arrange(desc(n))
print(pattern_summary)

cat("\nRecovery trajectory summary:\n")
trajectory_summary <- site_response_patterns %>%
  count(recovery_trajectory) %>%
  arrange(desc(n))
print(trajectory_summary)

# ============================================================================
# VISUALIZATION 4: Site Response Pattern Scatter Plot
# ============================================================================

# Create scatter plot comparing responses between periods
# Justification: Reveals correlations, outliers, and different response strategies

p4 <- ggplot(site_response_patterns, aes(x = recovery_achieved_p1, y = recovery_achieved_p2, 
<<<<<<< HEAD
                                         color = pattern_classification)) +
=======
                                        color = pattern_classification)) +
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = recovery_quartiles[2:4], linetype = "dotted", alpha = 0.5, color = "blue") +
  geom_vline(xintercept = recovery_quartiles[2:4], linetype = "dotted", alpha = 0.5, color = "blue") +
  scale_color_viridis_d(name = "Response\nPattern") +
  labs(
    title = "Site Response Consistency: Period 1 vs Period 2",
    subtitle = "Diagonal line shows equal recovery; dotted lines mark quartile thresholds",
    x = "Period 1 Recovery (2023 Annual → 2024 PBL, %)",
    y = "Period 2 Recovery (2024 Annual → 2025 PBL, %)",
    caption = "This plot reveals site-specific response consistency across periods.\nPoints on the diagonal line show equal recovery in both periods.\nPoints above the line recovered more in Period 2, while points below\nrecovered more in Period 1. Dotted lines mark quartile-based category\nthresholds. Distinct patterns may indicate different resilience strategies."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))

ggsave("03_plot_response_consistency.png", p4, width = 12, height = 8, dpi = 300)
cat("Saved: 03_plot_response_consistency.png\n")

# ============================================================================
# THERMAL STRESS INTEGRATION
# ============================================================================

cat("\nSTEP 6: Integrating thermal stress data with response patterns\n")
cat("-------------------------------------------------------------\n")

if(!is.null(thermal_data)) {
  # Merge response data with thermal stress metrics
  # Rationale: Understanding how thermal stress relates to recovery patterns
  # provides mechanistic insights into coral resilience
  
  # Prepare thermal data for merging
  thermal_summary <- thermal_data %>%
    select(site, year, max_dhw, temp_sd, total_dhw_accumulation) %>%
    pivot_wider(names_from = year, 
                values_from = c(max_dhw, temp_sd, total_dhw_accumulation),
                names_sep = "_")
  
  # Merge with response patterns
  response_thermal_data <- site_response_patterns %>%
    left_join(thermal_summary, by = "site") %>%
    mutate(
      # Calculate thermal stress indices
      cumulative_dhw = max_dhw_2023 + max_dhw_2024,
      thermal_variability = temp_sd_2023 + temp_sd_2024,
      stress_escalation = max_dhw_2024 - max_dhw_2023
    )
  
  cat("Thermal-response integration completed for", nrow(response_thermal_data), "sites\n")
  
  # ========================================================================
  # VISUALIZATION 5: Thermal Stress vs Recovery Analysis
  # ========================================================================
  
  # Create comprehensive thermal stress vs recovery plot
  # Justification: Reveals whether high thermal stress inhibits recovery
  # or if other factors are more important for resilience
  
  p5 <- ggplot(response_thermal_data, aes(x = max_dhw_2024, y = recovery_achieved_p2, 
<<<<<<< HEAD
                                          color = thermal_variability, size = cumulative_dhw)) +
=======
                                         color = thermal_variability, size = cumulative_dhw)) +
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", size = 1) +
    scale_color_viridis_c(name = "Temperature\nVariability\n(2-year sum)") +
    scale_size_continuous(name = "Cumulative\nDHW\n(2-year sum)", range = c(2, 8)) +
    labs(
      title = "2024 Thermal Stress vs 2024→2025 Recovery Response",
      subtitle = "Point size = cumulative 2-year DHW; color = temperature variability",
      x = "2024 Maximum DHW",
      y = "Recovery Achieved 2024→2025 (%)",
      caption = "This plot examines the relationship between thermal stress and recovery.\nThe red dashed line shows the overall trend. Point size indicates\ncumulative thermal stress over both years, while color shows temperature\nvariability. Strong negative relationships would suggest thermal stress\nlimits recovery, while weak relationships indicate other factors dominate."
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))
  
  ggsave("03_plot_thermal_vs_recovery.png", p5, width = 12, height = 8, dpi = 300)
  cat("Saved: 03_plot_thermal_vs_recovery.png\n")
  
  # Calculate correlation between thermal stress and recovery
  thermal_recovery_correlation <- cor(response_thermal_data$max_dhw_2024, 
<<<<<<< HEAD
                                      response_thermal_data$recovery_achieved_p2, 
                                      use = "complete.obs")
=======
                                     response_thermal_data$recovery_achieved_p2, 
                                     use = "complete.obs")
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
  
  cat(sprintf("Correlation between 2024 DHW and 2024→2025 recovery: %.3f\n", thermal_recovery_correlation))
  
} else {
  cat("Thermal data not available - skipping thermal integration analysis\n")
  response_thermal_data <- site_response_patterns
}

# ============================================================================
# EXTREME RESPONDER IDENTIFICATION
# ============================================================================

cat("\nSTEP 7: Identifying and characterizing extreme responders\n")
cat("--------------------------------------------------------\n")

# Identify sites with exceptional or concerning responses
# Rationale: Extreme responders (both positive and negative) may provide
# insights into resilience mechanisms or vulnerability factors

# Define extreme responders using data-driven thresholds
extreme_responders <- site_response_patterns %>%
  mutate(
    extreme_category = case_when(
      recovery_achieved_p2 > recovery_quartiles[4] ~ "Exceptional Recovery",
      worsening_achieved_p2 > quantile(combined_response_data$worsening_achieved, 0.9, na.rm = TRUE) ~ "Severe Worsening",
<<<<<<< HEAD
      response_consistency < quantile(site_response_patterns$response_consistency, 0.1, na.rm = TRUE) ~ "Highly Consistent",
      response_consistency > quantile(site_response_patterns$response_consistency, 0.9, na.rm = TRUE) ~ "Highly Variable",
=======
      recovery_consistency < quantile(site_response_patterns$response_consistency, 0.1, na.rm = TRUE) ~ "Highly Consistent",
      recovery_consistency > quantile(site_response_patterns$response_consistency, 0.9, na.rm = TRUE) ~ "Highly Variable",
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
      TRUE ~ "Typical Response"
    )
  ) %>%
  filter(extreme_category != "Typical Response")

cat("Extreme responder categories:\n")
extreme_summary <- extreme_responders %>%
  count(extreme_category) %>%
  arrange(desc(n))
print(extreme_summary)

if(nrow(extreme_responders) > 0) {
  cat("\nDetailed extreme responder analysis:\n")
  for(category in unique(extreme_responders$extreme_category)) {
    cat(sprintf("\n%s sites:\n", category))
    category_sites <- extreme_responders %>%
      filter(extreme_category == category) %>%
      arrange(desc(recovery_achieved_p2))
    
    for(i in 1:min(5, nrow(category_sites))) {
      site_data <- category_sites[i, ]
      cat(sprintf("  %s: P1 recovery=%.1f%%, P2 recovery=%.1f%%\n",
                  site_data$site, site_data$recovery_achieved_p1, site_data$recovery_achieved_p2))
    }
  }
}

# ============================================================================
# SAVE COMPREHENSIVE RESPONSE ANALYSIS RESULTS
# ============================================================================

cat("\nSTEP 8: Saving comprehensive response analysis results\n")
cat("------------------------------------------------------\n")

# Save all response analysis datasets
write_csv(combined_response_data, "03_combined_response_metrics.csv")
write_csv(site_response_patterns, "03_site_response_patterns.csv")
write_csv(extreme_responders, "03_extreme_responders.csv")

if(!is.null(thermal_data)) {
  write_csv(response_thermal_data, "03_response_thermal_integrated.csv")
}

# Save response category thresholds for documentation
response_thresholds_data <- data.frame(
  metric = c("recovery_minimal", "recovery_moderate", "recovery_strong", "recovery_exceptional",
             "response_strong_recovery", "response_stable", "response_strong_worsening"),
  threshold = c(recovery_quartiles[2], recovery_quartiles[3], recovery_quartiles[4], recovery_quartiles[5],
                response_mag_quartiles[2], 2, response_mag_quartiles[4]),
  percentile = c("25th", "50th", "75th", "100th", "25th", "~50th", "75th"),
  description = c("Minimal recovery threshold", "Moderate recovery threshold", 
<<<<<<< HEAD
                  "Strong recovery threshold", "Maximum recovery observed",
                  "Strong recovery threshold", "Stability threshold (±2%)",
                  "Strong worsening threshold")
=======
                 "Strong recovery threshold", "Maximum recovery observed",
                 "Strong recovery threshold", "Stability threshold (±2%)",
                 "Strong worsening threshold")
>>>>>>> f1725d6fce25375293039a1c314c3c9560b0a9a3
)
write_csv(response_thresholds_data, "03_response_category_thresholds.csv")

# ============================================================================
# FINAL SUMMARY STATISTICS
# ============================================================================

cat("\nFINAL RESPONSE ANALYSIS SUMMARY\n")
cat("===============================\n")

cat(sprintf("Sites analyzed: %d\n", length(unique(combined_response_data$site))))
cat(sprintf("Total site-period combinations: %d\n", nrow(combined_response_data)))

# Period 1 summary
period1_summary <- combined_response_data %>% filter(period == "2023_to_2024_PBL")
cat(sprintf("\nPeriod 1 (2023 Annual → 2024 PBL) summary:\n"))
cat(sprintf("  Mean recovery: %.1f%%\n", mean(period1_summary$recovery_achieved, na.rm = TRUE)))
cat(sprintf("  Sites with >10%% recovery: %d (%.1f%%)\n", 
            sum(period1_summary$recovery_achieved > 10), 
            sum(period1_summary$recovery_achieved > 10) / nrow(period1_summary) * 100))
cat(sprintf("  Sites with worsening: %d (%.1f%%)\n",
            sum(period1_summary$response_magnitude > 0),
            sum(period1_summary$response_magnitude > 0) / nrow(period1_summary) * 100))

# Period 2 summary  
period2_summary <- combined_response_data %>% filter(period == "2024_to_2025_PBL")
cat(sprintf("\nPeriod 2 (2024 Annual → 2025 PBL) summary:\n"))
cat(sprintf("  Mean recovery: %.1f%%\n", mean(period2_summary$recovery_achieved, na.rm = TRUE)))
cat(sprintf("  Sites with >10%% recovery: %d (%.1f%%)\n",
            sum(period2_summary$recovery_achieved > 10),
            sum(period2_summary$recovery_achieved > 10) / nrow(period2_summary) * 100))
cat(sprintf("  Sites with worsening: %d (%.1f%%)\n",
            sum(period2_summary$response_magnitude > 0),
            sum(period2_summary$response_magnitude > 0) / nrow(period2_summary) * 100))

cat("\nResponse category thresholds (data-driven quartiles):\n")
for(i in 1:4) {
  cat(sprintf("  %s recovery: >%.1f%% (%s percentile)\n",
              c("Minimal", "Moderate", "Strong", "Exceptional")[i],
              c(0, recovery_quartiles[2:4])[i+1],
              c("0th", "25th", "50th", "75th")[i+1]))
}

cat("\nFiles saved:\n")
cat("  - 03_combined_response_metrics.csv\n")
cat("  - 03_site_response_patterns.csv\n")
cat("  - 03_extreme_responders.csv\n")
if(!is.null(thermal_data)) {
  cat("  - 03_response_thermal_integrated.csv\n")
}
cat("  - 03_response_category_thresholds.csv\n")

cat("\nVisualizations saved:\n")
cat("  - 03_plot_temporal_trajectories.png\n")
cat("  - 03_plot_recovery_distributions.png\n")
cat("  - 03_plot_recovery_categories.png\n")
cat("  - 03_plot_response_consistency.png\n")
if(!is.null(thermal_data)) {
  cat("  - 03_plot_thermal_vs_recovery.png\n")
}

cat("\n============================================================================\n")
cat("STEP 3 COMPLETE: Comprehensive bleaching response analysis using data-driven thresholds\n")
cat("Next: Predictive analysis comparing different predictor variables\n")
cat("============================================================================\n")