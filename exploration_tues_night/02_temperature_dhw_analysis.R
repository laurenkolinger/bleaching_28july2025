# ============================================================================
  # 02: Comprehensive Temperature and DHW Analysis for 2023-2024 Bleaching Events
  # ============================================================================
# Purpose: Analyze thermal stress patterns, temperature variability, and
#          calculate comprehensive DHW metrics using data-driven quartile
#          classifications. Establish thermal stress categories based on
#          observed data distributions rather than literature thresholds.
# Author: Coral Bleaching Analysis Pipeline
# Date: Analysis of 2023-2025 bleaching events
# Dependencies: Requires 01_severity_thresholds.csv from previous script
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
library(lubridate)    # Date manipulation (if needed)

cat("============================================================================\n")
cat("TEMPERATURE AND DHW ANALYSIS FOR 2023-2024 BLEACHING EVENTS\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD DATA AND INITIAL EXPLORATION
# ============================================================================

cat("STEP 1: Loading temperature data and bleaching thresholds\n")
cat("---------------------------------------------------------\n")

# Load temperature data
# Rationale: Temperature and DHW data provide the thermal stress context
# that drives bleaching responses. Weekly resolution allows detailed analysis
# of stress accumulation patterns and temperature variability.
temp_data <- read_csv("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

# Load severity thresholds from previous analysis
# Rationale: Using consistent, data-driven thresholds ensures coherent
# classification across all analysis components
if(file.exists("01_severity_thresholds.csv")) {
  severity_thresholds <- read_csv("01_severity_thresholds.csv")
  cat("Loaded bleaching severity thresholds from previous analysis\n")
} else {
  cat("Warning: Severity thresholds not found. Using default values.\n")
  severity_thresholds <- data.frame(
    category = c("minimal", "moderate", "high", "severe"),
    upper_threshold = c(25, 50, 75, 100),
    description = c("Default quartiles", "Default quartiles", "Default quartiles", "Default quartiles")
  )
}

cat("Dataset dimensions:\n")
cat(sprintf("  Temperature data: %d rows × %d columns\n", nrow(temp_data), ncol(temp_data)))
cat(sprintf("  Date range: %d - %d\n", min(temp_data$year, na.rm = TRUE), max(temp_data$year, na.rm = TRUE)))
cat(sprintf("  Total unique sites: %d\n", n_distinct(temp_data$site)))

# Examine data structure and key variables
cat("\nKey variables in temperature data:\n")
cat(paste(names(temp_data), collapse = ", "), "\n")

# Check data completeness for 2023-2024 period
analysis_period_data <- temp_data %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(!is.na(dhw) & !is.na(weekly_max_temp))

cat(sprintf("\nTemperature records for 2023-2024: %d\n", nrow(analysis_period_data)))
cat(sprintf("Sites with 2023-2024 temperature data: %d\n", n_distinct(analysis_period_data$site)))

# ============================================================================
# DATA QUALITY ASSESSMENT FOR THERMAL DATA
# ============================================================================

cat("\nSTEP 2: Temperature data quality assessment\n")
cat("------------------------------------------\n")

# Check missing values and data gaps
missing_temp_analysis <- temp_data %>%
  filter(year %in% c(2023, 2024)) %>%
  summarise(
    total_records = n(),
    missing_dhw = sum(is.na(dhw)),
    missing_temp = sum(is.na(weekly_max_temp)),
    missing_bt = sum(is.na(BT)),
    zero_dhw_count = sum(dhw == 0, na.rm = TRUE),
    complete_records = sum(!is.na(dhw) & !is.na(weekly_max_temp))
  )

cat("Temperature data completeness for 2023-2024:\n")
print(missing_temp_analysis)

# Site-level data completeness
site_completeness <- analysis_period_data %>%
  group_by(site, year) %>%
  summarise(
    weeks_available = n(),
    weeks_with_dhw = sum(dhw > 0, na.rm = TRUE),
    temp_range = max(weekly_max_temp, na.rm = TRUE) - min(weekly_max_temp, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nSite-level data completeness summary:\n")
completeness_summary <- site_completeness %>%
  group_by(year) %>%
  summarise(
    sites = n(),
    mean_weeks = mean(weeks_available),
    min_weeks = min(weeks_available),
    max_weeks = max(weeks_available),
    mean_dhw_weeks = mean(weeks_with_dhw),
    .groups = "drop"
  )
print(completeness_summary)

# ============================================================================
# VISUALIZATION 1: Data Completeness Heatmap
# ============================================================================

# Create heatmap showing data availability by site and week
# Justification: Identifies temporal gaps in temperature monitoring that
# could affect DHW calculations and thermal stress assessment

data_completeness_matrix <- analysis_period_data %>%
  mutate(year_week = paste(year, sprintf("%02d", week), sep = "-W")) %>%
  group_by(site, year_week) %>%
  summarise(
    has_data = !is.na(first(weekly_max_temp)) & !is.na(first(dhw)),
    .groups = "drop"
  ) %>%
  arrange(site, year_week)

p1 <- ggplot(data_completeness_matrix, aes(x = year_week, y = reorder(site, has_data), fill = has_data)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "steelblue"),
                    labels = c("Missing", "Available"),
                    name = "Data Status") +
  labs(
    title = "Temperature Data Completeness Matrix: 2023-2024",
    subtitle = "Weekly temperature and DHW data availability by site",
    x = "Year-Week",
    y = "Monitoring Site",
    caption = "This heatmap reveals temporal gaps in temperature monitoring.\nComplete weekly coverage is essential for accurate DHW calculations\nand thermal stress assessment. Missing data (red) may underestimate\nthermal stress exposure at affected sites."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 8),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("02_plot_temperature_data_completeness.png", p1, width = 16, height = 10, dpi = 300)
cat("\nSaved: 02_plot_temperature_data_completeness.png\n")

# ============================================================================
# COMPREHENSIVE THERMAL METRICS CALCULATION
# ============================================================================

cat("\nSTEP 3: Calculating comprehensive thermal stress metrics\n")
cat("-------------------------------------------------------\n")

# Calculate detailed annual thermal metrics for each site-year
# Rationale: Multiple thermal stress indicators provide different perspectives
# on heat exposure - maximum intensity, cumulative stress, duration, and variability
annual_thermal_metrics <- analysis_period_data %>%
  group_by(site, year) %>%
  summarise(
    # Basic DHW metrics
    max_dhw = max(dhw, na.rm = TRUE),
    mean_dhw = mean(dhw, na.rm = TRUE),
    total_dhw_accumulation = sum(dhw, na.rm = TRUE),
    
    # Temperature extremes
    max_weekly_temp = max(weekly_max_temp, na.rm = TRUE),
    mean_weekly_temp = mean(weekly_max_temp, na.rm = TRUE),
    min_weekly_temp = min(weekly_max_temp, na.rm = TRUE),
    temp_range = max_weekly_temp - min_weekly_temp,
    
    # Temperature variability metrics
    temp_sd = sd(weekly_max_temp, na.rm = TRUE),
    temp_cv = temp_sd / mean_weekly_temp,
    
    # Thermal stress duration metrics
    weeks_above_29 = sum(weekly_max_temp > 29, na.rm = TRUE),
    weeks_above_30 = sum(weekly_max_temp > 30, na.rm = TRUE),
    weeks_above_31 = sum(weekly_max_temp > 31, na.rm = TRUE),
    weeks_with_dhw = sum(dhw > 0, na.rm = TRUE),
    weeks_dhw_above_4 = sum(dhw > 4, na.rm = TRUE),
    weeks_dhw_above_8 = sum(dhw > 8, na.rm = TRUE),
    
    # Data quality metrics
    total_weeks = n(),
    data_completeness = total_weeks / 52,  # Assuming 52 weeks per year
    
    .groups = "drop"
  ) %>%
  # Handle infinite values from division by zero
  mutate(
    temp_cv = ifelse(is.infinite(temp_cv) | is.nan(temp_cv), NA, temp_cv),
    max_dhw = ifelse(is.infinite(max_dhw), NA, max_dhw)
  )

cat(sprintf("Calculated thermal metrics for %d site-year combinations\n", nrow(annual_thermal_metrics)))

# ============================================================================
# DATA-DRIVEN DHW THRESHOLD CALCULATION
# ============================================================================

cat("\nSTEP 4: Establishing data-driven DHW thresholds using observed quartiles\n")
cat("------------------------------------------------------------------------\n")

# Calculate DHW quartiles from observed data
# Rationale: Literature-based thresholds (DHW 4 = bleaching, DHW 8 = mortality)
# may not reflect local conditions. Data-driven thresholds ensure classifications
# are appropriate for this specific dataset and region.

# Extract all non-zero DHW values for quartile calculation
all_dhw_values <- annual_thermal_metrics$max_dhw[!is.na(annual_thermal_metrics$max_dhw) & 
                                                   annual_thermal_metrics$max_dhw > 0]

dhw_quartiles <- quantile(all_dhw_values, probs = c(0, 0.25, 0.5, 0.75, 1.0))
cat("Maximum DHW quartiles (observed data):\n")
print(dhw_quartiles)

# Calculate temperature variability quartiles
temp_var_values <- annual_thermal_metrics$temp_sd[!is.na(annual_thermal_metrics$temp_sd)]
temp_var_quartiles <- quantile(temp_var_values, probs = c(0, 0.25, 0.5, 0.75, 1.0))
cat("\nTemperature variability (SD) quartiles:\n")
print(temp_var_quartiles)

# Calculate cumulative DHW quartiles
cumul_dhw_values <- annual_thermal_metrics$total_dhw_accumulation[!is.na(annual_thermal_metrics$total_dhw_accumulation) & 
                                                                    annual_thermal_metrics$total_dhw_accumulation > 0]
cumul_dhw_quartiles <- quantile(cumul_dhw_values, probs = c(0, 0.25, 0.5, 0.75, 1.0))
cat("\nCumulative DHW quartiles:\n")
print(cumul_dhw_quartiles)

# Define thermal stress categories based on observed quartiles
thermal_stress_thresholds <- list(
  dhw_low = dhw_quartiles[2],        # 0-25th percentile
  dhw_moderate = dhw_quartiles[3],   # 25th-50th percentile
  dhw_high = dhw_quartiles[4],       # 50th-75th percentile
  dhw_extreme = dhw_quartiles[5]     # 75th-100th percentile
)

cat("\nData-driven thermal stress categories (Max DHW):\n")
cat(sprintf("  Low Stress: 0 - %.1f DHW (0-25th percentile)\n", thermal_stress_thresholds$dhw_low))
cat(sprintf("  Moderate Stress: %.1f - %.1f DHW (25th-50th percentile)\n", 
            thermal_stress_thresholds$dhw_low, thermal_stress_thresholds$dhw_moderate))
cat(sprintf("  High Stress: %.1f - %.1f DHW (50th-75th percentile)\n", 
            thermal_stress_thresholds$dhw_moderate, thermal_stress_thresholds$dhw_high))
cat(sprintf("  Extreme Stress: %.1f - %.1f DHW (75th-100th percentile)\n", 
            thermal_stress_thresholds$dhw_high, thermal_stress_thresholds$dhw_extreme))

# Apply thermal stress categories
annual_thermal_metrics <- annual_thermal_metrics %>%
  mutate(
    dhw_stress_category = case_when(
      is.na(max_dhw) | max_dhw == 0 ~ "No Stress",
      max_dhw <= thermal_stress_thresholds$dhw_low ~ "Low Stress",
      max_dhw <= thermal_stress_thresholds$dhw_moderate ~ "Moderate Stress",
      max_dhw <= thermal_stress_thresholds$dhw_high ~ "High Stress",
      max_dhw <= thermal_stress_thresholds$dhw_extreme ~ "Extreme Stress",
      TRUE ~ "Extreme Stress"
    ),
    dhw_stress_category = factor(dhw_stress_category, 
                                 levels = c("No Stress", "Low Stress", "Moderate Stress", 
                                            "High Stress", "Extreme Stress")),
    
    # Temperature variability categories
    temp_var_category = case_when(
      is.na(temp_sd) ~ "Unknown",
      temp_sd <= temp_var_quartiles[2] ~ "Low Variability",
      temp_sd <= temp_var_quartiles[3] ~ "Moderate Variability",
      temp_sd <= temp_var_quartiles[4] ~ "High Variability",
      TRUE ~ "Extreme Variability"
    ),
    temp_var_category = factor(temp_var_category,
                               levels = c("Low Variability", "Moderate Variability",
                                          "High Variability", "Extreme Variability"))
  )

# ============================================================================
# VISUALIZATION 2: DHW Distribution Analysis
# ============================================================================

# Create comprehensive DHW distribution plots
# Justification: Understanding DHW distributions reveals the thermal stress
# landscape and validates our quartile-based classification system

dhw_2023 <- annual_thermal_metrics %>% filter(year == 2023)
dhw_2024 <- annual_thermal_metrics %>% filter(year == 2024)

p2 <- ggplot(annual_thermal_metrics, aes(x = factor(year), y = max_dhw, fill = factor(year))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  geom_hline(yintercept = thermal_stress_thresholds$dhw_low, linetype = "dashed", color = "green") +
  geom_hline(yintercept = thermal_stress_thresholds$dhw_moderate, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = thermal_stress_thresholds$dhw_high, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(name = "Year") +
  labs(
    title = "Distribution of Maximum DHW by Year (2023-2024)",
    subtitle = "Dashed lines show data-driven stress category thresholds",
    x = "Year",
    y = "Maximum DHW",
    caption = "This plot reveals the intensity of thermal stress in each year.\nData-driven thresholds (dashed lines) are based on observed quartiles\nrather than literature values, ensuring appropriate classification for\nthis specific dataset. Higher DHW values indicate more severe thermal\nstress that could drive bleaching responses."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("02_plot_dhw_distributions.png", p2, width = 10, height = 7, dpi = 300)
cat("Saved: 02_plot_dhw_distributions.png\n")

# ============================================================================
# VISUALIZATION 3: Thermal Stress Category Analysis
# ============================================================================

# Create stacked bar chart of thermal stress categories
# Justification: Shows the distribution of sites across stress categories
# and how this changes between years

stress_category_summary <- annual_thermal_metrics %>%
  group_by(year, dhw_stress_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  )

p3 <- ggplot(stress_category_summary, aes(x = factor(year), y = percentage, fill = dhw_stress_category)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = sprintf("n=%d", count)), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_viridis_d(name = "Thermal Stress\nCategory", option = "plasma") +
  labs(
    title = "Thermal Stress Category Distribution by Year",
    subtitle = "Categories based on observed DHW quartiles",
    x = "Year",
    y = "Percentage of Sites (%)",
    caption = "This stacked bar chart reveals the distribution of thermal stress\nlevels across sites in each year. Quartile-based categories ensure\nbalanced representation of stress levels. Shifts between years\nindicate changes in regional thermal conditions that could\ninfluence bleaching patterns and recovery dynamics."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))

ggsave("02_plot_stress_categories.png", p3, width = 10, height = 7, dpi = 300)
cat("Saved: 02_plot_stress_categories.png\n")

# ============================================================================
# TEMPERATURE VARIABILITY ANALYSIS
# ============================================================================

cat("\nSTEP 5: Analyzing temperature variability patterns\n")
cat("--------------------------------------------------\n")

# Calculate detailed temperature variability metrics
# Rationale: Temperature variability may be as important as absolute values
# for coral stress responses. Some corals may be more sensitive to fluctuations
# than to sustained high temperatures.

temp_variability_analysis <- analysis_period_data %>%
  group_by(site, year) %>%
  arrange(week) %>%
  mutate(
    # Week-to-week temperature changes
    temp_change = abs(weekly_max_temp - lag(weekly_max_temp)),
    temp_increasing = weekly_max_temp > lag(weekly_max_temp),
    
    # Rolling temperature metrics (4-week windows)
    temp_4wk_mean = zoo::rollmean(weekly_max_temp, k = 4, fill = NA, align = "right"),
    temp_4wk_max = zoo::rollmax(weekly_max_temp, k = 4, fill = NA, align = "right")
  ) %>%
  summarise(
    # Temperature change metrics
    mean_weekly_change = mean(temp_change, na.rm = TRUE),
    max_weekly_change = max(temp_change, na.rm = TRUE),
    weeks_increasing = sum(temp_increasing, na.rm = TRUE),
    weeks_decreasing = sum(!temp_increasing, na.rm = TRUE),
    
    # Temperature persistence metrics
    longest_hot_streak = max(rle(weekly_max_temp > 29)$lengths[rle(weekly_max_temp > 29)$values], default = 0),
    longest_extreme_streak = max(rle(weekly_max_temp > 30)$lengths[rle(weekly_max_temp > 30)$values], default = 0),
    
    # Coefficient of variation
    cv_temperature = sd(weekly_max_temp, na.rm = TRUE) / mean(weekly_max_temp, na.rm = TRUE),
    
    .groups = "drop"
  )

# Merge variability metrics with thermal metrics
comprehensive_thermal_data <- annual_thermal_metrics %>%
  left_join(temp_variability_analysis, by = c("site", "year"))

cat("Temperature variability statistics:\n")
variability_stats <- comprehensive_thermal_data %>%
  summarise(
    mean_weekly_change = mean(mean_weekly_change, na.rm = TRUE),
    mean_max_change = mean(max_weekly_change, na.rm = TRUE),
    mean_cv = mean(cv_temperature, na.rm = TRUE),
    mean_hot_streak = mean(longest_hot_streak, na.rm = TRUE),
    .groups = "drop"
  )
print(variability_stats)

# ============================================================================
# VISUALIZATION 4: Temperature Variability vs. Maximum DHW
# ============================================================================

# Create scatter plot showing relationship between variability and maximum stress
# Justification: Reveals whether sites with high temperature variability
# also experience high maximum DHW, or if these are independent stress factors

p4 <- ggplot(comprehensive_thermal_data, aes(x = temp_sd, y = max_dhw, color = factor(year))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  facet_wrap(~year, labeller = labeller(year = function(x) paste("Year", x))) +
  scale_color_viridis_d(name = "Year") +
  labs(
    title = "Temperature Variability vs. Maximum DHW",
    subtitle = "Relationship between thermal instability and peak stress",
    x = "Temperature Standard Deviation (°C)",
    y = "Maximum DHW",
    caption = "This plot examines whether temperature variability and maximum DHW\nare correlated or represent independent stress factors. High variability\nwith low maximum DHW might indicate frequent moderate fluctuations,\nwhile low variability with high DHW suggests sustained extreme heat.\nThe relationship could influence coral stress responses and recovery."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("02_plot_variability_vs_dhw.png", p4, width = 12, height = 8, dpi = 300)
cat("Saved: 02_plot_variability_vs_dhw.png\n")

# ============================================================================
# CUMULATIVE THERMAL STRESS ANALYSIS
# ============================================================================

cat("\nSTEP 6: Analyzing cumulative thermal stress patterns\n")
cat("----------------------------------------------------\n")

# Calculate cumulative stress metrics across both years
# Rationale: Two-year cumulative stress may better predict coral responses
# than single-year metrics, as thermal stress effects can be cumulative

cumulative_stress <- comprehensive_thermal_data %>%
  select(site, year, max_dhw, total_dhw_accumulation, weeks_with_dhw, temp_sd) %>%
  pivot_wider(names_from = year, 
              values_from = c(max_dhw, total_dhw_accumulation, weeks_with_dhw, temp_sd),
              names_sep = "_") %>%
  mutate(
    # Cumulative metrics across both years
    cumulative_max_dhw = pmax(max_dhw_2023, max_dhw_2024, na.rm = TRUE),
    total_stress_2years = total_dhw_accumulation_2023 + total_dhw_accumulation_2024,
    consecutive_stress_weeks = weeks_with_dhw_2023 + weeks_with_dhw_2024,
    mean_temp_variability = (temp_sd_2023 + temp_sd_2024) / 2,
    
    # Stress escalation/de-escalation patterns
    dhw_change_2023_to_2024 = max_dhw_2024 - max_dhw_2023,
    stress_trend = case_when(
      dhw_change_2023_to_2024 > 2 ~ "Escalating",
      dhw_change_2023_to_2024 < -2 ~ "De-escalating", 
      TRUE ~ "Stable"
    )
  )

# Calculate cumulative stress quartiles for classification
cumul_stress_quartiles <- quantile(cumulative_stress$total_stress_2years, 
                                   probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("Cumulative 2-year thermal stress quartiles:\n")
print(cumul_stress_quartiles)

# Identify sites with different thermal stress patterns
thermal_stress_patterns <- cumulative_stress %>%
  mutate(
    cumulative_stress_category = case_when(
      total_stress_2years <= cumul_stress_quartiles[2] ~ "Low Cumulative Stress",
      total_stress_2years <= cumul_stress_quartiles[3] ~ "Moderate Cumulative Stress",
      total_stress_2years <= cumul_stress_quartiles[4] ~ "High Cumulative Stress",
      TRUE ~ "Extreme Cumulative Stress"
    ),
    
    stress_pattern = case_when(
      max_dhw_2023 > dhw_quartiles[4] & max_dhw_2024 > dhw_quartiles[4] ~ "High_Both_Years",
      max_dhw_2023 > dhw_quartiles[4] & max_dhw_2024 <= dhw_quartiles[3] ~ "High_2023_Moderate_2024",
      max_dhw_2023 <= dhw_quartiles[3] & max_dhw_2024 > dhw_quartiles[4] ~ "Moderate_2023_High_2024",
      max_dhw_2023 > dhw_quartiles[3] & max_dhw_2024 > dhw_quartiles[3] ~ "Moderate_Both_Years",
      TRUE ~ "Low_Stress_Both_Years"
    )
  )

cat("\nThermal stress pattern distribution:\n")
pattern_counts <- thermal_stress_patterns %>%
  count(stress_pattern) %>%
  arrange(desc(n))
print(pattern_counts)

# ============================================================================
# VISUALIZATION 5: Cumulative Stress Analysis
# ============================================================================

# Create comprehensive cumulative stress visualization
# Justification: Cumulative stress over multiple years may be more predictive
# of coral responses than single-year metrics

p5 <- ggplot(thermal_stress_patterns, aes(x = max_dhw_2023, y = max_dhw_2024, 
                                          color = cumulative_stress_category, 
                                          size = total_stress_2years)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = dhw_quartiles[4], linetype = "dotted", color = "red", alpha = 0.5) +
  geom_vline(xintercept = dhw_quartiles[4], linetype = "dotted", color = "red", alpha = 0.5) +
  scale_color_viridis_d(name = "Cumulative\nStress Category") +
  scale_size_continuous(name = "Total DHW\n(2 years)", range = c(2, 8)) +
  labs(
    title = "Two-Year Thermal Stress Comparison: 2023 vs 2024",
    subtitle = "Point size represents cumulative DHW; lines show quartile thresholds",
    x = "2023 Maximum DHW",
    y = "2024 Maximum DHW",
    caption = "This plot reveals thermal stress patterns across consecutive years.\nPoints above the diagonal line experienced higher stress in 2024,\nwhile points below had higher 2023 stress. The dotted lines mark\nthe 75th percentile threshold for high stress. Point size indicates\ncumulative stress load, which may better predict coral responses\nthan single-year metrics alone."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))

ggsave("02_plot_cumulative_stress.png", p5, width = 12, height = 8, dpi = 300)
cat("Saved: 02_plot_cumulative_stress.png\n")

# ============================================================================
# SITE-SPECIFIC THERMAL PROFILES
# ============================================================================

cat("\nSTEP 7: Creating site-specific thermal stress profiles\n")
cat("------------------------------------------------------\n")

# Calculate comprehensive thermal profiles for each site
# Rationale: Sites may have distinctive thermal signatures that influence
# their bleaching susceptibility and recovery capacity

site_thermal_profiles <- comprehensive_thermal_data %>%
  group_by(site) %>%
  summarise(
    # Multi-year thermal characteristics
    years_data = n(),
    mean_max_dhw = mean(max_dhw, na.rm = TRUE),
    max_max_dhw = max(max_dhw, na.rm = TRUE),
    mean_temp_variability = mean(temp_sd, na.rm = TRUE),
    mean_cumul_dhw = mean(total_dhw_accumulation, na.rm = TRUE),
    
    # Thermal stress frequency
    years_high_stress = sum(max_dhw > dhw_quartiles[4], na.rm = TRUE),
    years_extreme_stress = sum(max_dhw > dhw_quartiles[5], na.rm = TRUE),
    
    # Temperature characteristics
    mean_annual_max_temp = mean(max_weekly_temp, na.rm = TRUE),
    mean_annual_range = mean(temp_range, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Only include sites with data from both years
  filter(years_data >= 2) %>%
  mutate(
    # Thermal stress frequency classification
    stress_frequency = case_when(
      years_extreme_stress >= 2 ~ "Consistently Extreme",
      years_high_stress >= 2 ~ "Consistently High",
      years_high_stress == 1 ~ "Intermittent High",
      TRUE ~ "Generally Low"
    ),
    
    # Overall thermal profile
    thermal_profile = case_when(
      mean_max_dhw > dhw_quartiles[4] & mean_temp_variability > temp_var_quartiles[4] ~ "High Stress + High Variability",
      mean_max_dhw > dhw_quartiles[4] & mean_temp_variability <= temp_var_quartiles[2] ~ "High Stress + Low Variability",
      mean_max_dhw <= dhw_quartiles[2] & mean_temp_variability > temp_var_quartiles[4] ~ "Low Stress + High Variability",
      mean_max_dhw <= dhw_quartiles[2] & mean_temp_variability <= temp_var_quartiles[2] ~ "Low Stress + Low Variability",
      TRUE ~ "Moderate Mixed Profile"
    )
  ) %>%
  arrange(desc(mean_max_dhw))

cat("Site thermal profile summary:\n")
profile_summary <- site_thermal_profiles %>%
  count(thermal_profile) %>%
  arrange(desc(n))
print(profile_summary)

cat("\nTop 10 highest thermal stress sites:\n")
print(head(site_thermal_profiles %>% select(site, mean_max_dhw, thermal_profile), 10))

# ============================================================================
# SAVE COMPREHENSIVE THERMAL ANALYSIS RESULTS
# ============================================================================

cat("\nSTEP 8: Saving comprehensive thermal analysis results\n")
cat("-----------------------------------------------------\n")

# Save all thermal analysis datasets
write_csv(annual_thermal_metrics, "02_annual_thermal_metrics.csv")
write_csv(comprehensive_thermal_data, "02_comprehensive_thermal_data.csv")
write_csv(cumulative_stress, "02_cumulative_thermal_stress.csv")
write_csv(thermal_stress_patterns, "02_thermal_stress_patterns.csv")
write_csv(site_thermal_profiles, "02_site_thermal_profiles.csv")

# Save thermal stress thresholds for use in other scripts
thermal_thresholds_data <- data.frame(
  metric = c("max_dhw_low", "max_dhw_moderate", "max_dhw_high", "max_dhw_extreme",
             "temp_var_low", "temp_var_moderate", "temp_var_high", "temp_var_extreme"),
  threshold = c(thermal_stress_thresholds$dhw_low, thermal_stress_thresholds$dhw_moderate,
                thermal_stress_thresholds$dhw_high, thermal_stress_thresholds$dhw_extreme,
                temp_var_quartiles[2], temp_var_quartiles[3], 
                temp_var_quartiles[4], temp_var_quartiles[5]),
  percentile = rep(c("25th", "50th", "75th", "100th"), 2),
  category = rep(c("DHW", "Temperature_Variability"), each = 4)
)
write_csv(thermal_thresholds_data, "02_thermal_stress_thresholds.csv")

# ============================================================================
# FINAL SUMMARY STATISTICS
# ============================================================================

cat("\nFINAL THERMAL ANALYSIS SUMMARY\n")
cat("===============================\n")

cat(sprintf("Sites analyzed: %d\n", n_distinct(comprehensive_thermal_data$site)))
cat(sprintf("Site-year combinations: %d\n", nrow(comprehensive_thermal_data)))
cat(sprintf("Total temperature records processed: %d\n", nrow(analysis_period_data)))

cat("\nThermal stress thresholds (data-driven quartiles):\n")
for(i in 1:4) {
  cat(sprintf("  %s: ≤%.1f DHW (%s percentile)\n", 
              c("Low", "Moderate", "High", "Extreme")[i],
              c(thermal_stress_thresholds$dhw_low, thermal_stress_thresholds$dhw_moderate,
                thermal_stress_thresholds$dhw_high, thermal_stress_thresholds$dhw_extreme)[i],
              c("25th", "50th", "75th", "100th")[i]))
}

cat(sprintf("\n2023 thermal stress summary:\n"))
stress_2023 <- comprehensive_thermal_data %>% filter(year == 2023)
cat(sprintf("  Mean max DHW: %.1f\n", mean(stress_2023$max_dhw, na.rm = TRUE)))
cat(sprintf("  Range: %.1f - %.1f\n", min(stress_2023$max_dhw, na.rm = TRUE), max(stress_2023$max_dhw, na.rm = TRUE)))
cat(sprintf("  Sites with extreme stress (>%.1f DHW): %d\n", thermal_stress_thresholds$dhw_high, 
            sum(stress_2023$max_dhw > thermal_stress_thresholds$dhw_high, na.rm = TRUE)))

cat(sprintf("\n2024 thermal stress summary:\n"))
stress_2024 <- comprehensive_thermal_data %>% filter(year == 2024)
cat(sprintf("  Mean max DHW: %.1f\n", mean(stress_2024$max_dhw, na.rm = TRUE)))
cat(sprintf("  Range: %.1f - %.1f\n", min(stress_2024$max_dhw, na.rm = TRUE), max(stress_2024$max_dhw, na.rm = TRUE)))
cat(sprintf("  Sites with extreme stress (>%.1f DHW): %d\n", thermal_stress_thresholds$dhw_high,
            sum(stress_2024$max_dhw > thermal_stress_thresholds$dhw_high, na.rm = TRUE)))

cat("\nFiles saved:\n")
cat("  - 02_annual_thermal_metrics.csv\n")
cat("  - 02_comprehensive_thermal_data.csv\n")
cat("  - 02_cumulative_thermal_stress.csv\n")
cat("  - 02_thermal_stress_patterns.csv\n")
cat("  - 02_site_thermal_profiles.csv\n")
cat("  - 02_thermal_stress_thresholds.csv\n")

cat("\nVisualizations saved:\n")
cat("  - 02_plot_temperature_data_completeness.png\n")
cat("  - 02_plot_dhw_distributions.png\n")
cat("  - 02_plot_stress_categories.png\n")
cat("  - 02_plot_variability_vs_dhw.png\n")
cat("  - 02_plot_cumulative_stress.png\n")

cat("\n============================================================================\n")
cat("STEP 2 COMPLETE: Comprehensive thermal stress analysis using data-driven thresholds\n")
cat("Next: Bleaching response and recovery pattern analysis\n")
cat("============================================================================\n")