# ===============================================================
# Coral Bleaching and Degree Heating Weeks Analysis
# TCRMP Data Analysis Script
# ===============================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(viridis)
library(zoo)  # For rolling window calculations

# Set working directory to the project root (adjust as needed)
setwd(".")

# ===============================================================
# 1. DATA IMPORT
# ===============================================================

# Import bleaching extent data
bleaching_extent <- read.csv("data/s3pt1_coralcondition_bleachingextent_34sites_2022_2025.csv", 
                            stringsAsFactors = FALSE)

# Import bleaching prevalence data  
bleaching_prevalence <- read.csv("data/s3pt1_coralcondition_bleachingprevalence_34sites_2022_2025.csv", 
                                stringsAsFactors = FALSE)

# Import DHW data
dhw_data <- read.csv("data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv", 
                     stringsAsFactors = FALSE)

# ===============================================================
# 2. DATA PREPROCESSING
# ===============================================================

# Convert date columns to proper date format
bleaching_extent$date <- as.Date(bleaching_extent$date)
bleaching_prevalence$date <- as.Date(bleaching_prevalence$date)

# Create week numbers for bleaching data to match with DHW data
bleaching_extent$week <- week(bleaching_extent$date)
bleaching_prevalence$week <- week(bleaching_prevalence$date)

# Check site overlap between datasets
common_sites <- intersect(unique(bleaching_extent$site), unique(dhw_data$site))
cat("Common sites between bleaching and DHW data:", length(common_sites), "\n")
cat("Sites in common:\n")
print(sort(common_sites))

# Prepare DHW data for weekly matching (DHW is already 12-week rolling average)
dhw_weekly <- dhw_data %>%
  select(year, week, site, dhw, weekly_max_temp, BT, depth, program) %>%
  rename(pre_survey_dhw = dhw,
         pre_survey_temp = weekly_max_temp,
         bleaching_threshold = BT)

# ===============================================================
# 3. CREATE ANALYSIS DATASETS
# ===============================================================

# Function to summarize bleaching data by site-year-period
calculate_bleaching_metrics_by_site_year_period <- function(data, metric_prefix) {
  data %>%
    group_by(site, year, period) %>%
    summarise(
      n_replicates = n(),
      survey_date = first(date),
      survey_week = first(week),
      pre_survey_week = pmax(1, first(week) - 1),  # Week before survey, minimum week 1
      mean_severely_bleached = mean(get(paste0(metric_prefix, "_bleached")), na.rm = TRUE),
      mean_pale_bleached_bleached = mean(get(paste0(metric_prefix, "_pale")), na.rm = TRUE),
      # mean_slightly_pale = mean(get(paste0(metric_prefix, "_slightlypale")), na.rm = TRUE),
      mean_very_pale = mean(get(paste0(metric_prefix, "_verypale")), na.rm = TRUE),
      # Calculate total bleaching by summing all bleaching categories
      mean_combined_bleaching = mean_severely_bleached + mean_pale_bleached_bleached + mean_very_pale,
      .groups = 'drop'
    )
}

# Calculate bleaching metrics by grouping
bleaching_extent_by_site_year <- calculate_bleaching_metrics_by_site_year_period(bleaching_extent, "ext")
bleaching_prevalence_by_site_year <- calculate_bleaching_metrics_by_site_year_period(bleaching_prevalence, "prev")

# Add number of colonies for prevalence data
bleaching_prevalence_by_site_year <- bleaching_prevalence_by_site_year %>%
  left_join(
    bleaching_prevalence %>%
      group_by(site, year, period) %>%
      summarise(mean_ncolonies = mean(ncolonies, na.rm = TRUE), .groups = 'drop'),
    by = c("site", "year", "period")
  )

# ===============================================================
# 4. MERGE BLEACHING AND DHW DATA
# ===============================================================

# Merge extent data with DHW for the week before survey
bleaching_extent_with_dhw <- bleaching_extent_by_site_year %>%
  filter(site %in% common_sites) %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data for that week

# Merge prevalence data with DHW for the week before survey
bleaching_prevalence_with_dhw <- bleaching_prevalence_by_site_year %>%
  filter(site %in% common_sites) %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data for that week

# ===============================================================
# 5. EXPLORATORY DATA ANALYSIS
# ===============================================================

# Basic summary statistics
cat("\n=== DATASET SUMMARY ===\n")
cat("Bleaching extent records with DHW:", nrow(bleaching_extent_with_dhw), "\n")
cat("Bleaching prevalence records with DHW:", nrow(bleaching_prevalence_with_dhw), "\n")
cat("Years covered:", range(bleaching_extent_with_dhw$year), "\n")
cat("Sites with both datasets:", length(common_sites), "\n")

# Debug: Check what data we have by year and period
cat("\n=== DATA AVAILABILITY CHECK ===\n")
cat("Extent data by year and period:\n")
print(bleaching_extent_with_dhw %>% 
      group_by(year, period) %>% 
      summarise(n_records = n(), n_sites = length(unique(site)), .groups = 'drop') %>%
      arrange(year, period))

cat("\nPrevalence data by year and period:\n") 
print(bleaching_prevalence_with_dhw %>% 
      group_by(year, period) %>% 
      summarise(n_records = n(), n_sites = length(unique(site)), .groups = 'drop') %>%
      arrange(year, period))

# DHW distribution
cat("\nDHW Summary Statistics (Week Before Survey):\n")
print(summary(bleaching_extent_with_dhw$pre_survey_dhw))

# Bleaching distribution by period
cat("\nBleaching extent by survey period:\n")
print(bleaching_extent_with_dhw %>% group_by(period) %>% summarise(
  n = n(),
  mean_severely_bleached = round(mean(mean_severely_bleached, na.rm = TRUE), 2),
  mean_pale_bleached = round(mean(mean_pale_bleached, na.rm = TRUE), 2),
  mean_slightly_pale = round(mean(mean_slightly_pale, na.rm = TRUE), 2),
  mean_very_pale = round(mean(mean_very_pale, na.rm = TRUE), 2),
  mean_combined_bleaching = round(mean(mean_combined_bleaching, na.rm = TRUE), 2),
  mean_pre_survey_dhw = round(mean(pre_survey_dhw, na.rm = TRUE), 2),
  .groups = 'drop'
))

# Site analysis - check for site resistance patterns
cat("\nSite resistance analysis (sites with >3 observations):\n")
site_bleaching_resistance <- bleaching_extent_with_dhw %>%
  group_by(site) %>%
  filter(n() > 3) %>%  # Only sites with multiple observations
  summarise(
    n_observations = n(),
    mean_pre_survey_dhw = round(mean(pre_survey_dhw, na.rm = TRUE), 2),
    mean_combined_bleaching = round(mean(mean_combined_bleaching, na.rm = TRUE), 2),
    bleaching_susceptibility_ratio = ifelse(mean_pre_survey_dhw > 0, mean_combined_bleaching / mean_pre_survey_dhw, NA),
    .groups = 'drop'
  ) %>%
  arrange(bleaching_susceptibility_ratio)

print(site_bleaching_resistance)

# ===============================================================
# 6. CORRELATION ANALYSIS
# ===============================================================

# Function to perform correlation analysis
analyze_correlations <- function(data, title_prefix) {
  cat(paste("\n=== CORRELATION ANALYSIS:", title_prefix, "===\n"))
  
  # Select numeric variables for correlation
  cor_vars <- data %>%
    select(pre_survey_dhw, pre_survey_temp, bleaching_threshold, depth,
           mean_severely_bleached, mean_pale_bleached, mean_slightly_pale, mean_very_pale, mean_combined_bleaching) %>%
    na.omit()
  
  # Calculate correlations
  cor_matrix <- cor(cor_vars, use = "complete.obs")
  
  # Print correlations with DHW from week before survey
  cat("Correlations with Pre-Survey DHW (12-week rolling):\n")
  dhw_cors <- cor_matrix[, "pre_survey_dhw"]
  print(round(dhw_cors[dhw_cors != 1], 3))
  
  # Print correlations between bleaching categories
  cat("\nCorrelations between bleaching categories:\n")
  bleaching_cors <- cor_matrix[c("mean_severely_bleached", "mean_pale_bleached", "mean_slightly_pale", "mean_very_pale", "mean_combined_bleaching"),
                              c("mean_severely_bleached", "mean_pale_bleached", "mean_slightly_pale", "mean_very_pale", "mean_combined_bleaching")]
  print(round(bleaching_cors, 3))
  
  return(cor_matrix)
}

# Analyze correlations for extent data
extent_cor <- analyze_correlations(bleaching_extent_with_dhw, "EXTENT")

# Analyze correlations for prevalence data (Annual period only)
prevalence_cor <- analyze_correlations(bleaching_prevalence_with_dhw, "PREVALENCE")

# ===============================================================
# 7. VISUALIZATION FUNCTIONS
# ===============================================================

# Function to create scatter plots
create_scatter_plot <- function(data, x_var, y_var, x_label, y_label, title_suffix = "") {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = factor(year), shape = period), alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    labs(
      x = x_label,
      y = y_label,
      title = paste("Coral Bleaching vs DHW", title_suffix),
      color = "Year",
      shape = "Survey Period"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_viridis_d()
}

# Function to create boxplots by DHW categories
create_dhw_boxplot <- function(data, x_var, y_var, y_label, dhw_type = "") {
  # Create DHW categories
  data_cat <- data %>%
    mutate(dhw_category = case_when(
      .data[[x_var]] < 4 ~ "Low (<4)",
      .data[[x_var]] < 8 ~ "Moderate (4-8)",
      .data[[x_var]] >= 8 ~ "High (≥8)"
    )) %>%
    filter(!is.na(dhw_category))
  
  ggplot(data_cat, aes(x = dhw_category, y = .data[[y_var]])) +
    geom_boxplot(aes(fill = dhw_category), alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
      x = paste("DHW Category", dhw_type),
      y = y_label,
      title = paste("Coral Bleaching by DHW Category", dhw_type),
      fill = "DHW Category"
    ) +
    theme_bw() +
    scale_fill_viridis_d()
}

# ===============================================================
# 8. CREATE VISUALIZATIONS
# ===============================================================

# Multiple bleaching category plots for extent
plot_extent_combined_bleaching <- create_scatter_plot(bleaching_extent_with_dhw, "pre_survey_dhw", "mean_combined_bleaching", 
                         "Pre-Survey DHW (°C-weeks)", "Combined Bleaching Extent (%)", "(Extent)")

plot_extent_severe_bleaching <- create_scatter_plot(bleaching_extent_with_dhw, "pre_survey_dhw", "mean_severely_bleached", 
                         "Pre-Survey DHW (°C-weeks)", "Severely Bleached Extent (%)", "(Extent)")

plot_extent_pale_bleaching <- create_scatter_plot(bleaching_extent_with_dhw, "pre_survey_dhw", "mean_pale_bleached", 
                         "Pre-Survey DHW (°C-weeks)", "Pale Bleached Extent (%)", "(Extent)")

# Prevalence plots 
plot_prevalence_combined_bleaching <- create_scatter_plot(bleaching_prevalence_with_dhw, "pre_survey_dhw", "mean_combined_bleaching", 
                         "Pre-Survey DHW (°C-weeks)", "Combined Bleaching Prevalence (%)", "(Prevalence)")

plot_prevalence_severe_bleaching <- create_scatter_plot(bleaching_prevalence_with_dhw, "pre_survey_dhw", "mean_severely_bleached", 
                         "Pre-Survey DHW (°C-weeks)", "Severely Bleached Prevalence (%)", "(Prevalence)")

# Site-specific analysis plot - check if resistant sites stand out
plot_site_specific_responses <- ggplot(bleaching_extent_with_dhw, aes(x = pre_survey_dhw, y = mean_combined_bleaching)) +
  geom_point(aes(color = factor(year), shape = period), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  facet_wrap(~site, scales = "free", ncol = 4) +
  labs(
    x = "Pre-Survey DHW (°C-weeks)",
    y = "Total Bleaching Extent (%)",
    title = "Site-Specific Bleaching Responses to DHW",
    color = "Year",
    shape = "Period"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8)) +
  scale_color_viridis_d()

# Create a plot specifically highlighting 2023 vs 2024 Annual surveys
annual_surveys_2023_2024 <- bleaching_extent_with_dhw %>% filter(period == "Annual", year %in% c(2023, 2024))
plot_annual_comparison_2023_2024 <- ggplot(annual_surveys_2023_2024, aes(x = pre_survey_dhw, y = mean_combined_bleaching)) +
  geom_point(aes(color = factor(year), size = 3), alpha = 0.8) +
  geom_text(aes(label = site), vjust = -0.5, hjust = 0.5, size = 3) +
  geom_smooth(method = "lm", se = TRUE, aes(color = factor(year))) +
  labs(
    x = "Pre-Survey DHW (°C-weeks)",
    y = "Total Bleaching Extent (%)",
    title = "2023 vs 2024 Annual Surveys - Peak Bleaching Years",
    color = "Year",
    size = ""
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  guides(size = "none")

cat("\n=== ANNUAL SURVEY COMPARISON ===\n")
cat("2023 vs 2024 Annual survey data:\n")
if(nrow(annual_surveys_2023_2024) > 0) {
  print(annual_surveys_2023_2024 %>% 
        select(site, year, period, pre_survey_dhw, mean_combined_bleaching, mean_severely_bleached) %>%
        arrange(year, site))
} else {
  cat("No Annual data found for 2023-2024!\n")
}

# Display plots
print(plot_extent_combined_bleaching)
print(plot_extent_severe_bleaching)
print(plot_extent_pale_bleaching)
print(plot_prevalence_combined_bleaching) 
print(plot_prevalence_severe_bleaching)
print(plot_site_specific_responses)
print(plot_annual_comparison_2023_2024)

# ===============================================================
# 9. STATISTICAL MODELS
# ===============================================================

# Function to fit and summarize linear models
fit_bleaching_model <- function(data, response_var, title, include_site = TRUE) {
  cat(paste("\n=== LINEAR MODEL:", title, "===\n"))
  
  # Fit model with and without site effects
  if(include_site) {
    formula_str <- paste(response_var, "~ pre_survey_dhw + factor(year) + period + site")
    model_with_site <- lm(as.formula(formula_str), data = data)
    
    cat("Model WITH site effects:\n")
    print(summary(model_with_site))
    cat("R-squared:", round(summary(model_with_site)$r.squared, 3), "\n")
    cat("Adjusted R-squared:", round(summary(model_with_site)$adj.r.squared, 3), "\n")
    
    # Simpler model without site
    formula_simple <- paste(response_var, "~ pre_survey_dhw + factor(year) + period")
    model_simple <- lm(as.formula(formula_simple), data = data)
    
    cat("\nModel WITHOUT site effects:\n")
    print(summary(model_simple))
    cat("R-squared:", round(summary(model_simple)$r.squared, 3), "\n")
    cat("Adjusted R-squared:", round(summary(model_simple)$adj.r.squared, 3), "\n")
    
    # Compare models
    anova_result <- anova(model_simple, model_with_site)
    cat("\nANOVA comparing models (site importance):\n")
    print(anova_result)
    
    return(list(with_site = model_with_site, without_site = model_simple, anova = anova_result))
  } else {
    formula_str <- paste(response_var, "~ pre_survey_dhw + factor(year) + period")
    model <- lm(as.formula(formula_str), data = data)
    print(summary(model))
    return(model)
  }
}

# Fit models for different bleaching categories
cat("=== EXTENT MODELS ===\n")
model_extent_total <- fit_bleaching_model(bleaching_extent_with_dhw, "mean_combined_bleaching", 
                                         "EXTENT - Total Bleaching", include_site = TRUE)

model_extent_bleached <- fit_bleaching_model(bleaching_extent_with_dhw, "mean_severely_bleached", 
                                            "EXTENT - Severely Bleached", include_site = TRUE)

model_extent_pale <- fit_bleaching_model(bleaching_extent_with_dhw, "mean_pale_bleached", 
                                        "EXTENT - Pale", include_site = TRUE)

cat("\n=== PREVALENCE MODELS ===\n")
model_prev_total <- fit_bleaching_model(bleaching_prevalence_with_dhw, "mean_combined_bleaching", 
                                       "PREVALENCE - Total Bleaching", include_site = TRUE)

model_prev_bleached <- fit_bleaching_model(bleaching_prevalence_with_dhw, "mean_severely_bleached", 
                                          "PREVALENCE - Severely Bleached", include_site = TRUE)

# ===============================================================
# 10. SUMMARY TABLES
# ===============================================================

# Enhanced site-level summary
site_summary <- bleaching_extent_with_dhw %>%
  group_by(site) %>%
  summarise(
    n_surveys = n(),
    years_surveyed = paste(sort(unique(year)), collapse = ", "),
    periods_surveyed = paste(sort(unique(period)), collapse = ", "),
    mean_pre_survey_dhw = round(mean(pre_survey_dhw, na.rm = TRUE), 2),
    max_pre_survey_dhw = round(max(pre_survey_dhw, na.rm = TRUE), 2),
    mean_combined_bleaching = round(mean(mean_combined_bleaching, na.rm = TRUE), 1),
    mean_severe_bleaching = round(mean(mean_severely_bleached, na.rm = TRUE), 1),
    mean_pale_bleached = round(mean(mean_pale_bleached, na.rm = TRUE), 1),
    max_total_bleaching = round(max(mean_combined_bleaching, na.rm = TRUE), 1),
    # Resistance metric: bleaching per unit DHW
    resistance_score = ifelse(mean_pre_survey_dhw > 0, 
                             round(mean_combined_bleaching / mean_pre_survey_dhw, 3), NA),
    depth = first(depth),
    .groups = 'drop'
  ) %>%
  arrange(resistance_score)

cat("\n=== SITE SUMMARY TABLE ===\n")
print(site_summary)

# Year-level summary
year_summary <- bleaching_extent_with_dhw %>%
  group_by(year, period) %>%
  summarise(
    n_sites = length(unique(site)),
    mean_pre_survey_dhw = round(mean(pre_survey_dhw, na.rm = TRUE), 2),
    mean_combined_bleaching = round(mean(mean_combined_bleaching, na.rm = TRUE), 1),
    mean_severe_bleaching = round(mean(mean_severely_bleached, na.rm = TRUE), 1),
    sites_high_dhw = sum(pre_survey_dhw >= 8, na.rm = TRUE),
    sites_high_bleaching = sum(mean_combined_bleaching >= 25, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year, period)

cat("\n=== YEAR-PERIOD SUMMARY TABLE ===\n")
print(year_summary)

# Specific 2023/2024 Annual comparison
cat("\n=== 2023 vs 2024 ANNUAL COMPARISON ===\n")
annual_comparison <- bleaching_extent_with_dhw %>%
  filter(period == "Annual", year %in% c(2023, 2024)) %>%
  group_by(year) %>%
  summarise(
    n_sites = length(unique(site)),
    sites_list = paste(sort(unique(site)), collapse = ", "),
    mean_dhw = round(mean(pre_survey_dhw, na.rm = TRUE), 2),
    sd_dhw = round(sd(pre_survey_dhw, na.rm = TRUE), 2),
    mean_combined_bleaching = round(mean(mean_combined_bleaching, na.rm = TRUE), 1),
    sd_total_bleaching = round(sd(mean_combined_bleaching, na.rm = TRUE), 1),
    mean_severe_bleaching = round(mean(mean_severely_bleached, na.rm = TRUE), 1),
    .groups = 'drop'
  )

if(nrow(annual_comparison) > 0) {
  print(annual_comparison)
  
  # Statistical test
  data_2023 <- bleaching_extent_with_dhw %>% filter(period == "Annual", year == 2023)
  data_2024 <- bleaching_extent_with_dhw %>% filter(period == "Annual", year == 2024)
  
  if(nrow(data_2023) > 0 && nrow(data_2024) > 0) {
    cat("\nT-test comparing 2023 vs 2024 Annual total bleaching:\n")
    t_test_result <- t.test(data_2023$mean_combined_bleaching, data_2024$mean_combined_bleaching)
    print(t_test_result)
  } else {
    cat("Insufficient data for statistical comparison\n")
    cat("2023 Annual records:", nrow(data_2023), "\n")
    cat("2024 Annual records:", nrow(data_2024), "\n")
  }
} else {
  cat("No Annual data found for 2023-2024 comparison!\n")
}

# ===============================================================
# 11. EXPORT RESULTS
# ===============================================================

# Save processed datasets
write.csv(bleaching_extent_with_dhw, "extent_presurvey_dhw_analysis.csv", row.names = FALSE)
write.csv(bleaching_prevalence_with_dhw, "prevalence_presurvey_dhw_analysis.csv", row.names = FALSE)
write.csv(site_summary, "site_bleaching_resistance_summary.csv", row.names = FALSE)
write.csv(year_summary, "year_period_summary.csv", row.names = FALSE)

# Save diagnostic datasets
if(nrow(annual_comparison) > 0) {
  write.csv(annual_comparison, "annual_2023_2024_comparison.csv", row.names = FALSE)
}

# Save site resistance analysis
write.csv(site_bleaching_resistance, "site_bleaching_resistance_analysis.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results exported to CSV files:\n")
cat("- extent_presurvey_dhw_analysis.csv\n")
cat("- prevalence_presurvey_dhw_analysis.csv\n") 
cat("- site_bleaching_resistance_summary.csv\n")
cat("- year_period_summary.csv\n")
cat("- site_bleaching_resistance_analysis.csv\n")
if(nrow(annual_comparison) > 0) {
  cat("- annual_2023_2024_comparison.csv\n")
}

# Summary of key findings
cat("\n=== KEY FINDINGS SUMMARY ===\n")
cat("1. Total records analyzed:", nrow(bleaching_extent_with_dhw), "\n")
cat("2. Years with data:", paste(sort(unique(bleaching_extent_with_dhw$year)), collapse = ", "), "\n")
cat("3. Periods surveyed:", paste(sort(unique(bleaching_extent_with_dhw$period)), collapse = ", "), "\n")
cat("4. Sites with resistance scores available:", sum(!is.na(site_summary$resistance_score)), "\n")
cat("5. Most resistant sites (lowest bleaching per DHW):\n")
print(head(site_summary %>% filter(!is.na(resistance_score)) %>% 
           select(site, resistance_score, mean_combined_bleaching, mean_pre_survey_dhw), 5))
cat("6. Least resistant sites (highest bleaching per DHW):\n")
print(tail(site_summary %>% filter(!is.na(resistance_score)) %>% 
           select(site, resistance_score, mean_combined_bleaching, mean_pre_survey_dhw), 5))