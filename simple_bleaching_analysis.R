# ===============================================================
# SIMPLIFIED Coral Bleaching vs DHW Analysis
# Quick Start Version
# ===============================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# ===============================================================
# 1. IMPORT DATA
# ===============================================================

# Import the three main datasets
bleaching_extent <- read.csv("data/s3pt1_coralcondition_bleachingextent_34sites_2022_2025.csv")
bleaching_prevalence <- read.csv("data/s3pt1_coralcondition_bleachingprevalence_34sites_2022_2025.csv")
dhw_data <- read.csv("data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

# ===============================================================
# 2. PREPARE DATA FOR ANALYSIS
# ===============================================================

# Prepare DHW data for weekly matching (DHW is already 12-week rolling average)
dhw_weekly <- dhw_data %>%
  select(year, week, site, dhw, weekly_max_temp) %>%
  rename(pre_survey_dhw = dhw,
         pre_survey_temp = weekly_max_temp)

# Convert dates and calculate survey weeks
bleaching_extent$date <- as.Date(bleaching_extent$date)
bleaching_prevalence$date <- as.Date(bleaching_prevalence$date)
bleaching_extent$survey_week <- week(bleaching_extent$date)
bleaching_prevalence$survey_week <- week(bleaching_prevalence$date)

# Summarize bleaching extent by site-year-period (average across replicates)
extent_summary <- bleaching_extent %>%
  group_by(site, year, period) %>%
  summarise(
    survey_week = first(survey_week),
    pre_survey_week = pmax(1, first(survey_week) - 1),  # Week before survey, minimum week 1
    mean_any_bleaching = mean(ext_anybleaching, na.rm = TRUE),
    mean_bleached = mean(ext_bleached, na.rm = TRUE),
    mean_pale = mean(ext_pale, na.rm = TRUE),
    n_replicates = n(),
    .groups = 'drop'
  )

# Summarize bleaching prevalence by site-year-period  
prevalence_summary <- bleaching_prevalence %>%
  group_by(site, year, period) %>%
  summarise(
    survey_week = first(survey_week),
    pre_survey_week = pmax(1, first(survey_week) - 1),  # Week before survey, minimum week 1
    mean_any_bleaching = mean(prev_anybleaching, na.rm = TRUE),
    mean_bleached = mean(prev_bleached, na.rm = TRUE),
    mean_pale = mean(prev_pale, na.rm = TRUE),
    n_replicates = n(),
    .groups = 'drop'
  )

# ===============================================================
# 3. MERGE BLEACHING AND DHW DATA
# ===============================================================

# Combine extent data with DHW from week before survey
extent_dhw <- extent_summary %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data

# Combine prevalence data with DHW from week before survey
prevalence_dhw <- prevalence_summary %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data

# ===============================================================
# 4. BASIC STATISTICS
# ===============================================================

cat("=== BASIC SUMMARY ===\n")
cat("Extent records with DHW data:", nrow(extent_dhw), "\n")
cat("Prevalence records with DHW data:", nrow(prevalence_dhw), "\n")
cat("Sites in common:", length(intersect(extent_dhw$site, prevalence_dhw$site)), "\n")
cat("Years covered:", paste(range(extent_dhw$year), collapse = " to "), "\n")

# Check correlations
cat("\n=== CORRELATIONS (Extent) ===\n")
extent_cor <- cor(extent_dhw$pre_survey_dhw, extent_dhw$mean_any_bleaching, use = "complete.obs")
cat("Pre-Survey DHW vs Any Bleaching Extent:", round(extent_cor, 3), "\n")

cat("\n=== CORRELATIONS (Prevalence) ===\n")
prevalence_cor <- cor(prevalence_dhw$pre_survey_dhw, prevalence_dhw$mean_any_bleaching, use = "complete.obs")
cat("Pre-Survey DHW vs Any Bleaching Prevalence:", round(prevalence_cor, 3), "\n")

# ===============================================================
# 5. SIMPLE PLOTS
# ===============================================================

# Plot 1: Extent vs DHW
p1 <- ggplot(extent_dhw, aes(x = pre_survey_dhw, y = mean_any_bleaching)) +
  geom_point(aes(color = factor(year)), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Pre-Survey DHW (°C-weeks)",
    y = "Mean Any Bleaching Extent (%)",
    title = "Coral Bleaching Extent vs Degree Heating Weeks",
    color = "Year"
  ) +
  theme_minimal()

print(p1)

# Plot 2: Prevalence vs DHW
p2 <- ggplot(prevalence_dhw, aes(x = pre_survey_dhw, y = mean_any_bleaching)) +
  geom_point(aes(color = factor(year)), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Pre-Survey DHW (°C-weeks)",
    y = "Mean Any Bleaching Prevalence (%)",
    title = "Coral Bleaching Prevalence vs Degree Heating Weeks",
    color = "Year"
  ) +
  theme_minimal()

print(p2)

# ===============================================================
# 6. SIMPLE LINEAR MODELS
# ===============================================================

# Model for extent
model_extent <- lm(mean_any_bleaching ~ pre_survey_dhw + factor(year), data = extent_dhw)
cat("\n=== EXTENT MODEL SUMMARY ===\n")
print(summary(model_extent))

# Model for prevalence
model_prevalence <- lm(mean_any_bleaching ~ pre_survey_dhw + factor(year), data = prevalence_dhw)
cat("\n=== PREVALENCE MODEL SUMMARY ===\n")
print(summary(model_prevalence))

# ===============================================================
# 7. SAVE RESULTS
# ===============================================================

# Save the merged datasets for further analysis
write.csv(extent_dhw, "simple_extent_presurvey_dhw.csv", row.names = FALSE)
write.csv(prevalence_dhw, "simple_prevalence_presurvey_dhw.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Saved files: simple_extent_presurvey_dhw.csv and simple_prevalence_presurvey_dhw.csv\n")