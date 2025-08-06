# Site-Specific Analysis: Detailed Coral Response Characterization
# Comprehensive analysis of individual site responses and recovery patterns

library(dplyr)
library(readr)
library(tidyr)
library(knitr)

# Load all processed data
extent_means <- read_csv("01_extent_site_means.csv")
temp_metrics <- read_csv("02_temperature_metrics_2023_2024.csv")
predictive_data <- read_csv("04_predictive_dataset.csv")
site_patterns <- read_csv("03_site_response_patterns.csv")

cat("=== COMPREHENSIVE SITE-SPECIFIC CORAL BLEACHING ANALYSIS ===\n")
cat("Analysis Period: 2023-2025 Bleaching Events\n")
cat("Focus: 2024 Annual → 2025 PBL Response Prediction\n\n")

# Create comprehensive site dataset
site_comprehensive <- predictive_data %>%
  left_join(
    site_patterns %>% select(site, response_pattern), 
    by = "site"
  ) %>%
  left_join(
    temp_metrics %>% filter(year == 2024) %>% 
    select(site, weeks_above_30, total_dhw_accumulation, cv_temperature),
    by = "site"
  ) %>%
  mutate(
    # Response classifications
    recovery_category = case_when(
      recovery_achieved > 30 ~ "Exceptional_Recovery",
      recovery_achieved > 15 ~ "Strong_Recovery", 
      recovery_achieved > 5 ~ "Moderate_Recovery",
      abs(response_magnitude) <= 5 ~ "Stable",
      response_magnitude > 15 ~ "Strong_Worsening",
      response_magnitude > 5 ~ "Moderate_Worsening",
      TRUE ~ "Minimal_Change"
    ),
    
    # Thermal stress classification
    thermal_stress_level = case_when(
      dhw_2024 > 12 ~ "Extreme_Stress",
      dhw_2024 > 8 ~ "High_Stress",
      dhw_2024 > 4 ~ "Moderate_Stress",
      TRUE ~ "Low_Stress"
    ),
    
    # Previous impact classification  
    previous_impact_level = case_when(
      predictor_2023_annual > 60 ~ "Severe_2023_Impact",
      predictor_2023_annual > 30 ~ "Moderate_2023_Impact", 
      predictor_2023_annual > 10 ~ "Low_2023_Impact",
      TRUE ~ "Minimal_2023_Impact"
    ),
    
    # Combined vulnerability index
    vulnerability_score = 
      (predictor_2023_annual / 100) * 0.4 +  # 40% weight to previous impact
      (dhw_2024 / 16) * 0.4 +                # 40% weight to current thermal stress  
      (temp_instability / max(temp_instability, na.rm = TRUE)) * 0.2  # 20% weight to instability
  ) %>%
  arrange(desc(recovery_achieved))

# Generate detailed site summaries
cat("=== TOP PERFORMING SITES (HIGHEST RECOVERY) ===\n")
top_recovery <- site_comprehensive %>%
  filter(recovery_achieved > 10) %>%
  arrange(desc(recovery_achieved)) %>%
  slice_head(n = 10)

for(i in 1:nrow(top_recovery)) {
  site_data <- top_recovery[i, ]
  cat(sprintf("\n%d. %s\n", i, site_data$site))
  cat(sprintf("   Recovery: %.1f%% reduction (%.1f%% → %.1f%%)\n", 
              site_data$recovery_achieved, site_data$baseline_2024_annual, site_data$outcome_2025_pbl))
  cat(sprintf("   2023 Bleaching: %.1f%%, 2024 DHW: %.1f\n", 
              site_data$predictor_2023_annual, site_data$dhw_2024))
  cat(sprintf("   Thermal Stress: %s, Previous Impact: %s\n",
              site_data$thermal_stress_level, site_data$previous_impact_level))
  cat(sprintf("   Vulnerability Score: %.3f\n", site_data$vulnerability_score))
}

cat("\n=== WORST PERFORMING SITES (HIGHEST WORSENING) ===\n")
worst_response <- site_comprehensive %>%
  filter(response_magnitude > 10) %>%
  arrange(desc(response_magnitude)) %>%
  slice_head(n = 8)

for(i in 1:nrow(worst_response)) {
  site_data <- worst_response[i, ]
  cat(sprintf("\n%d. %s\n", i, site_data$site))
  cat(sprintf("   Worsening: +%.1f%% increase (%.1f%% → %.1f%%)\n", 
              site_data$response_magnitude, site_data$baseline_2024_annual, site_data$outcome_2025_pbl))
  cat(sprintf("   2023 Bleaching: %.1f%%, 2024 DHW: %.1f\n", 
              site_data$predictor_2023_annual, site_data$dhw_2024))
  cat(sprintf("   Thermal Stress: %s, Previous Impact: %s\n",
              site_data$thermal_stress_level, site_data$previous_impact_level))
  cat(sprintf("   Vulnerability Score: %.3f\n", site_data$vulnerability_score))
}

# Statistical summaries by response category
cat("\n=== RESPONSE CATEGORY ANALYSIS ===\n")
category_analysis <- site_comprehensive %>%
  group_by(recovery_category) %>%
  summarise(
    n_sites = n(),
    mean_2023_bleaching = round(mean(predictor_2023_annual, na.rm = TRUE), 1),
    mean_2024_dhw = round(mean(dhw_2024, na.rm = TRUE), 1),
    mean_temp_instability = round(mean(temp_instability, na.rm = TRUE), 2),
    mean_vulnerability = round(mean(vulnerability_score, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sites))

print(category_analysis)

# Thermal stress pattern analysis
cat("\n=== THERMAL STRESS PATTERN EFFECTS ===\n")
thermal_analysis <- site_comprehensive %>%
  group_by(thermal_stress_level) %>%
  summarise(
    n_sites = n(),
    mean_response = round(mean(response_magnitude, na.rm = TRUE), 1),
    mean_recovery = round(mean(recovery_achieved, na.rm = TRUE), 1),
    recovery_sites = sum(recovery_achieved > 5, na.rm = TRUE),
    worsening_sites = sum(response_magnitude > 5, na.rm = TRUE),
    .groups = "drop"
  )

print(thermal_analysis)

# Previous impact pattern analysis  
cat("\n=== PREVIOUS YEAR IMPACT EFFECTS ===\n")
previous_impact_analysis <- site_comprehensive %>%
  group_by(previous_impact_level) %>%
  summarise(
    n_sites = n(),
    mean_response = round(mean(response_magnitude, na.rm = TRUE), 1),
    mean_recovery = round(mean(recovery_achieved, na.rm = TRUE), 1),
    proportion_recovering = round(mean(recovery_achieved > 5, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(previous_impact_analysis)

# Site clustering by vulnerability and response
cat("\n=== SITE VULNERABILITY CLUSTERS ===\n")

# High vulnerability, different outcomes
high_vulnerability <- site_comprehensive %>%
  filter(vulnerability_score > 0.6) %>%
  select(site, vulnerability_score, recovery_achieved, response_magnitude, 
         predictor_2023_annual, dhw_2024) %>%
  arrange(desc(vulnerability_score))

cat("High Vulnerability Sites (>0.6):\n")
print(high_vulnerability, n = Inf)

# Low vulnerability, different outcomes  
low_vulnerability <- site_comprehensive %>%
  filter(vulnerability_score < 0.3) %>%
  select(site, vulnerability_score, recovery_achieved, response_magnitude,
         predictor_2023_annual, dhw_2024) %>%
  arrange(vulnerability_score)

cat("\nLow Vulnerability Sites (<0.3):\n") 
print(low_vulnerability, n = Inf)

# Key numerical insights
cat("\n=== KEY NUMERICAL INSIGHTS ===\n")

total_sites <- nrow(site_comprehensive)
sites_with_recovery <- sum(site_comprehensive$recovery_achieved > 5, na.rm = TRUE)
sites_with_worsening <- sum(site_comprehensive$response_magnitude > 5, na.rm = TRUE)
sites_stable <- sum(abs(site_comprehensive$response_magnitude) <= 5, na.rm = TRUE)

strong_recovery_sites <- sum(site_comprehensive$recovery_achieved > 15, na.rm = TRUE)
exceptional_recovery_sites <- sum(site_comprehensive$recovery_achieved > 30, na.rm = TRUE)

mean_2023_bleaching <- round(mean(site_comprehensive$predictor_2023_annual, na.rm = TRUE), 1)
mean_2024_dhw <- round(mean(site_comprehensive$dhw_2024, na.rm = TRUE), 1)
mean_response <- round(mean(site_comprehensive$response_magnitude, na.rm = TRUE), 1)

cat(sprintf("Total analyzed sites: %d\n", total_sites))
cat(sprintf("Sites showing recovery (>5%% reduction): %d (%.1f%%)\n", 
            sites_with_recovery, sites_with_recovery/total_sites*100))
cat(sprintf("Sites showing worsening (>5%% increase): %d (%.1f%%)\n", 
            sites_with_worsening, sites_with_worsening/total_sites*100))
cat(sprintf("Stable sites (±5%% change): %d (%.1f%%)\n", 
            sites_stable, sites_stable/total_sites*100))
cat(sprintf("Strong recovery sites (>15%% reduction): %d\n", strong_recovery_sites))
cat(sprintf("Exceptional recovery sites (>30%% reduction): %d\n", exceptional_recovery_sites))
cat(sprintf("Mean 2023 bleaching extent: %.1f%%\n", mean_2023_bleaching))
cat(sprintf("Mean 2024 maximum DHW: %.1f\n", mean_2024_dhw))
cat(sprintf("Mean 2024→2025 response: %.1f%%\n", mean_response))

# Generate comprehensive site ranking
site_ranking <- site_comprehensive %>%
  mutate(
    resilience_score = 
      ifelse(recovery_achieved > 0, recovery_achieved, 0) * 0.6 +  # 60% recovery achieved
      pmax(0, -response_magnitude) * 0.4,  # 40% resistance to worsening
    
    rank_recovery = rank(desc(recovery_achieved), ties.method = "min"),
    rank_resilience = rank(desc(resilience_score), ties.method = "min"),
    rank_vulnerability = rank(vulnerability_score, ties.method = "min")
  ) %>%
  select(site, recovery_achieved, response_magnitude, resilience_score, vulnerability_score,
         rank_recovery, rank_resilience, rank_vulnerability, thermal_stress_level, 
         previous_impact_level, predictor_2023_annual, dhw_2024) %>%
  arrange(rank_resilience)

# Save all analysis results
write_csv(site_comprehensive, "06_site_comprehensive_analysis.csv")
write_csv(site_ranking, "06_site_resilience_ranking.csv")
write_csv(category_analysis, "06_response_category_summary.csv")
write_csv(thermal_analysis, "06_thermal_stress_analysis.csv")
write_csv(previous_impact_analysis, "06_previous_impact_analysis.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Site-specific analysis files saved:\n")
cat("- 06_site_comprehensive_analysis.csv\n")
cat("- 06_site_resilience_ranking.csv\n")
cat("- 06_response_category_summary.csv\n")
cat("- 06_thermal_stress_analysis.csv\n")
cat("- 06_previous_impact_analysis.csv\n")