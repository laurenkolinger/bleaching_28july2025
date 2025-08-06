# Master Analysis Script: Comprehensive Coral Bleaching Response Analysis
# Executes complete analysis pipeline and generates final summary

library(dplyr)
library(readr)

cat("========================================\n")
cat("CORAL BLEACHING ANALYSIS PIPELINE\n")
cat("Analysis Period: 2023-2025\n")
cat("Objective: Evaluate 2024 coral response prediction\n")
cat("========================================\n\n")

# Set working directory to exploration folder
setwd("/workspace/exploration")

# Execute analysis pipeline
analysis_start_time <- Sys.time()

cat("Step 1: Loading and processing coral condition data...\n")
source("01_coral_bleaching_analysis.R")
cat("✓ Completed: Basic data loading and site-level calculations\n\n")

cat("Step 2: Analyzing temperature and DHW patterns...\n")
source("02_temperature_dhw_analysis.R")
cat("✓ Completed: Thermal stress metrics and variability analysis\n\n")

cat("Step 3: Examining bleaching response and recovery patterns...\n")
source("03_bleaching_response_analysis.R")
cat("✓ Completed: Recovery trajectory analysis for both time periods\n\n")

cat("Step 4: Conducting predictive analysis...\n")
source("04_predictive_analysis.R")
cat("✓ Completed: Comparative predictive power analysis\n\n")

cat("Step 5: Generating visualizations...\n")
source("05_visualization_analysis.R")
cat("✓ Completed: Comprehensive visualization suite\n\n")

cat("Step 6: Performing site-specific analysis...\n")
source("06_site_specific_analysis.R")
cat("✓ Completed: Detailed site characterization and ranking\n\n")

analysis_end_time <- Sys.time()
analysis_duration <- round(as.numeric(difftime(analysis_end_time, analysis_start_time, units = "mins")), 2)

# Generate final summary report
cat("========================================\n")
cat("FINAL ANALYSIS SUMMARY REPORT\n")
cat("========================================\n\n")

# Load key results for summary
correlation_data <- read_csv("04_correlation_analysis.csv")
model_comparison <- read_csv("04_model_comparison.csv")
site_comprehensive <- read_csv("06_site_comprehensive_analysis.csv")
thermal_analysis <- read_csv("06_thermal_stress_analysis.csv")

cat("=== PREDICTIVE POWER COMPARISON ===\n")
cat("Correlation with 2024→2025 response magnitude:\n")
top_predictors <- correlation_data %>%
  arrange(desc(abs(correlation_with_2025_response))) %>%
  slice_head(n = 5)

for(i in 1:nrow(top_predictors)) {
  cat(sprintf("%d. %s: r = %.3f\n", i, top_predictors$predictor[i], top_predictors$correlation_with_2025_response[i]))
}

cat("\nModel Performance (R-squared):\n")
for(i in 1:nrow(model_comparison)) {
  cat(sprintf("%s: %.4f\n", model_comparison$model[i], model_comparison$r_squared[i]))
}

cat("\n=== THERMAL STRESS IMPACT SUMMARY ===\n")
print(thermal_analysis)

cat("\n=== SITE PERFORMANCE OVERVIEW ===\n")
total_sites <- nrow(site_comprehensive)
recovery_sites <- sum(site_comprehensive$recovery_achieved > 5, na.rm = TRUE)
worsening_sites <- sum(site_comprehensive$response_magnitude > 5, na.rm = TRUE)
stable_sites <- sum(abs(site_comprehensive$response_magnitude) <= 5, na.rm = TRUE)

cat(sprintf("Total sites analyzed: %d\n", total_sites))
cat(sprintf("Sites with recovery (>5%% reduction): %d (%.1f%%)\n", 
            recovery_sites, recovery_sites/total_sites*100))
cat(sprintf("Sites with worsening (>5%% increase): %d (%.1f%%)\n", 
            worsening_sites, worsening_sites/total_sites*100))
cat(sprintf("Stable sites (±5%%): %d (%.1f%%)\n", 
            stable_sites, stable_sites/total_sites*100))

# Exceptional performers
exceptional_recovery <- site_comprehensive %>%
  filter(recovery_achieved > 30) %>%
  arrange(desc(recovery_achieved))

extreme_worsening <- site_comprehensive %>%
  filter(response_magnitude > 30) %>%
  arrange(desc(response_magnitude))

cat(sprintf("\nExceptional recovery sites (>30%% reduction): %d\n", nrow(exceptional_recovery)))
if(nrow(exceptional_recovery) > 0) {
  cat("Sites: ", paste(exceptional_recovery$site, collapse = ", "), "\n")
}

cat(sprintf("Extreme worsening sites (>30%% increase): %d\n", nrow(extreme_worsening)))
if(nrow(extreme_worsening) > 0) {
  cat("Sites: ", paste(extreme_worsening$site, collapse = ", "), "\n")
}

cat("\n=== KEY INSIGHTS ===\n")

# Calculate key insight metrics
mean_2023_bleaching <- round(mean(site_comprehensive$predictor_2023_annual, na.rm = TRUE), 1)
mean_2024_dhw <- round(mean(site_comprehensive$dhw_2024, na.rm = TRUE), 1)
mean_response <- round(mean(site_comprehensive$response_magnitude, na.rm = TRUE), 1)

# Best predictor information
best_predictor <- correlation_data %>% 
  filter(abs(correlation_with_2025_response) == max(abs(correlation_with_2025_response)))

# DHW vs previous bleaching comparison
dhw_correlation <- correlation_data %>% filter(predictor == "2024_DHW") %>% pull(correlation_with_2025_response)
bleaching_correlation <- correlation_data %>% filter(predictor == "2023_Bleaching") %>% pull(correlation_with_2025_response)

cat("1. PREDICTIVE RELATIONSHIPS:\n")
cat(sprintf("   - Strongest predictor: %s (r = %.3f)\n", best_predictor$predictor, best_predictor$correlation_with_2025_response))
cat(sprintf("   - 2024 DHW correlation: %.3f\n", dhw_correlation))
cat(sprintf("   - 2023 Bleaching correlation: %.3f\n", bleaching_correlation))

stronger_predictor <- ifelse(abs(dhw_correlation) > abs(bleaching_correlation), "2024 DHW", "2023 Bleaching")
cat(sprintf("   - %s is the stronger predictor\n", stronger_predictor))

cat("\n2. RECOVERY PATTERNS:\n")
period1_recovery <- mean(site_comprehensive$impact_2023_to_2024, na.rm = TRUE)
period2_recovery <- mean(site_comprehensive$response_magnitude, na.rm = TRUE) * -1  # Convert to recovery direction

cat(sprintf("   - Mean 2023→2024 PBL change: %.1f%%\n", period1_recovery))
cat(sprintf("   - Mean 2024→2025 PBL recovery: %.1f%%\n", period2_recovery))

cat("\n3. THERMAL STRESS EFFECTS:\n")
high_dhw_sites <- sum(site_comprehensive$dhw_2024 > 8, na.rm = TRUE)
extreme_dhw_sites <- sum(site_comprehensive$dhw_2024 > 12, na.rm = TRUE)

cat(sprintf("   - Sites with high DHW (>8): %d (%.1f%%)\n", 
            high_dhw_sites, high_dhw_sites/total_sites*100))
cat(sprintf("   - Sites with extreme DHW (>12): %d (%.1f%%)\n", 
            extreme_dhw_sites, extreme_dhw_sites/total_sites*100))

cat("\n4. VULNERABILITY PATTERNS:\n")
high_vuln_sites <- sum(site_comprehensive$vulnerability_score > 0.6, na.rm = TRUE)
low_vuln_sites <- sum(site_comprehensive$vulnerability_score < 0.3, na.rm = TRUE)

cat(sprintf("   - High vulnerability sites (>0.6): %d\n", high_vuln_sites))
cat(sprintf("   - Low vulnerability sites (<0.3): %d\n", low_vuln_sites))

cat("\n=== FILES GENERATED ===\n")
generated_files <- c(
  "01_extent_site_means.csv",
  "01_prevalence_site_means.csv", 
  "01_mortality_site_means.csv",
  "02_temperature_metrics_2023_2024.csv",
  "02_cumulative_thermal_stress.csv",
  "02_thermal_stress_patterns.csv",
  "03_recovery_metrics_both_periods.csv",
  "03_recovery_with_thermal_data.csv",
  "03_site_response_patterns.csv",
  "04_predictive_dataset.csv",
  "04_correlation_analysis.csv",
  "04_model_comparison.csv",
  "04_extreme_responders.csv",
  "05_plot_bleaching_trajectories.png",
  "05_plot_recovery_comparison.png",
  "05_plot_dhw_vs_response.png",
  "05_plot_previous_bleaching_vs_response.png",
  "05_plot_response_patterns.png",
  "05_plot_temperature_variability.png",
  "05_plot_top_recovery_sites.png",
  "05_plot_correlation_matrix.png",
  "05_dashboard_summary.png",
  "06_site_comprehensive_analysis.csv",
  "06_site_resilience_ranking.csv",
  "06_response_category_summary.csv",
  "06_thermal_stress_analysis.csv",
  "06_previous_impact_analysis.csv"
)

cat("Data files:\n")
data_files <- generated_files[grepl("\\.csv$", generated_files)]
for(file in data_files) {
  cat(sprintf("  - %s\n", file))
}

cat("\nVisualization files:\n")
plot_files <- generated_files[grepl("\\.png$", generated_files)]
for(file in plot_files) {
  cat(sprintf("  - %s\n", file))
}

cat(sprintf("\nTotal analysis runtime: %.2f minutes\n", analysis_duration))
cat("\n========================================\n")
cat("ANALYSIS PIPELINE COMPLETED SUCCESSFULLY\n")
cat("========================================\n")