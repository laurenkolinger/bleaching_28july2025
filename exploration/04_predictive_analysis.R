# Predictive Analysis: Previous Year Bleaching vs DHW in Predicting Coral Response
# Comparing predictive power of 2023 bleaching vs thermal stress metrics

library(dplyr)
library(ggplot2)
library(readr)
library(corrplot)
library(broom)

# Load processed data
extent_means <- read_csv("01_extent_site_means.csv")
temp_metrics <- read_csv("02_temperature_metrics_2023_2024.csv")
recovery_data <- read_csv("03_recovery_with_thermal_data.csv")

cat("Loaded data for predictive analysis\n")

# Create comprehensive predictive dataset
# Response variable: 2024 Annual -> 2025 PBL response
# Predictors: 2023 bleaching, 2024 DHW, temperature variability

# Get 2024 Annual bleaching as response baseline
response_baseline <- extent_means %>%
  filter(year == 2024, period == "Annual") %>%
  select(site, baseline_2024_annual = ext_anybleaching)

# Get 2025 PBL as outcome
outcome_2025 <- extent_means %>%
  filter(year == 2025, period == "PBL") %>%
  select(site, outcome_2025_pbl = ext_anybleaching)

# Get 2023 Annual as predictor (previous year impact)
predictor_2023 <- extent_means %>%
  filter(year == 2023, period == "Annual") %>%
  select(site, predictor_2023_annual = ext_anybleaching)

# Get 2024 PBL to assess persistence from 2023
persistence_2024 <- extent_means %>%
  filter(year == 2024, period == "PBL") %>%
  select(site, persistence_2024_pbl = ext_anybleaching)

# Merge all components
predictive_dataset <- response_baseline %>%
  inner_join(outcome_2025, by = "site") %>%
  inner_join(predictor_2023, by = "site") %>%
  inner_join(persistence_2024, by = "site") %>%
  left_join(temp_metrics %>% filter(year == 2023) %>% 
            select(site, dhw_2023 = max_dhw, temp_sd_2023 = temp_sd, temp_range_2023 = temp_range), 
            by = "site") %>%
  left_join(temp_metrics %>% filter(year == 2024) %>% 
            select(site, dhw_2024 = max_dhw, temp_sd_2024 = temp_sd, temp_range_2024 = temp_range), 
            by = "site") %>%
  mutate(
    # Response metrics
    response_magnitude = outcome_2025_pbl - baseline_2024_annual,
    recovery_achieved = pmax(0, baseline_2024_annual - outcome_2025_pbl),
    proportional_recovery = ifelse(baseline_2024_annual > 0, recovery_achieved / baseline_2024_annual, 0),
    
    # Predictive stress measures
    cumulative_dhw = dhw_2023 + dhw_2024,
    max_annual_dhw = pmax(dhw_2023, dhw_2024, na.rm = TRUE),
    temp_instability = temp_sd_2023 + temp_sd_2024,
    
    # Previous impact measures
    impact_2023_to_2024 = persistence_2024_pbl - predictor_2023_annual,
    bleaching_persistence = pmin(predictor_2023_annual, persistence_2024_pbl) / pmax(predictor_2023_annual, 1)
  ) %>%
  filter(!is.na(dhw_2023) & !is.na(dhw_2024))

cat("Predictive dataset created with", nrow(predictive_dataset), "sites\n")

# Correlation analysis
correlation_vars <- predictive_dataset %>%
  select(response_magnitude, recovery_achieved, proportional_recovery,
         predictor_2023_annual, dhw_2023, dhw_2024, cumulative_dhw, max_annual_dhw,
         temp_sd_2023, temp_sd_2024, temp_instability, impact_2023_to_2024, bleaching_persistence) %>%
  filter(complete.cases(.))

correlation_matrix <- cor(correlation_vars, use = "complete.obs")

# Extract key correlations for reporting
key_correlations <- data.frame(
  predictor = c("2023_Bleaching", "2024_DHW", "2023_DHW", "Cumulative_DHW", "Max_DHW", "Temp_Instability", "Bleaching_Persistence"),
  correlation_with_2025_response = c(
    correlation_matrix["response_magnitude", "predictor_2023_annual"],
    correlation_matrix["response_magnitude", "dhw_2024"],
    correlation_matrix["response_magnitude", "dhw_2023"],
    correlation_matrix["response_magnitude", "cumulative_dhw"],
    correlation_matrix["response_magnitude", "max_annual_dhw"],
    correlation_matrix["response_magnitude", "temp_instability"],
    correlation_matrix["response_magnitude", "bleaching_persistence"]
  ),
  correlation_with_recovery = c(
    correlation_matrix["recovery_achieved", "predictor_2023_annual"],
    correlation_matrix["recovery_achieved", "dhw_2024"],
    correlation_matrix["recovery_achieved", "dhw_2023"],
    correlation_matrix["recovery_achieved", "cumulative_dhw"],
    correlation_matrix["recovery_achieved", "max_annual_dhw"],
    correlation_matrix["recovery_achieved", "temp_instability"],
    correlation_matrix["recovery_achieved", "bleaching_persistence"]
  )
) %>%
  mutate(
    correlation_with_2025_response = round(correlation_with_2025_response, 3),
    correlation_with_recovery = round(correlation_with_recovery, 3),
    abs_response_cor = abs(correlation_with_2025_response),
    abs_recovery_cor = abs(correlation_with_recovery)
  ) %>%
  arrange(desc(abs_response_cor))

# Linear models to compare predictive power
model_2023_bleaching <- lm(response_magnitude ~ predictor_2023_annual, data = predictive_dataset)
model_2024_dhw <- lm(response_magnitude ~ dhw_2024, data = predictive_dataset)
model_combined <- lm(response_magnitude ~ predictor_2023_annual + dhw_2024, data = predictive_dataset)
model_comprehensive <- lm(response_magnitude ~ predictor_2023_annual + dhw_2024 + temp_instability + bleaching_persistence, data = predictive_dataset)

# Model comparison
model_comparison <- data.frame(
  model = c("2023_Bleaching_Only", "2024_DHW_Only", "Combined", "Comprehensive"),
  r_squared = c(
    summary(model_2023_bleaching)$r.squared,
    summary(model_2024_dhw)$r.squared,
    summary(model_combined)$r.squared,
    summary(model_comprehensive)$r.squared
  ),
  adj_r_squared = c(
    summary(model_2023_bleaching)$adj.r.squared,
    summary(model_2024_dhw)$adj.r.squared,
    summary(model_combined)$adj.r.squared,
    summary(model_comprehensive)$adj.r.squared
  ),
  rmse = c(
    sqrt(mean(residuals(model_2023_bleaching)^2)),
    sqrt(mean(residuals(model_2024_dhw)^2)),
    sqrt(mean(residuals(model_combined)^2)),
    sqrt(mean(residuals(model_comprehensive)^2))
  )
) %>%
  mutate(
    r_squared = round(r_squared, 4),
    adj_r_squared = round(adj_r_squared, 4),
    rmse = round(rmse, 2)
  ) %>%
  arrange(desc(adj_r_squared))

# Detailed analysis of extreme responders
extreme_responders <- predictive_dataset %>%
  mutate(
    response_category = case_when(
      recovery_achieved > 20 ~ "High_Recovery",
      response_magnitude > 20 ~ "Strong_Worsening", 
      abs(response_magnitude) <= 5 ~ "Stable",
      recovery_achieved > 5 ~ "Moderate_Recovery",
      response_magnitude > 5 ~ "Moderate_Worsening",
      TRUE ~ "Minimal_Change"
    )
  ) %>%
  filter(response_category %in% c("High_Recovery", "Strong_Worsening"))

cat("\n=== PREDICTIVE ANALYSIS RESULTS ===\n")
cat("Correlations with 2024->2025 response magnitude:\n")
print(key_correlations[, c("predictor", "correlation_with_2025_response")])

cat("\nModel comparison (predicting 2024->2025 response):\n")
print(model_comparison)

cat("\nExtreme responder analysis:\n")
extreme_summary <- extreme_responders %>%
  group_by(response_category) %>%
  summarise(
    n_sites = n(),
    mean_2023_bleaching = round(mean(predictor_2023_annual, na.rm = TRUE), 1),
    mean_2024_dhw = round(mean(dhw_2024, na.rm = TRUE), 1),
    mean_response = round(mean(response_magnitude, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(extreme_summary)

# Save predictive analysis results
write_csv(predictive_dataset, "04_predictive_dataset.csv")
write_csv(key_correlations, "04_correlation_analysis.csv")
write_csv(model_comparison, "04_model_comparison.csv")
write_csv(extreme_responders, "04_extreme_responders.csv")

cat("\nPredictive analysis complete. Files saved:\n")
cat("- 04_predictive_dataset.csv\n")
cat("- 04_correlation_analysis.csv\n")
cat("- 04_model_comparison.csv\n")
cat("- 04_extreme_responders.csv\n")