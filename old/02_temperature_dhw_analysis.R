# Temperature and DHW Analysis for 2023-2024 Bleaching Events
# Calculate thermal stress metrics and temperature variability

library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)

# Load temperature data
temp_data <- read_csv("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

cat("Temperature data loaded. Dimensions:", dim(temp_data), "\n")
cat("Year range:", range(temp_data$year, na.rm = TRUE), "\n")
cat("Sites in temperature data:", length(unique(temp_data$site)), "\n")

# Filter for 2023-2024 analysis period
temp_analysis <- temp_data %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(!is.na(dhw) & !is.na(weekly_max_temp))

cat("Temperature records for 2023-2024:", nrow(temp_analysis), "\n")
cat("Sites with 2023-2024 temperature data:", length(unique(temp_analysis$site)), "\n")

# Calculate annual maximum DHW for each site-year
annual_max_dhw <- temp_analysis %>%
  group_by(site, year) %>%
  summarise(
    max_dhw = max(dhw, na.rm = TRUE),
    max_weekly_temp = max(weekly_max_temp, na.rm = TRUE),
    mean_weekly_temp = mean(weekly_max_temp, na.rm = TRUE),
    temp_range = max(weekly_max_temp, na.rm = TRUE) - min(weekly_max_temp, na.rm = TRUE),
    temp_sd = sd(weekly_max_temp, na.rm = TRUE),
    weeks_above_29 = sum(weekly_max_temp > 29, na.rm = TRUE),
    weeks_above_30 = sum(weekly_max_temp > 30, na.rm = TRUE),
    weeks_with_dhw = sum(dhw > 0, na.rm = TRUE),
    total_dhw_accumulation = sum(dhw, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate temperature variability metrics
temp_variability <- temp_analysis %>%
  group_by(site, year) %>%
  arrange(week) %>%
  mutate(
    temp_change = abs(weekly_max_temp - lag(weekly_max_temp)),
    temp_increasing = weekly_max_temp > lag(weekly_max_temp)
  ) %>%
  summarise(
    mean_weekly_change = mean(temp_change, na.rm = TRUE),
    max_weekly_change = max(temp_change, na.rm = TRUE),
    weeks_increasing = sum(temp_increasing, na.rm = TRUE),
    weeks_decreasing = sum(!temp_increasing, na.rm = TRUE),
    cv_temperature = sd(weekly_max_temp, na.rm = TRUE) / mean(weekly_max_temp, na.rm = TRUE),
    .groups = "drop"
  )

# Merge temperature metrics
temp_metrics <- annual_max_dhw %>%
  left_join(temp_variability, by = c("site", "year"))

# Create detailed summary statistics
cat("\n=== DHW SUMMARY STATISTICS ===\n")
dhw_2023 <- temp_metrics %>% filter(year == 2023)
dhw_2024 <- temp_metrics %>% filter(year == 2024)

cat("2023 DHW Statistics:\n")
cat("  Mean max DHW:", round(mean(dhw_2023$max_dhw, na.rm = TRUE), 2), "\n")
cat("  Range max DHW:", round(range(dhw_2023$max_dhw, na.rm = TRUE), 2), "\n")
cat("  Sites with DHW > 4:", sum(dhw_2023$max_dhw > 4, na.rm = TRUE), "\n")
cat("  Sites with DHW > 8:", sum(dhw_2023$max_dhw > 8, na.rm = TRUE), "\n")

cat("\n2024 DHW Statistics:\n")
cat("  Mean max DHW:", round(mean(dhw_2024$max_dhw, na.rm = TRUE), 2), "\n")
cat("  Range max DHW:", round(range(dhw_2024$max_dhw, na.rm = TRUE), 2), "\n")
cat("  Sites with DHW > 4:", sum(dhw_2024$max_dhw > 4, na.rm = TRUE), "\n")
cat("  Sites with DHW > 8:", sum(dhw_2024$max_dhw > 8, na.rm = TRUE), "\n")

# Calculate cumulative thermal stress
cumulative_stress <- temp_metrics %>%
  select(site, year, max_dhw, total_dhw_accumulation, weeks_with_dhw) %>%
  pivot_wider(names_from = year, 
              values_from = c(max_dhw, total_dhw_accumulation, weeks_with_dhw),
              names_sep = "_") %>%
  mutate(
    cumulative_max_dhw = pmax(max_dhw_2023, max_dhw_2024, na.rm = TRUE),
    total_stress_2years = total_dhw_accumulation_2023 + total_dhw_accumulation_2024,
    consecutive_stress_weeks = weeks_with_dhw_2023 + weeks_with_dhw_2024
  )

# Identify sites with different thermal stress patterns
thermal_stress_patterns <- cumulative_stress %>%
  mutate(
    stress_pattern = case_when(
      max_dhw_2023 > 8 & max_dhw_2024 > 8 ~ "High_Both_Years",
      max_dhw_2023 > 8 & max_dhw_2024 <= 8 ~ "High_2023_Moderate_2024",
      max_dhw_2023 <= 8 & max_dhw_2024 > 8 ~ "Moderate_2023_High_2024",
      max_dhw_2023 > 4 & max_dhw_2024 > 4 ~ "Moderate_Both_Years",
      TRUE ~ "Low_Stress"
    )
  )

cat("\n=== THERMAL STRESS PATTERNS ===\n")
pattern_summary <- thermal_stress_patterns %>%
  count(stress_pattern) %>%
  arrange(desc(n))
print(pattern_summary)

# Save temperature analysis results
write_csv(temp_metrics, "02_temperature_metrics_2023_2024.csv")
write_csv(cumulative_stress, "02_cumulative_thermal_stress.csv")
write_csv(thermal_stress_patterns, "02_thermal_stress_patterns.csv")

cat("\nTemperature analysis complete. Files saved:\n")
cat("- 02_temperature_metrics_2023_2024.csv\n")
cat("- 02_cumulative_thermal_stress.csv\n")
cat("- 02_thermal_stress_patterns.csv\n")