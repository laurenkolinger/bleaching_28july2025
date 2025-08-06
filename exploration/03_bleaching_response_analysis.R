# Coral Bleaching Response and Recovery Analysis
# Examining 2023 Annual -> 2024 PBL vs 2024 Annual -> 2025 PBL patterns

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Load processed data
extent_means <- read_csv("01_extent_site_means.csv")
prevalence_means <- read_csv("01_prevalence_site_means.csv")
mortality_means <- read_csv("01_mortality_site_means.csv")
temp_metrics <- read_csv("02_temperature_metrics_2023_2024.csv")

cat("Loaded processed datasets for response analysis\n")

# Create bleaching response periods dataset
# Period 1: 2023 Annual -> 2024 PBL (preceding year impact)
# Period 2: 2024 Annual -> 2025 PBL (response of interest)

# Extract key timepoints for extent data
extent_key_times <- extent_means %>%
  filter(
    (year == 2023 & period == "Annual") |
    (year == 2024 & period == "PBL") |
    (year == 2024 & period == "Annual") |
    (year == 2025 & period == "PBL")
  ) %>%
  select(site, year, period, ext_anybleaching, ext_bleached, ext_nobleaching) %>%
  mutate(timepoint = paste(year, period, sep = "_"))

cat("Sites with all four key timepoints:", 
    length(intersect(
      extent_key_times$site[extent_key_times$timepoint == "2023_Annual"],
      intersect(
        extent_key_times$site[extent_key_times$timepoint == "2024_PBL"],
        intersect(
          extent_key_times$site[extent_key_times$timepoint == "2024_Annual"],
          extent_key_times$site[extent_key_times$timepoint == "2025_PBL"]
        )
      )
    )), "\n")

# Calculate recovery metrics for each period
calculate_recovery_metrics <- function(annual_bleaching, pbl_bleaching) {
  recovery_rate = pmax(0, annual_bleaching - pbl_bleaching)
  recovery_proportion = ifelse(annual_bleaching > 0, recovery_rate / annual_bleaching, 0)
  persistence_rate = pmin(annual_bleaching, pbl_bleaching)
  
  list(
    recovery_rate = recovery_rate,
    recovery_proportion = recovery_proportion,
    persistence_rate = persistence_rate,
    net_change = pbl_bleaching - annual_bleaching
  )
}

# Process Period 1: 2023 Annual -> 2024 PBL
period1_data <- extent_key_times %>%
  filter(timepoint %in% c("2023_Annual", "2024_PBL")) %>%
  select(site, timepoint, ext_anybleaching) %>%
  pivot_wider(names_from = timepoint, values_from = ext_anybleaching, names_prefix = "bleaching_") %>%
  filter(!is.na(bleaching_2023_Annual) & !is.na(bleaching_2024_PBL)) %>%
  mutate(
    period = "2023_to_2024_PBL",
    initial_bleaching = bleaching_2023_Annual,
    final_bleaching = bleaching_2024_PBL
  )

period1_metrics <- period1_data %>%
  rowwise() %>%
  mutate(
    recovery_metrics = list(calculate_recovery_metrics(initial_bleaching, final_bleaching))
  ) %>%
  unnest_wider(recovery_metrics) %>%
  select(site, period, initial_bleaching, final_bleaching, recovery_rate, recovery_proportion, persistence_rate, net_change)

# Process Period 2: 2024 Annual -> 2025 PBL
period2_data <- extent_key_times %>%
  filter(timepoint %in% c("2024_Annual", "2025_PBL")) %>%
  select(site, timepoint, ext_anybleaching) %>%
  pivot_wider(names_from = timepoint, values_from = ext_anybleaching, names_prefix = "bleaching_") %>%
  filter(!is.na(bleaching_2024_Annual) & !is.na(bleaching_2025_PBL)) %>%
  mutate(
    period = "2024_to_2025_PBL",
    initial_bleaching = bleaching_2024_Annual,
    final_bleaching = bleaching_2025_PBL
  )

period2_metrics <- period2_data %>%
  rowwise() %>%
  mutate(
    recovery_metrics = list(calculate_recovery_metrics(initial_bleaching, final_bleaching))
  ) %>%
  unnest_wider(recovery_metrics) %>%
  select(site, period, initial_bleaching, final_bleaching, recovery_rate, recovery_proportion, persistence_rate, net_change)

# Combine both periods
combined_recovery <- bind_rows(period1_metrics, period2_metrics)

# Add DHW data for predictive analysis
dhw_2023 <- temp_metrics %>% filter(year == 2023) %>% select(site, max_dhw_2023 = max_dhw, temp_sd_2023 = temp_sd)
dhw_2024 <- temp_metrics %>% filter(year == 2024) %>% select(site, max_dhw_2024 = max_dhw, temp_sd_2024 = temp_sd)

# Merge with DHW data
recovery_with_dhw <- combined_recovery %>%
  left_join(dhw_2023, by = "site") %>%
  left_join(dhw_2024, by = "site") %>%
  mutate(
    relevant_dhw = ifelse(period == "2023_to_2024_PBL", max_dhw_2023, max_dhw_2024),
    relevant_temp_sd = ifelse(period == "2023_to_2024_PBL", temp_sd_2023, temp_sd_2024)
  )

# Calculate summary statistics
cat("\n=== RECOVERY ANALYSIS SUMMARY ===\n")

period1_summary <- period1_metrics %>%
  summarise(
    n_sites = n(),
    mean_initial_bleaching = round(mean(initial_bleaching, na.rm = TRUE), 2),
    mean_final_bleaching = round(mean(final_bleaching, na.rm = TRUE), 2),
    mean_recovery_rate = round(mean(recovery_rate, na.rm = TRUE), 2),
    mean_recovery_proportion = round(mean(recovery_proportion, na.rm = TRUE), 2),
    sites_with_recovery = sum(recovery_rate > 5, na.rm = TRUE),
    sites_with_worsening = sum(net_change > 5, na.rm = TRUE)
  )

period2_summary <- period2_metrics %>%
  summarise(
    n_sites = n(),
    mean_initial_bleaching = round(mean(initial_bleaching, na.rm = TRUE), 2),
    mean_final_bleaching = round(mean(final_bleaching, na.rm = TRUE), 2),
    mean_recovery_rate = round(mean(recovery_rate, na.rm = TRUE), 2),
    mean_recovery_proportion = round(mean(recovery_proportion, na.rm = TRUE), 2),
    sites_with_recovery = sum(recovery_rate > 5, na.rm = TRUE),
    sites_with_worsening = sum(net_change > 5, na.rm = TRUE)
  )

cat("Period 1 (2023 Annual -> 2024 PBL):\n")
print(period1_summary)
cat("\nPeriod 2 (2024 Annual -> 2025 PBL):\n")
print(period2_summary)

# Identify sites with different response patterns
site_patterns <- period1_metrics %>%
  inner_join(period2_metrics, by = "site", suffix = c("_p1", "_p2")) %>%
  mutate(
    response_pattern = case_when(
      recovery_rate_p1 > 10 & recovery_rate_p2 > 10 ~ "Consistent_Recovery",
      recovery_rate_p1 > 10 & recovery_rate_p2 <= 5 ~ "Early_Recovery_Later_Persistence",
      recovery_rate_p1 <= 5 & recovery_rate_p2 > 10 ~ "Late_Recovery",
      net_change_p1 > 5 & net_change_p2 > 5 ~ "Consistent_Worsening",
      abs(net_change_p1) <= 5 & abs(net_change_p2) <= 5 ~ "Stable",
      TRUE ~ "Variable"
    )
  )

cat("\n=== SITE RESPONSE PATTERNS ===\n")
pattern_counts <- site_patterns %>%
  count(response_pattern) %>%
  arrange(desc(n))
print(pattern_counts)

# Save analysis results
write_csv(combined_recovery, "03_recovery_metrics_both_periods.csv")
write_csv(recovery_with_dhw, "03_recovery_with_thermal_data.csv")
write_csv(site_patterns, "03_site_response_patterns.csv")

cat("\nBleaching response analysis complete. Files saved:\n")
cat("- 03_recovery_metrics_both_periods.csv\n")
cat("- 03_recovery_with_thermal_data.csv\n") 
cat("- 03_site_response_patterns.csv\n")