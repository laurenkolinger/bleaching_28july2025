# Coral Bleaching Analysis: 2023-2024 Response and Recovery Patterns
# Analysis of coral response in 2024 and predictive power of 2023 bleaching vs DHW

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(corrplot)
library(gridExtra)
library(viridis)

# Load datasets
extent_data <- read_csv("s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv")
prevalence_data <- read_csv("s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv")
mortality_data <- read_csv("s3pt1_coralcondition_mortalityprevalence_33sites_2022_2025.csv")
temp_data <- read_csv("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

# Check data structure
cat("Extent data dimensions:", dim(extent_data), "\n")
cat("Prevalence data dimensions:", dim(prevalence_data), "\n")
cat("Mortality data dimensions:", dim(mortality_data), "\n")
cat("Temperature data dimensions:", dim(temp_data), "\n")

cat("\nUnique periods in extent data:", unique(extent_data$period), "\n")
cat("Years available:", unique(extent_data$year), "\n")
cat("Number of sites:", length(unique(extent_data$site)), "\n")

# Function to calculate site-level means across replicates
calculate_site_means <- function(data, group_vars = c("site", "year", "period")) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

# Calculate site-level means for all datasets
extent_means <- calculate_site_means(extent_data)
prevalence_means <- calculate_site_means(prevalence_data)
mortality_means <- calculate_site_means(mortality_data)

# Examine the key timepoints
key_timepoints <- c("2023_Annual", "2024_PBL", "2024_Annual", "2025_PBL")
cat("\nExamining key timepoints for analysis:\n")
for(tp in key_timepoints) {
  year <- as.numeric(substr(tp, 1, 4))
  period <- substr(tp, 6, nchar(tp))
  count <- extent_means %>% filter(year == !!year, period == !!period) %>% nrow()
  cat(tp, ":", count, "site records\n")
}

# Save initial data summary
write_csv(extent_means, "01_extent_site_means.csv")
write_csv(prevalence_means, "01_prevalence_site_means.csv")
write_csv(mortality_means, "01_mortality_site_means.csv")

cat("\nInitial data loading and processing complete.\n")
cat("Site-level means calculated and saved.\n")