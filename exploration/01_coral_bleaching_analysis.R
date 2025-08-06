# ============================================================================
# 01: Comprehensive Coral Bleaching Data Loading and Initial Analysis
# ============================================================================
# Purpose: Load coral condition datasets, perform initial data exploration,
#          calculate site-level statistics, and establish data-driven thresholds
#          using actual quartiles rather than arbitrary cutoffs
# Author: Coral Bleaching Analysis Pipeline
# Date: Analysis of 2023-2025 bleaching events
# ============================================================================

# Load required libraries with explicit purpose for each
library(dplyr)        # Data manipulation and summarization
library(ggplot2)      # Advanced plotting and visualization
library(tidyr)        # Data reshaping and tidying
library(readr)        # Efficient CSV reading
library(corrplot)     # Correlation matrix visualization
library(gridExtra)    # Multiple plot arrangement
library(viridis)      # Perceptually uniform color scales
library(scales)       # Scale functions for plots

cat("============================================================================\n")
cat("CORAL BLEACHING DATA LOADING AND INITIAL ANALYSIS\n")
cat("============================================================================\n\n")

# ============================================================================
# DATA LOADING AND INITIAL EXPLORATION
# ============================================================================

cat("STEP 1: Loading coral condition datasets\n")
cat("----------------------------------------\n")

# Load all three coral condition datasets
# Rationale: We need extent, prevalence, and mortality data to get complete
# picture of coral condition responses across different metrics
extent_data <- read_csv("s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv")
prevalence_data <- read_csv("s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv")
mortality_data <- read_csv("s3pt1_coralcondition_mortalityprevalence_33sites_2022_2025.csv")

# Load temperature data for context
temp_data <- read_csv("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

# Display data dimensions and structure
cat("Dataset dimensions:\n")
cat(sprintf("  Extent data: %d rows × %d columns\n", nrow(extent_data), ncol(extent_data)))
cat(sprintf("  Prevalence data: %d rows × %d columns\n", nrow(prevalence_data), ncol(prevalence_data)))
cat(sprintf("  Mortality data: %d rows × %d columns\n", nrow(mortality_data), ncol(mortality_data)))
cat(sprintf("  Temperature data: %d rows × %d columns\n", nrow(temp_data), ncol(temp_data)))

# Examine data structure and key variables
cat("\nKey variables in extent data:\n")
cat(paste(names(extent_data), collapse = ", "), "\n")

cat("\nUnique time periods in data:\n")
unique_periods <- extent_data %>% 
  distinct(year, period) %>% 
  arrange(year, period)
print(unique_periods)

cat(sprintf("\nTotal unique sites in extent data: %d\n", n_distinct(extent_data$site)))
cat(sprintf("Year range: %d - %d\n", min(extent_data$year), max(extent_data$year)))

# ============================================================================
# DATA QUALITY ASSESSMENT
# ============================================================================

cat("\nSTEP 2: Data quality assessment\n")
cat("-------------------------------\n")

# Check for missing values in key variables
missing_analysis <- extent_data %>%
  summarise(
    missing_site = sum(is.na(site)),
    missing_year = sum(is.na(year)),
    missing_period = sum(is.na(period)),
    missing_anybleaching = sum(is.na(ext_anybleaching)),
    missing_bleached = sum(is.na(ext_bleached)),
    missing_nobleaching = sum(is.na(ext_nobleaching)),
    total_rows = n()
  )

cat("Missing data summary:\n")
print(missing_analysis)

# Check data completeness by site and time period
completeness_check <- extent_data %>%
  group_by(site, year, period) %>%
  summarise(
    n_replicates = n(),
    complete_cases = sum(!is.na(ext_anybleaching)),
    .groups = "drop"
  )

cat("\nReplication summary:\n")
replication_summary <- completeness_check %>%
  group_by(n_replicates) %>%
  summarise(n_site_time_combinations = n(), .groups = "drop")
print(replication_summary)

# ============================================================================
# VISUALIZATION 1: Data Availability Matrix
# ============================================================================

# Create data availability heatmap
# Justification: Visual assessment of data gaps is crucial for understanding
# which sites and timepoints have sufficient data for analysis

availability_matrix <- extent_data %>%
  group_by(site, year, period) %>%
  summarise(data_availability = !is.na(first(ext_anybleaching)), .groups = "drop") %>%
  mutate(timepoint = paste(year, period, sep = "_"))

p1 <- ggplot(availability_matrix, aes(x = timepoint, y = reorder(site, year), fill = data_availability)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen"), 
                    labels = c("Missing", "Available"),
                    name = "Data Status") +
  labs(
    title = "Data Availability Matrix Across Sites and Time Periods",
    subtitle = "Green = data available, Red = missing data",
    x = "Time Period (Year_Period)",
    y = "Monitoring Site",
    caption = "This heatmap reveals data gaps that could affect analysis completeness.\nComplete temporal coverage is essential for recovery trajectory analysis."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("01_plot_data_availability_matrix.png", p1, width = 12, height = 10, dpi = 300)
cat("\nSaved: 01_plot_data_availability_matrix.png\n")

# ============================================================================
# SITE-LEVEL DATA AGGREGATION
# ============================================================================

cat("\nSTEP 3: Calculating site-level means across replicates\n")
cat("------------------------------------------------------\n")

# Function to calculate site-level means
# Rationale: Multiple replicates per site need to be averaged to get 
# representative site-level values for analysis
calculate_site_means <- function(data, group_vars = c("site", "year", "period")) {
  # Calculate means across replicates, excluding NA values
  # This preserves data where some but not all replicates are missing
  result <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    # Remove rows where key variables are NaN (no valid replicates)
    filter(!is.nan(ext_anybleaching))
  
  return(result)
}

# Apply aggregation to all datasets
extent_means <- calculate_site_means(extent_data)
prevalence_means <- calculate_site_means(prevalence_data)  
mortality_means <- calculate_site_means(mortality_data)

cat(sprintf("Original extent data: %d rows\n", nrow(extent_data)))
cat(sprintf("Aggregated extent data: %d site-year-period combinations\n", nrow(extent_means)))
cat(sprintf("Data reduction: %.1f%% (due to replicate averaging)\n", 
            (1 - nrow(extent_means)/nrow(extent_data)) * 100))

# ============================================================================
# KEY TIMEPOINT IDENTIFICATION
# ============================================================================

cat("\nSTEP 4: Identifying key timepoints for 2023-2025 analysis\n")
cat("--------------------------------------------------------\n")

# Define the four critical timepoints for the analysis
# Rationale: These timepoints capture the bleaching events and recovery periods
# 2023 Annual: Baseline condition before major bleaching
# 2024 PBL: Post-bleaching survey after 2023 stress
# 2024 Annual: Condition before 2024 bleaching event  
# 2025 PBL: Post-bleaching survey after 2024 stress (our main response variable)

key_timepoints <- c("2023_Annual", "2024_PBL", "2024_Annual", "2025_PBL")

# Check data availability for each key timepoint
timepoint_availability <- extent_means %>%
  filter(
    (year == 2023 & period == "Annual") |
    (year == 2024 & period == "PBL") |
    (year == 2024 & period == "Annual") |
    (year == 2025 & period == "PBL")
  ) %>%
  mutate(timepoint = paste(year, period, sep = "_")) %>%
  group_by(timepoint) %>%
  summarise(
    n_sites = n(),
    mean_bleaching = mean(ext_anybleaching, na.rm = TRUE),
    median_bleaching = median(ext_anybleaching, na.rm = TRUE),
    min_bleaching = min(ext_anybleaching, na.rm = TRUE),
    max_bleaching = max(ext_anybleaching, na.rm = TRUE),
    .groups = "drop"
  )

cat("Data availability and basic statistics for key timepoints:\n")
print(timepoint_availability)

# Find sites with complete data across all four timepoints
complete_sites <- extent_means %>%
  filter(
    (year == 2023 & period == "Annual") |
    (year == 2024 & period == "PBL") |
    (year == 2024 & period == "Annual") |
    (year == 2025 & period == "PBL")
  ) %>%
  mutate(timepoint = paste(year, period, sep = "_")) %>%
  group_by(site) %>%
  summarise(
    timepoints_available = n(),
    available_timepoints = paste(sort(unique(timepoint)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(timepoints_available == 4)

cat(sprintf("\nSites with complete data across all 4 key timepoints: %d\n", nrow(complete_sites)))
cat("These sites will form our core analysis dataset.\n")

# ============================================================================
# VISUALIZATION 2: Bleaching Extent Distributions by Timepoint
# ============================================================================

# Create comprehensive distribution plots
# Justification: Understanding the distribution of bleaching values across
# timepoints reveals the severity and variability of bleaching events

timepoint_data <- extent_means %>%
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

# Box plots with individual points
p2 <- ggplot(timepoint_data, aes(x = timepoint_ordered, y = ext_anybleaching, fill = timepoint_ordered)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_viridis_d(name = "Time Period") +
  labs(
    title = "Distribution of Bleaching Extent Across Key Timepoints",
    subtitle = "Box plots show median, quartiles, and individual site values",
    x = "Time Period",
    y = "Bleaching Extent (%)",
    caption = "This plot reveals the severity of bleaching events and recovery patterns.\nHigh values in 2024 Annual indicate severe bleaching, while lower 2025 PBL\nvalues would indicate recovery. Outliers may represent particularly\nresilient or vulnerable sites deserving detailed investigation."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("01_plot_bleaching_distributions.png", p2, width = 10, height = 7, dpi = 300)
cat("Saved: 01_plot_bleaching_distributions.png\n")

# ============================================================================
# DATA-DRIVEN THRESHOLD CALCULATION
# ============================================================================

cat("\nSTEP 5: Calculating data-driven thresholds using actual quartiles\n")
cat("------------------------------------------------------------------\n")

# Calculate quartiles for bleaching extent across all timepoints
# Rationale: Using actual data quartiles provides objective, data-driven
# thresholds rather than arbitrary cutoffs. This ensures our classifications
# reflect the actual distribution of observed values.

all_bleaching_values <- timepoint_data$ext_anybleaching[!is.na(timepoint_data$ext_anybleaching)]

bleaching_quartiles <- quantile(all_bleaching_values, probs = c(0, 0.25, 0.5, 0.75, 1.0))
cat("Bleaching extent quartiles (all timepoints combined):\n")
print(bleaching_quartiles)

# Calculate quartiles for each timepoint separately
# Rationale: Different timepoints may have different baseline distributions
timepoint_quartiles <- timepoint_data %>%
  group_by(timepoint_ordered) %>%
  summarise(
    n = n(),
    Q0_min = min(ext_anybleaching, na.rm = TRUE),
    Q1_25th = quantile(ext_anybleaching, 0.25, na.rm = TRUE),
    Q2_median = median(ext_anybleaching, na.rm = TRUE),
    Q3_75th = quantile(ext_anybleaching, 0.75, na.rm = TRUE),
    Q4_max = max(ext_anybleaching, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nQuartiles by timepoint:\n")
print(timepoint_quartiles)

# Define bleaching severity categories based on overall quartiles
# Rationale: Four categories based on quartiles provide balanced groups
# while maintaining meaningful ecological interpretation
bleaching_severity_thresholds <- list(
  minimal = bleaching_quartiles[2],      # 0-25th percentile
  moderate = bleaching_quartiles[3],     # 25th-50th percentile  
  high = bleaching_quartiles[4],         # 50th-75th percentile
  severe = bleaching_quartiles[5]        # 75th-100th percentile
)

cat("\nData-driven bleaching severity categories:\n")
cat(sprintf("  Minimal: 0 - %.1f%%\n", bleaching_severity_thresholds$minimal))
cat(sprintf("  Moderate: %.1f - %.1f%%\n", bleaching_severity_thresholds$minimal, bleaching_severity_thresholds$moderate))
cat(sprintf("  High: %.1f - %.1f%%\n", bleaching_severity_thresholds$moderate, bleaching_severity_thresholds$high))
cat(sprintf("  Severe: %.1f - %.1f%%\n", bleaching_severity_thresholds$high, bleaching_severity_thresholds$severe))

# Apply severity categories to data
timepoint_data_categorized <- timepoint_data %>%
  mutate(
    severity_category = case_when(
      ext_anybleaching <= bleaching_severity_thresholds$minimal ~ "Minimal",
      ext_anybleaching <= bleaching_severity_thresholds$moderate ~ "Moderate", 
      ext_anybleaching <= bleaching_severity_thresholds$high ~ "High",
      ext_anybleaching <= bleaching_severity_thresholds$severe ~ "Severe",
      TRUE ~ "Severe"
    ),
    severity_category = factor(severity_category, levels = c("Minimal", "Moderate", "High", "Severe"))
  )

# ============================================================================
# VISUALIZATION 3: Severity Category Distribution
# ============================================================================

# Create stacked bar chart of severity categories by timepoint
# Justification: Shows how bleaching severity changes across timepoints
# and validates that our quartile-based categories create meaningful groups

severity_summary <- timepoint_data_categorized %>%
  group_by(timepoint_ordered, severity_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(timepoint_ordered) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  )

p3 <- ggplot(severity_summary, aes(x = timepoint_ordered, y = percentage, fill = severity_category)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = sprintf("n=%d", count)), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_viridis_d(name = "Severity\nCategory", option = "plasma") +
  labs(
    title = "Bleaching Severity Category Distribution Across Timepoints",
    subtitle = "Categories based on actual data quartiles, not arbitrary thresholds",
    x = "Time Period", 
    y = "Percentage of Sites (%)",
    caption = "This stacked bar chart reveals temporal patterns in bleaching severity.\nQuartile-based categories ensure each severity level represents 25% of\nobserved values, providing balanced and data-driven classifications.\nShifts between timepoints indicate recovery or deterioration patterns."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("01_plot_severity_distributions.png", p3, width = 10, height = 7, dpi = 300)
cat("Saved: 01_plot_severity_distributions.png\n")

# ============================================================================
# SITE-LEVEL VARIABILITY ANALYSIS
# ============================================================================

cat("\nSTEP 6: Analyzing site-level variability and consistency\n")
cat("--------------------------------------------------------\n")

# Calculate coefficient of variation for each site across timepoints
# Rationale: Some sites may be consistently high/low while others are highly variable
# This identifies sites with stable vs. dynamic bleaching responses

site_variability <- timepoint_data %>%
  group_by(site) %>%
  summarise(
    n_timepoints = n(),
    mean_bleaching = mean(ext_anybleaching, na.rm = TRUE),
    sd_bleaching = sd(ext_anybleaching, na.rm = TRUE),
    min_bleaching = min(ext_anybleaching, na.rm = TRUE),
    max_bleaching = max(ext_anybleaching, na.rm = TRUE),
    range_bleaching = max_bleaching - min_bleaching,
    cv_bleaching = sd_bleaching / mean_bleaching,
    .groups = "drop"
  ) %>%
  filter(n_timepoints >= 3) %>%  # Only sites with sufficient data
  arrange(desc(cv_bleaching))

cat("Sites with highest temporal variability (coefficient of variation):\n")
print(head(site_variability, 10))

cat("\nSites with lowest temporal variability (most consistent):\n") 
print(tail(site_variability, 10))

# Calculate variability quartiles for site classification
variability_quartiles <- quantile(site_variability$cv_bleaching, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("\nCoefficient of variation quartiles:\n")
print(variability_quartiles)

# ============================================================================
# VISUALIZATION 4: Site Variability Analysis
# ============================================================================

# Create scatter plot of mean vs. variability
# Justification: Identifies sites with different response patterns - 
# some may be consistently impacted, others highly variable

site_variability_plot <- site_variability %>%
  mutate(
    variability_category = case_when(
      cv_bleaching <= variability_quartiles[2] ~ "Low Variability",
      cv_bleaching <= variability_quartiles[3] ~ "Moderate Variability",
      cv_bleaching <= variability_quartiles[4] ~ "High Variability", 
      TRUE ~ "Extreme Variability"
    ),
    variability_category = factor(variability_category, 
                                  levels = c("Low Variability", "Moderate Variability", 
                                            "High Variability", "Extreme Variability"))
  )

p4 <- ggplot(site_variability_plot, aes(x = mean_bleaching, y = cv_bleaching, color = variability_category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_viridis_d(name = "Variability\nCategory") +
  labs(
    title = "Site-Level Bleaching Variability vs. Mean Impact",
    subtitle = "Coefficient of variation reveals sites with consistent vs. dynamic responses",
    x = "Mean Bleaching Extent Across Timepoints (%)",
    y = "Coefficient of Variation (SD/Mean)",
    caption = "This plot identifies sites with different temporal response patterns.\nLow variability sites may be consistently resilient or vulnerable.\nHigh variability sites show dynamic responses that could indicate\nrecovery capacity or susceptibility to environmental fluctuations.\nThe trend line reveals whether high-impact sites are more variable."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))

ggsave("01_plot_site_variability.png", p4, width = 10, height = 7, dpi = 300)
cat("Saved: 01_plot_site_variability.png\n")

# ============================================================================
# SAVE PROCESSED DATASETS
# ============================================================================

cat("\nSTEP 7: Saving processed datasets for downstream analysis\n")
cat("---------------------------------------------------------\n")

# Save all processed datasets with metadata
write_csv(extent_means, "01_extent_site_means.csv")
write_csv(prevalence_means, "01_prevalence_site_means.csv")
write_csv(mortality_means, "01_mortality_site_means.csv")

# Save threshold information for use in other scripts
thresholds_data <- data.frame(
  category = c("minimal", "moderate", "high", "severe"),
  upper_threshold = c(bleaching_severity_thresholds$minimal,
                     bleaching_severity_thresholds$moderate,
                     bleaching_severity_thresholds$high, 
                     bleaching_severity_thresholds$severe),
  description = c("0-25th percentile", "25th-50th percentile", 
                 "50th-75th percentile", "75th-100th percentile")
)
write_csv(thresholds_data, "01_severity_thresholds.csv")

# Save site variability analysis
write_csv(site_variability_plot, "01_site_variability_analysis.csv")

# Save complete sites list
write_csv(complete_sites, "01_complete_sites_list.csv")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\nFINAL SUMMARY STATISTICS\n")
cat("========================\n")

cat(sprintf("Total sites in dataset: %d\n", n_distinct(extent_means$site)))
cat(sprintf("Sites with complete 4-timepoint data: %d\n", nrow(complete_sites)))
cat(sprintf("Total site-year-period combinations: %d\n", nrow(extent_means)))

cat("\nBleaching severity thresholds (data-driven quartiles):\n")
for(i in 1:nrow(thresholds_data)) {
  cat(sprintf("  %s: ≤%.1f%% (%s)\n", 
              str_to_title(thresholds_data$category[i]),
              thresholds_data$upper_threshold[i],
              thresholds_data$description[i]))
}

cat("\nFiles saved:\n")
cat("  - 01_extent_site_means.csv (processed extent data)\n")
cat("  - 01_prevalence_site_means.csv (processed prevalence data)\n") 
cat("  - 01_mortality_site_means.csv (processed mortality data)\n")
cat("  - 01_severity_thresholds.csv (data-driven thresholds)\n")
cat("  - 01_site_variability_analysis.csv (site variability metrics)\n")
cat("  - 01_complete_sites_list.csv (sites with complete data)\n")

cat("\nVisualizations saved:\n")
cat("  - 01_plot_data_availability_matrix.png\n")
cat("  - 01_plot_bleaching_distributions.png\n") 
cat("  - 01_plot_severity_distributions.png\n")
cat("  - 01_plot_site_variability.png\n")

cat("\n============================================================================\n")
cat("STEP 1 COMPLETE: Data loaded, processed, and thresholds established\n")
cat("Next: Temperature and DHW analysis using these data-driven thresholds\n")
cat("============================================================================\n")