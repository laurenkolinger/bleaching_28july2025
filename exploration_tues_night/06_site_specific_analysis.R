# ============================================================================
# 06: Comprehensive Site-Specific Analysis and Characterization
# ============================================================================
# Purpose: Conduct detailed site-specific analysis with data-driven categorizations,
#          comprehensive resilience rankings, vulnerability assessments, and 
#          detailed characterizations of coral response patterns. Provide
#          site-specific insights using quartile-based classifications and
#          multi-dimensional performance metrics.
# Author: Coral Bleaching Analysis Pipeline
# Date: Analysis of 2023-2025 bleaching events
# Dependencies: Requires outputs from scripts 01, 02, 03, 04, and 05
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
library(stringr)      # String manipulation
library(cluster)      # Clustering analysis
library(factoextra)   # Enhanced clustering visualization
library(ggrepel)      # Text repelling for clean labels

cat("============================================================================\n")
cat("COMPREHENSIVE SITE-SPECIFIC ANALYSIS AND CHARACTERIZATION\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD ALL PROCESSED DATA FOR COMPREHENSIVE SITE ANALYSIS
# ============================================================================

cat("STEP 1: Loading comprehensive datasets for site-specific analysis\n")
cat("-----------------------------------------------------------------\n")

# Load all relevant datasets with error handling
# Rationale: Comprehensive site analysis requires integration of all available
# data sources to create complete site profiles and accurate characterizations

required_files <- c(
  "03_combined_response_metrics.csv",
  "03_site_response_patterns.csv"
)

optional_files <- c(
  "01_extent_site_means.csv",
  "02_comprehensive_thermal_data.csv",
  "04_predictive_dataset_complete.csv",
  "03_response_category_thresholds.csv",
  "02_thermal_stress_thresholds.csv"
)

loaded_data <- list()

# Load required files
for(file in required_files) {
  if(file.exists(file)) {
    dataset_name <- str_remove(str_remove(file, ".csv"), "^[0-9]+_")
    loaded_data[[dataset_name]] <- read_csv(file)
    cat(sprintf("✓ Loaded: %s (%d rows)\n", file, nrow(loaded_data[[dataset_name]])))
  } else {
    cat(sprintf("✗ Missing required file: %s\n", file))
  }
}

# Load optional files
for(file in optional_files) {
  if(file.exists(file)) {
    dataset_name <- str_remove(str_remove(file, ".csv"), "^[0-9]+_")
    loaded_data[[dataset_name]] <- read_csv(file)
    cat(sprintf("+ Optional: %s (%d rows)\n", file, nrow(loaded_data[[dataset_name]])))
  }
}

cat(sprintf("\nLoaded %d datasets for site-specific analysis\n", length(loaded_data)))

# Validate data availability
if(!"combined_response_metrics" %in% names(loaded_data)) {
  stop("Critical error: Response metrics data not available for site analysis")
}

# ============================================================================
# DATA-DRIVEN SITE CLASSIFICATION THRESHOLDS
# ============================================================================

cat("\nSTEP 2: Establishing data-driven site classification thresholds\n")
cat("--------------------------------------------------------------\n")

# Calculate comprehensive quartiles for site classification
# Rationale: Using actual data quartiles ensures that site classifications
# reflect observed patterns rather than arbitrary thresholds

response_data <- loaded_data$combined_response_metrics

# Recovery performance quartiles for both periods
period1_recovery <- response_data %>% filter(period == "2023_to_2024_PBL") %>% pull(recovery_achieved)
period2_recovery <- response_data %>% filter(period == "2024_to_2025_PBL") %>% pull(recovery_achieved)

overall_recovery_quartiles <- quantile(c(period1_recovery, period2_recovery), 
                                       probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("Overall recovery performance quartiles (both periods combined):\n")
print(overall_recovery_quartiles)

# Response magnitude quartiles (includes recovery and worsening)
all_responses <- response_data$response_magnitude
response_magnitude_quartiles <- quantile(all_responses, 
                                         probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("\nResponse magnitude quartiles (negative = recovery, positive = worsening):\n")
print(response_magnitude_quartiles)

# Initial bleaching impact quartiles
all_initial_bleaching <- response_data$initial_bleaching
initial_impact_quartiles <- quantile(all_initial_bleaching, 
                                     probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

cat("\nInitial bleaching impact quartiles:\n")
print(initial_impact_quartiles)

# ============================================================================
# COMPREHENSIVE SITE DATASET CONSTRUCTION
# ============================================================================

cat("\nSTEP 3: Constructing comprehensive site-specific dataset\n")
cat("-------------------------------------------------------\n")

# Create master site dataset by merging all available information
# Rationale: Comprehensive site profiles require integration of temporal,
# thermal, and response data to enable multi-dimensional characterization

# Start with site response patterns if available
if("site_response_patterns" %in% names(loaded_data)) {
  master_site_data <- loaded_data$site_response_patterns
  cat("Base dataset: Site response patterns\n")
} else {
  # Create base dataset from response data
  master_site_data <- response_data %>%
    group_by(site) %>%
    summarise(
      periods_available = n(),
      mean_recovery = mean(recovery_achieved, na.rm = TRUE),
      mean_initial_impact = mean(initial_bleaching, na.rm = TRUE),
      .groups = "drop"
    )
  cat("Base dataset: Aggregated response data\n")
}

# Add thermal data if available
if("comprehensive_thermal_data" %in% names(loaded_data)) {
  thermal_summary <- loaded_data$comprehensive_thermal_data %>%
    group_by(site) %>%
    summarise(
      mean_max_dhw = mean(max_dhw, na.rm = TRUE),
      max_max_dhw = max(max_dhw, na.rm = TRUE),
      mean_temp_variability = mean(temp_sd, na.rm = TRUE),
      total_thermal_weeks = sum(weeks_with_dhw, na.rm = TRUE),
      thermal_years_available = n(),
      .groups = "drop"
    )
  
  master_site_data <- master_site_data %>%
    left_join(thermal_summary, by = "site")
  cat("Added thermal stress data\n")
}

# Add temporal extent data if available
if("extent_site_means" %in% names(loaded_data)) {
  temporal_summary <- loaded_data$extent_site_means %>%
    group_by(site) %>%
    summarise(
      temporal_observations = n(),
      mean_bleaching_extent = mean(ext_anybleaching, na.rm = TRUE),
      max_bleaching_extent = max(ext_anybleaching, na.rm = TRUE),
      min_bleaching_extent = min(ext_anybleaching, na.rm = TRUE),
      bleaching_range = max_bleaching_extent - min_bleaching_extent,
      bleaching_variability = sd(ext_anybleaching, na.rm = TRUE),
      .groups = "drop"
    )
  
  master_site_data <- master_site_data %>%
    left_join(temporal_summary, by = "site")
  cat("Added temporal bleaching data\n")
}

cat(sprintf("Master site dataset created with %d sites and %d variables\n", 
            nrow(master_site_data), ncol(master_site_data)))

# ============================================================================
# DATA-DRIVEN SITE CATEGORIZATION AND SCORING
# ============================================================================

cat("\nSTEP 4: Implementing data-driven site categorization and scoring\n")
cat("----------------------------------------------------------------\n")

# Create comprehensive site classifications using observed quartiles
# Rationale: Multiple classification dimensions capture different aspects
# of site performance and resilience

comprehensive_site_analysis <- master_site_data %>%
  mutate(
    # Recovery performance categories (if response pattern data available)
    recovery_performance = if("recovery_achieved_p2" %in% names(.)) {
      case_when(
        recovery_achieved_p2 >= overall_recovery_quartiles[4] ~ "Exceptional Recovery",
        recovery_achieved_p2 >= overall_recovery_quartiles[3] ~ "Strong Recovery",
        recovery_achieved_p2 >= overall_recovery_quartiles[2] ~ "Moderate Recovery",
        recovery_achieved_p2 > 0 ~ "Minimal Recovery",
        TRUE ~ "No Recovery"
      )
    } else {
      case_when(
        mean_recovery >= overall_recovery_quartiles[4] ~ "Exceptional Recovery",
        mean_recovery >= overall_recovery_quartiles[3] ~ "Strong Recovery", 
        mean_recovery >= overall_recovery_quartiles[2] ~ "Moderate Recovery",
        mean_recovery > 0 ~ "Minimal Recovery",
        TRUE ~ "No Recovery"
      )
    },
    
    # Consistency classification (if response pattern data available)
    response_consistency = if("recovery_achieved_p1" %in% names(.) && "recovery_achieved_p2" %in% names(.)) {
      consistency_diff <- abs(recovery_achieved_p2 - recovery_achieved_p1)
      case_when(
        consistency_diff <= 5 ~ "Highly Consistent",
        consistency_diff <= 15 ~ "Moderately Consistent",
        recovery_achieved_p2 > recovery_achieved_p1 + 15 ~ "Improving Performance",
        recovery_achieved_p1 > recovery_achieved_p2 + 15 ~ "Declining Performance",
        TRUE ~ "Variable Performance"
      )
    } else {
      "Unknown"
    },
    
    # Thermal stress exposure (if thermal data available)
    thermal_stress_level = if("mean_max_dhw" %in% names(.)) {
      thermal_quartiles <- quantile(mean_max_dhw, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
      case_when(
        mean_max_dhw >= thermal_quartiles[4] ~ "High Thermal Stress",
        mean_max_dhw >= thermal_quartiles[3] ~ "Moderate Thermal Stress",
        mean_max_dhw >= thermal_quartiles[2] ~ "Low Thermal Stress",
        TRUE ~ "Minimal Thermal Stress"
      )
    } else {
      "Unknown"
    },
    
    # Impact severity (if impact data available)
    impact_severity = if("mean_initial_impact" %in% names(.)) {
      case_when(
        mean_initial_impact >= initial_impact_quartiles[4] ~ "Severe Impact",
        mean_initial_impact >= initial_impact_quartiles[3] ~ "High Impact",
        mean_initial_impact >= initial_impact_quartiles[2] ~ "Moderate Impact",
        TRUE ~ "Low Impact"
      )
    } else {
      "Unknown"
    },
    
    # Variability classification (if temporal data available)
    temporal_variability = if("bleaching_variability" %in% names(.)) {
      var_quartiles <- quantile(bleaching_variability, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
      case_when(
        bleaching_variability >= var_quartiles[4] ~ "High Variability",
        bleaching_variability >= var_quartiles[3] ~ "Moderate Variability",
        bleaching_variability >= var_quartiles[2] ~ "Low Variability",
        TRUE ~ "Minimal Variability"
      )
    } else {
      "Unknown"
    }
  )

# Create ordered factors for consistent plotting
factor_levels <- list(
  recovery_performance = c("No Recovery", "Minimal Recovery", "Moderate Recovery", 
                           "Strong Recovery", "Exceptional Recovery"),
  response_consistency = c("Declining Performance", "Variable Performance", "Moderately Consistent",
                           "Highly Consistent", "Improving Performance"),
  thermal_stress_level = c("Minimal Thermal Stress", "Low Thermal Stress", 
                           "Moderate Thermal Stress", "High Thermal Stress"),
  impact_severity = c("Low Impact", "Moderate Impact", "High Impact", "Severe Impact"),
  temporal_variability = c("Minimal Variability", "Low Variability", 
                           "Moderate Variability", "High Variability")
)

for(col in names(factor_levels)) {
  if(col %in% names(comprehensive_site_analysis)) {
    comprehensive_site_analysis[[col]] <- factor(comprehensive_site_analysis[[col]], 
                                                 levels = factor_levels[[col]])
  }
}

# ============================================================================
# COMPREHENSIVE RESILIENCE SCORING SYSTEM
# ============================================================================

cat("\nSTEP 5: Developing comprehensive resilience scoring system\n")
cat("---------------------------------------------------------\n")

# Create multi-dimensional resilience score using data-driven metrics
# Rationale: Single metrics may miss important resilience aspects;
# comprehensive scoring captures multiple dimensions of coral performance

resilience_scoring <- comprehensive_site_analysis %>%
  mutate(
    # Recovery score (0-25 points)
    recovery_score = if("recovery_achieved_p2" %in% names(.)) {
      pmin(25, pmax(0, recovery_achieved_p2))
    } else {
      pmin(25, pmax(0, mean_recovery))
    },
    
    # Consistency score (0-25 points)
    consistency_score = if("recovery_achieved_p1" %in% names(.) && "recovery_achieved_p2" %in% names(.)) {
      consistency_diff <- abs(recovery_achieved_p2 - recovery_achieved_p1)
      pmax(0, 25 - consistency_diff)
    } else {
      12.5  # Neutral score when data unavailable
    },
    
    # Thermal stress resistance score (0-25 points, inverse of stress)
    thermal_resistance_score = if("mean_max_dhw" %in% names(.)) {
      max_dhw_observed <- max(mean_max_dhw, na.rm = TRUE)
      25 * (1 - (mean_max_dhw / max_dhw_observed))
    } else {
      12.5  # Neutral score when data unavailable
    },
    
    # Impact resilience score (0-25 points, recovery relative to impact)
    impact_resilience_score = if("recovery_achieved_p2" %in% names(.) && "baseline_2024_annual" %in% names(.)) {
      ifelse(baseline_2024_annual > 0, 
             pmin(25, 25 * (recovery_achieved_p2 / baseline_2024_annual)),
             12.5)
    } else if("mean_recovery" %in% names(.) && "mean_initial_impact" %in% names(.)) {
      ifelse(mean_initial_impact > 0,
             pmin(25, 25 * (mean_recovery / mean_initial_impact)),
             12.5)
    } else {
      12.5  # Neutral score when data unavailable
    },
    
    # Calculate overall resilience score (0-100)
    overall_resilience_score = recovery_score + consistency_score + 
      thermal_resistance_score + impact_resilience_score
  ) %>%
  # Create resilience categories based on overall score quartiles
  mutate(
    resilience_quartile = ntile(overall_resilience_score, 4),
    resilience_category = case_when(
      resilience_quartile == 4 ~ "High Resilience",
      resilience_quartile == 3 ~ "Moderate Resilience", 
      resilience_quartile == 2 ~ "Low Resilience",
      resilience_quartile == 1 ~ "Very Low Resilience",
      TRUE ~ "Unclassified"
    ),
    resilience_category = factor(resilience_category,
                                 levels = c("Very Low Resilience", "Low Resilience", 
                                            "Moderate Resilience", "High Resilience"))
  )

cat("Resilience scoring system implemented with 4 components:\n")
cat("  1. Recovery Score (0-25): Absolute recovery magnitude\n")
cat("  2. Consistency Score (0-25): Temporal consistency of responses\n")
cat("  3. Thermal Resistance Score (0-25): Resistance to thermal stress\n")
cat("  4. Impact Resilience Score (0-25): Recovery relative to initial impact\n")
cat("  Total Resilience Score (0-100): Sum of all components\n")

# ============================================================================
# VULNERABILITY ASSESSMENT
# ============================================================================

cat("\nSTEP 6: Conducting comprehensive vulnerability assessment\n")
cat("--------------------------------------------------------\n")

# Create vulnerability scoring system complementary to resilience
# Rationale: Vulnerability assessment identifies sites at highest risk
# and provides complementary perspective to resilience scoring

vulnerability_assessment <- resilience_scoring %>%
  mutate(
    # Exposure vulnerability (thermal stress experienced)
    exposure_vulnerability = if("mean_max_dhw" %in% names(.)) {
      max_dhw_observed <- max(mean_max_dhw, na.rm = TRUE)
      100 * (mean_max_dhw / max_dhw_observed)
    } else {
      50  # Neutral score when data unavailable
    },
    
    # Sensitivity vulnerability (impact relative to stress)
    sensitivity_vulnerability = if("mean_initial_impact" %in% names(.) && "mean_max_dhw" %in% names(.)) {
      ifelse(mean_max_dhw > 0,
             pmin(100, 100 * (mean_initial_impact / mean_max_dhw)),
             50)
    } else {
      50  # Neutral score when data unavailable
    },
    
    # Adaptive capacity vulnerability (inverse of recovery ability)
    adaptive_capacity_vulnerability = if("recovery_achieved_p2" %in% names(.)) {
      max_recovery_observed <- max(recovery_achieved_p2, na.rm = TRUE)
      ifelse(max_recovery_observed > 0,
             100 * (1 - (recovery_achieved_p2 / max_recovery_observed)),
             50)
    } else if("mean_recovery" %in% names(.)) {
      max_recovery_observed <- max(mean_recovery, na.rm = TRUE)
      ifelse(max_recovery_observed > 0,
             100 * (1 - (mean_recovery / max_recovery_observed)),
             50)
    } else {
      50  # Neutral score when data unavailable
    },
    
    # Calculate overall vulnerability index (0-100)
    overall_vulnerability_index = (exposure_vulnerability + sensitivity_vulnerability + 
                                     adaptive_capacity_vulnerability) / 3
  ) %>%
  # Create vulnerability categories
  mutate(
    vulnerability_quartile = ntile(overall_vulnerability_index, 4),
    vulnerability_category = case_when(
      vulnerability_quartile == 4 ~ "Very High Vulnerability",
      vulnerability_quartile == 3 ~ "High Vulnerability",
      vulnerability_quartile == 2 ~ "Moderate Vulnerability", 
      vulnerability_quartile == 1 ~ "Low Vulnerability",
      TRUE ~ "Unclassified"
    ),
    vulnerability_category = factor(vulnerability_category,
                                    levels = c("Low Vulnerability", "Moderate Vulnerability",
                                               "High Vulnerability", "Very High Vulnerability"))
  )

cat("Vulnerability assessment implemented with 3 components:\n")
cat("  1. Exposure Vulnerability (0-100): Thermal stress exposure level\n")
cat("  2. Sensitivity Vulnerability (0-100): Impact sensitivity to stress\n")
cat("  3. Adaptive Capacity Vulnerability (0-100): Limited recovery ability\n")
cat("  Overall Vulnerability Index (0-100): Mean of all components\n")

# ============================================================================
# SITE CLUSTERING ANALYSIS
# ============================================================================

cat("\nSTEP 7: Performing site clustering analysis for pattern identification\n")
cat("---------------------------------------------------------------------\n")

# Perform clustering to identify natural site groupings
# Rationale: Clustering reveals natural site groupings that may not be
# apparent from individual metrics, providing insights into response strategies

# Prepare clustering data with available numeric variables
clustering_vars <- vulnerability_assessment %>%
  select_if(is.numeric) %>%
  select(-contains("quartile")) %>%  # Remove quartile rankings
  select_if(~ !all(is.na(.)))  # Remove variables with all NA

if(ncol(clustering_vars) >= 3 && nrow(clustering_vars) >= 6) {
  # Scale variables for clustering
  clustering_data_scaled <- scale(clustering_vars)
  
  # Determine optimal number of clusters using silhouette method
  if(nrow(clustering_data_scaled) >= 6) {
    silhouette_scores <- c()
    k_values <- 2:min(6, nrow(clustering_data_scaled)-1)
    
    for(k in k_values) {
      tryCatch({
        km_result <- kmeans(clustering_data_scaled, centers = k, nstart = 25)
        sil_score <- silhouette(km_result$cluster, dist(clustering_data_scaled))
        silhouette_scores <- c(silhouette_scores, mean(sil_score[, 3]))
      }, error = function(e) {
        silhouette_scores <- c(silhouette_scores, NA)
      })
    }
    
    if(any(!is.na(silhouette_scores))) {
      optimal_k <- k_values[which.max(silhouette_scores)]
      cat(sprintf("Optimal number of clusters: %d (silhouette score: %.3f)\n", 
                  optimal_k, max(silhouette_scores, na.rm = TRUE)))
      
      # Perform final clustering
      final_clustering <- kmeans(clustering_data_scaled, centers = optimal_k, nstart = 25)
      
      # Add cluster assignments to main dataset
      vulnerability_assessment$site_cluster <- paste0("Cluster_", final_clustering$cluster)
      
      # Characterize clusters
      cluster_characteristics <- vulnerability_assessment %>%
        group_by(site_cluster) %>%
        summarise(
          n_sites = n(),
          mean_resilience = mean(overall_resilience_score, na.rm = TRUE),
          mean_vulnerability = mean(overall_vulnerability_index, na.rm = TRUE),
          dominant_recovery_performance = names(sort(table(recovery_performance), decreasing = TRUE))[1],
          dominant_thermal_stress = names(sort(table(thermal_stress_level), decreasing = TRUE))[1],
          .groups = "drop"
        ) %>%
        arrange(desc(mean_resilience))
      
      cat("\nCluster characteristics:\n")
      print(cluster_characteristics)
      
    } else {
      cat("Warning: Clustering failed - insufficient data quality\n")
      vulnerability_assessment$site_cluster <- "Unclassified"
    }
  } else {
    cat("Warning: Insufficient sites for clustering analysis\n")
    vulnerability_assessment$site_cluster <- "Unclassified"
  }
} else {
  cat("Warning: Insufficient variables for clustering analysis\n")
  vulnerability_assessment$site_cluster <- "Unclassified"
}

# ============================================================================
# COMPREHENSIVE SITE RANKINGS
# ============================================================================

cat("\nSTEP 8: Creating comprehensive site rankings and characterizations\n")
cat("------------------------------------------------------------------\n")

# Create detailed site rankings across multiple dimensions
# Rationale: Multiple ranking perspectives reveal different aspects of
# site performance and provide comprehensive site characterizations

site_rankings <- vulnerability_assessment %>%
  arrange(desc(overall_resilience_score)) %>%
  mutate(
    resilience_rank = row_number(),
    resilience_percentile = round(100 * (nrow(.) - resilience_rank + 1) / nrow(.))
  ) %>%
  arrange(overall_vulnerability_index) %>%
  mutate(
    vulnerability_rank = row_number(),
    vulnerability_percentile = round(100 * (nrow(.) - vulnerability_rank + 1) / nrow(.))
  ) %>%
  # Create combined performance index
  mutate(
    combined_performance_index = (resilience_percentile + vulnerability_percentile) / 2
  ) %>%
  arrange(desc(combined_performance_index)) %>%
  mutate(
    overall_rank = row_number(),
    performance_tier = case_when(
      combined_performance_index >= 75 ~ "Tier 1: Exceptional Performance",
      combined_performance_index >= 50 ~ "Tier 2: Good Performance",
      combined_performance_index >= 25 ~ "Tier 3: Moderate Performance",
      TRUE ~ "Tier 4: Poor Performance"
    )
  )

# Identify top and bottom performers
top_performers <- site_rankings %>%
  filter(performance_tier == "Tier 1: Exceptional Performance") %>%
  arrange(desc(combined_performance_index))

bottom_performers <- site_rankings %>%
  filter(performance_tier == "Tier 4: Poor Performance") %>%
  arrange(combined_performance_index)

cat(sprintf("Site ranking completed for %d sites:\n", nrow(site_rankings)))
cat(sprintf("  Tier 1 (Exceptional): %d sites\n", sum(site_rankings$performance_tier == "Tier 1: Exceptional Performance")))
cat(sprintf("  Tier 2 (Good): %d sites\n", sum(site_rankings$performance_tier == "Tier 2: Good Performance")))
cat(sprintf("  Tier 3 (Moderate): %d sites\n", sum(site_rankings$performance_tier == "Tier 3: Moderate Performance")))
cat(sprintf("  Tier 4 (Poor): %d sites\n", sum(site_rankings$performance_tier == "Tier 4: Poor Performance")))

# ============================================================================
# DETAILED SITE CHARACTERIZATIONS
# ============================================================================

cat("\nSTEP 9: Generating detailed site characterizations\n")
cat("--------------------------------------------------\n")

# Create comprehensive site profiles for detailed analysis
# Rationale: Detailed characterizations provide actionable insights
# for site-specific understanding and management considerations

generate_site_profile <- function(site_data) {
  # Function to generate detailed text profiles for individual sites
  site_name <- site_data$site
  
  profile_text <- paste0(
    "SITE: ", site_name, "\n",
    "Performance Tier: ", site_data$performance_tier, "\n",
    "Overall Rank: ", site_data$overall_rank, " of ", nrow(site_rankings), "\n",
    "Resilience Score: ", round(site_data$overall_resilience_score, 1), "/100 (Rank #", site_data$resilience_rank, ")\n",
    "Vulnerability Index: ", round(site_data$overall_vulnerability_index, 1), "/100 (Rank #", site_data$vulnerability_rank, ")\n",
    "Recovery Performance: ", site_data$recovery_performance, "\n",
    "Response Consistency: ", site_data$response_consistency, "\n",
    "Thermal Stress Level: ", site_data$thermal_stress_level, "\n",
    "Impact Severity: ", site_data$impact_severity, "\n",
    "Site Cluster: ", site_data$site_cluster, "\n"
  )
  
  return(profile_text)
}

# Generate profiles for top and bottom performers
if(nrow(top_performers) > 0) {
  cat("\nTOP PERFORMING SITES:\n")
  cat("====================\n")
  for(i in 1:min(5, nrow(top_performers))) {
    cat(generate_site_profile(top_performers[i, ]))
    cat("\n")
  }
}

if(nrow(bottom_performers) > 0) {
  cat("\nPOOREST PERFORMING SITES:\n")
  cat("========================\n")
  for(i in 1:min(5, nrow(bottom_performers))) {
    cat(generate_site_profile(bottom_performers[i, ]))
    cat("\n")
  }
}

# ============================================================================
# STATISTICAL SUMMARIES BY CATEGORIES
# ============================================================================

cat("\nSTEP 10: Generating statistical summaries by site categories\n")
cat("-----------------------------------------------------------\n")

# Create comprehensive statistical summaries for different site categories
# Rationale: Category-based summaries reveal patterns and relationships
# between site characteristics and performance outcomes

# Recovery performance category analysis
if("recovery_performance" %in% names(site_rankings)) {
  recovery_category_summary <- site_rankings %>%
    group_by(recovery_performance) %>%
    summarise(
      n_sites = n(),
      mean_resilience_score = round(mean(overall_resilience_score, na.rm = TRUE), 1),
      mean_vulnerability_index = round(mean(overall_vulnerability_index, na.rm = TRUE), 1),
      mean_combined_performance = round(mean(combined_performance_index, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_combined_performance))
  
  cat("Recovery Performance Category Summary:\n")
  print(recovery_category_summary)
}

# Thermal stress level category analysis
if("thermal_stress_level" %in% names(site_rankings)) {
  thermal_category_summary <- site_rankings %>%
    group_by(thermal_stress_level) %>%
    summarise(
      n_sites = n(),
      mean_resilience_score = round(mean(overall_resilience_score, na.rm = TRUE), 1),
      mean_vulnerability_index = round(mean(overall_vulnerability_index, na.rm = TRUE), 1),
      mean_combined_performance = round(mean(combined_performance_index, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_combined_performance))
  
  cat("\nThermal Stress Level Category Summary:\n")
  print(thermal_category_summary)
}

# Performance tier analysis
tier_summary <- site_rankings %>%
  group_by(performance_tier) %>%
  summarise(
    n_sites = n(),
    mean_resilience_score = round(mean(overall_resilience_score, na.rm = TRUE), 1),
    mean_vulnerability_index = round(mean(overall_vulnerability_index, na.rm = TRUE), 1),
    resilience_range = paste0(round(min(overall_resilience_score, na.rm = TRUE), 1), 
                              " - ", round(max(overall_resilience_score, na.rm = TRUE), 1)),
    vulnerability_range = paste0(round(min(overall_vulnerability_index, na.rm = TRUE), 1),
                                 " - ", round(max(overall_vulnerability_index, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_resilience_score))

cat("\nPerformance Tier Summary:\n")
print(tier_summary)

# ============================================================================
# SAVE COMPREHENSIVE SITE ANALYSIS RESULTS
# ============================================================================

cat("\nSTEP 11: Saving comprehensive site analysis results\n")
cat("---------------------------------------------------\n")

# Save all site analysis datasets
write_csv(comprehensive_site_analysis, "06_comprehensive_site_analysis.csv")
write_csv(resilience_scoring, "06_site_resilience_scoring.csv")
write_csv(vulnerability_assessment, "06_site_vulnerability_assessment.csv")
write_csv(site_rankings, "06_site_rankings_complete.csv")

if(exists("recovery_category_summary")) {
  write_csv(recovery_category_summary, "06_recovery_category_summary.csv")
}

if(exists("thermal_category_summary")) {
  write_csv(thermal_category_summary, "06_thermal_category_summary.csv")
}

write_csv(tier_summary, "06_performance_tier_summary.csv")

if(exists("cluster_characteristics")) {
  write_csv(cluster_characteristics, "06_cluster_characteristics.csv")
}

# Save threshold documentation
site_thresholds_data <- data.frame(
  metric = c("recovery_minimal", "recovery_moderate", "recovery_strong", "recovery_exceptional",
             "impact_low", "impact_moderate", "impact_high", "impact_severe"),
  threshold = c(overall_recovery_quartiles[2], overall_recovery_quartiles[3], 
                overall_recovery_quartiles[4], overall_recovery_quartiles[5],
                initial_impact_quartiles[2], initial_impact_quartiles[3],
                initial_impact_quartiles[4], initial_impact_quartiles[5]),
  percentile = rep(c("25th", "50th", "75th", "100th"), 2),
  category = rep(c("Recovery_Performance", "Impact_Severity"), each = 4)
)

write_csv(site_thresholds_data, "06_site_classification_thresholds.csv")

# Create site profile text file
site_profiles_text <- c()
if(nrow(top_performers) > 0) {
  site_profiles_text <- c(site_profiles_text, "TOP PERFORMING SITES:", "=" %>% rep(20) %>% paste(collapse=""))
  for(i in 1:min(10, nrow(top_performers))) {
    site_profiles_text <- c(site_profiles_text, generate_site_profile(top_performers[i, ]), "")
  }
}

if(nrow(bottom_performers) > 0) {
  site_profiles_text <- c(site_profiles_text, "POOREST PERFORMING SITES:", "=" %>% rep(24) %>% paste(collapse=""))
  for(i in 1:min(10, nrow(bottom_performers))) {
    site_profiles_text <- c(site_profiles_text, generate_site_profile(bottom_performers[i, ]), "")
  }
}

writeLines(site_profiles_text, "06_detailed_site_profiles.txt")

# ============================================================================
# FINAL SUMMARY STATISTICS
# ============================================================================

cat("\nFINAL SITE-SPECIFIC ANALYSIS SUMMARY\n")
cat("====================================\n")

cat(sprintf("Total sites analyzed: %d\n", nrow(site_rankings)))
cat(sprintf("Variables per site: %d\n", ncol(site_rankings)))

cat("\nResilience Score Statistics:\n")
cat(sprintf("  Mean: %.1f\n", mean(site_rankings$overall_resilience_score, na.rm = TRUE)))
cat(sprintf("  Range: %.1f - %.1f\n", 
            min(site_rankings$overall_resilience_score, na.rm = TRUE),
            max(site_rankings$overall_resilience_score, na.rm = TRUE)))
cat(sprintf("  Standard Deviation: %.1f\n", sd(site_rankings$overall_resilience_score, na.rm = TRUE)))

cat("\nVulnerability Index Statistics:\n")
cat(sprintf("  Mean: %.1f\n", mean(site_rankings$overall_vulnerability_index, na.rm = TRUE)))
cat(sprintf("  Range: %.1f - %.1f\n", 
            min(site_rankings$overall_vulnerability_index, na.rm = TRUE),
            max(site_rankings$overall_vulnerability_index, na.rm = TRUE)))
cat(sprintf("  Standard Deviation: %.1f\n", sd(site_rankings$overall_vulnerability_index, na.rm = TRUE)))

cat("\nKey Insights:\n")
if(exists("top_performers") && nrow(top_performers) > 0) {
  cat(sprintf("  Best performing site: %s (Combined Index: %.1f)\n", 
              top_performers$site[1], top_performers$combined_performance_index[1]))
}
if(exists("bottom_performers") && nrow(bottom_performers) > 0) {
  cat(sprintf("  Poorest performing site: %s (Combined Index: %.1f)\n", 
              bottom_performers$site[1], bottom_performers$combined_performance_index[1]))
}

correlation_resilience_vulnerability <- cor(site_rankings$overall_resilience_score, 
                                            site_rankings$overall_vulnerability_index, 
                                            use = "complete.obs")
cat(sprintf("  Resilience-Vulnerability correlation: %.3f\n", correlation_resilience_vulnerability))

cat("\nFiles saved:\n")
cat("  - 06_comprehensive_site_analysis.csv (complete site dataset)\n")
cat("  - 06_site_resilience_scoring.csv (resilience scores and categories)\n")
cat("  - 06_site_vulnerability_assessment.csv (vulnerability assessment)\n")
cat("  - 06_site_rankings_complete.csv (complete rankings and tiers)\n")
cat("  - 06_recovery_category_summary.csv (recovery category analysis)\n")
cat("  - 06_thermal_category_summary.csv (thermal category analysis)\n")
cat("  - 06_performance_tier_summary.csv (performance tier analysis)\n")
if(exists("cluster_characteristics")) {
  cat("  - 06_cluster_characteristics.csv (site clustering results)\n")
}
cat("  - 06_site_classification_thresholds.csv (threshold documentation)\n")
cat("  - 06_detailed_site_profiles.txt (detailed site characterizations)\n")

cat("\n============================================================================\n")
cat("STEP 6 COMPLETE: Comprehensive site-specific analysis with data-driven classifications\n")
cat("Next: Master analysis orchestration and final reporting\n")
cat("============================================================================\n")