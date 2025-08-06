# ============================================================================
# 05: Comprehensive Visualization Analysis - Advanced Graphical Synthesis
# ============================================================================
# Purpose: Create advanced, publication-quality visualizations that synthesize
#          findings from all previous analyses. Generate comprehensive visual
#          narratives using data-driven categorizations and multiple integrated
#          perspectives on coral bleaching responses and recovery patterns.
# Author: Coral Bleaching Analysis Pipeline
# Date: Analysis of 2023-2025 bleaching events
# Dependencies: Requires outputs from scripts 01, 02, 03, and 04
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
library(patchwork)    # Advanced plot composition
library(ggridges)     # Ridge plots for distributions
library(ggforce)      # Extended ggplot functionality
library(stringr)      # String manipulation
library(RColorBrewer) # Color palettes
library(cowplot)      # Plot themes and arrangements

cat("============================================================================\n")
cat("COMPREHENSIVE VISUALIZATION ANALYSIS - ADVANCED GRAPHICAL SYNTHESIS\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD ALL PROCESSED DATA FROM PREVIOUS ANALYSES
# ============================================================================

cat("STEP 1: Loading comprehensive datasets from all previous analyses\n")
cat("-----------------------------------------------------------------\n")

# Load core datasets with error handling
# Rationale: Comprehensive visualization requires integration of all analysis
# components to create meaningful visual narratives

required_files <- c(
  "03_combined_response_metrics.csv",
  "03_site_response_patterns.csv", 
  "04_predictive_dataset_complete.csv",
  "04_model_performance_comparison.csv"
)

optional_files <- c(
  "01_extent_site_means.csv",
  "02_comprehensive_thermal_data.csv",
  "03_response_category_thresholds.csv",
  "04_correlation_analysis_detailed.csv"
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

cat(sprintf("\nLoaded %d datasets for comprehensive visualization\n", length(loaded_data)))

# ============================================================================
# DATA-DRIVEN VISUALIZATION PARAMETERS
# ============================================================================

cat("\nSTEP 2: Establishing data-driven visualization parameters and themes\n")
cat("-------------------------------------------------------------------\n")

# Calculate comprehensive quartiles for consistent visualization scales
# Rationale: Consistent, data-driven scales across all visualizations ensure
# accurate comparisons and meaningful interpretations

if("combined_response_metrics" %in% names(loaded_data)) {
  response_data <- loaded_data$combined_response_metrics
  
  # Recovery quartiles for consistent color mapping
  recovery_quartiles <- quantile(response_data$recovery_achieved, 
                                probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
  
  # Response magnitude quartiles for diverging scales
  response_quartiles <- quantile(response_data$response_magnitude, 
                               probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
  
  cat("Recovery achievement quartiles for visualization scales:\n")
  print(recovery_quartiles)
  cat("\nResponse magnitude quartiles for visualization scales:\n")  
  print(response_quartiles)
} else {
  cat("Warning: Response data not available for quartile calculation\n")
  recovery_quartiles <- c(0, 5, 15, 30, 80)
  response_quartiles <- c(-50, -15, 0, 15, 50)
}

# Define consistent visualization theme
# Rationale: Professional, publication-ready theme ensures visual consistency
# and readability across all plots
theme_coral_analysis <- theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray30", margin = margin(t = 10)),
    
    # Axis elements
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    
    # Legend elements
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    legend.box.spacing = unit(0.5, "cm"),
    
    # Panel elements
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),
    
    # Strip text for facets
    strip.text = element_text(size = 11, face = "bold", margin = margin(5, 5, 5, 5))
  )

# Define consistent color palettes based on data characteristics
# Rationale: Meaningful color mapping enhances interpretation and accessibility
recovery_palette <- scale_fill_viridis_d(name = "Recovery\nCategory", option = "viridis")
stress_palette <- scale_fill_viridis_d(name = "Stress\nCategory", option = "plasma")
diverging_palette <- scale_fill_gradient2(name = "Response", low = "blue", mid = "white", high = "red", midpoint = 0)

# ============================================================================
# ADVANCED TEMPORAL TRAJECTORY VISUALIZATION
# ============================================================================

cat("\nSTEP 3: Creating advanced temporal trajectory visualizations\n")
cat("-----------------------------------------------------------\n")

if("extent_site_means" %in% names(loaded_data)) {
  # Create comprehensive temporal analysis
  # Justification: Multi-panel temporal visualization reveals different aspects
  # of coral responses across time, from individual trajectories to population-level patterns
  
  temporal_data <- loaded_data$extent_site_means %>%
    filter(year %in% c(2023, 2024, 2025)) %>%
    mutate(
      timepoint_numeric = case_when(
        year == 2023 & period == "Annual" ~ 1,
        year == 2024 & period == "PBL" ~ 2,
        year == 2024 & period == "Annual" ~ 3,
        year == 2025 & period == "PBL" ~ 4
      ),
      timepoint_label = case_when(
        timepoint_numeric == 1 ~ "2023\nAnnual",
        timepoint_numeric == 2 ~ "2024\nPBL",
        timepoint_numeric == 3 ~ "2024\nAnnual",
        timepoint_numeric == 4 ~ "2025\nPBL"
      )
    ) %>%
    filter(!is.na(timepoint_numeric))
  
  # Calculate trajectory characteristics for each site
  trajectory_characteristics <- temporal_data %>%
    group_by(site) %>%
    filter(n() >= 3) %>%  # Ensure sufficient data points
    summarise(
      n_timepoints = n(),
      trajectory_mean = mean(ext_anybleaching, na.rm = TRUE),
      trajectory_range = max(ext_anybleaching, na.rm = TRUE) - min(ext_anybleaching, na.rm = TRUE),
      trajectory_slope = if(n_timepoints >= 2) {
        lm(ext_anybleaching ~ timepoint_numeric)$coefficients[2]
      } else { NA },
      max_bleaching = max(ext_anybleaching, na.rm = TRUE),
      final_bleaching = last(ext_anybleaching, order_by = timepoint_numeric),
      .groups = "drop"
    ) %>%
    mutate(
      trajectory_pattern = case_when(
        trajectory_slope < -5 ~ "Strong Recovery Trend",
        trajectory_slope < -1 ~ "Moderate Recovery Trend", 
        abs(trajectory_slope) <= 1 ~ "Stable",
        trajectory_slope > 1 ~ "Deteriorating Trend",
        TRUE ~ "Variable"
      ),
      trajectory_pattern = factor(trajectory_pattern, 
                                 levels = c("Strong Recovery Trend", "Moderate Recovery Trend",
                                           "Stable", "Deteriorating Trend", "Variable"))
    )
  
  # Merge trajectory characteristics with temporal data
  temporal_enhanced <- temporal_data %>%
    left_join(trajectory_characteristics, by = "site")
  
  # Panel A: Individual trajectories colored by pattern
  p1a <- ggplot(temporal_enhanced, aes(x = timepoint_numeric, y = ext_anybleaching)) +
    geom_line(aes(group = site, color = trajectory_pattern), alpha = 0.6, size = 0.8) +
    geom_point(aes(color = trajectory_pattern), alpha = 0.7, size = 2) +
    scale_color_viridis_d(name = "Trajectory\nPattern") +
    scale_x_continuous(breaks = 1:4, labels = c("2023\nAnnual", "2024\nPBL", "2024\nAnnual", "2025\nPBL")) +
    labs(
      title = "Individual Site Trajectory Patterns",
      subtitle = "Each line represents one site colored by overall trajectory pattern",
      x = "Time Period",
      y = "Bleaching Extent (%)"
    ) +
    theme_coral_analysis
  
  # Panel B: Population-level statistics with confidence bands
  p1b <- ggplot(temporal_enhanced, aes(x = timepoint_numeric, y = ext_anybleaching)) +
    stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.3, fill = "steelblue") +
    stat_summary(fun = mean, geom = "line", color = "darkblue", size = 2) +
    stat_summary(fun = mean, geom = "point", color = "darkblue", size = 4) +
    stat_summary(fun.data = function(x) data.frame(y = quantile(x, c(0.25, 0.75), na.rm = TRUE)), 
                 geom = "ribbon", alpha = 0.2, fill = "orange") +
    scale_x_continuous(breaks = 1:4, labels = c("2023\nAnnual", "2024\nPBL", "2024\nAnnual", "2025\nPBL")) +
    labs(
      title = "Population-Level Response Pattern",
      subtitle = "Mean ± 95% CI (blue) with interquartile range (orange)",
      x = "Time Period", 
      y = "Bleaching Extent (%)"
    ) +
    theme_coral_analysis
  
  # Panel C: Ridge plot showing distribution changes
  p1c <- ggplot(temporal_enhanced, aes(x = ext_anybleaching, y = factor(timepoint_label), fill = factor(timepoint_label))) +
    geom_density_ridges(alpha = 0.7, scale = 0.9) +
    scale_fill_viridis_d(name = "Time Period", option = "mako") +
    labs(
      title = "Bleaching Distribution Evolution",
      subtitle = "Density distributions show changing response patterns",
      x = "Bleaching Extent (%)",
      y = "Time Period"
    ) +
    theme_coral_analysis +
    theme(legend.position = "none")
  
  # Combine panels into comprehensive temporal visualization
  temporal_combined <- (p1a | p1b) / p1c +
    plot_annotation(
      title = "COMPREHENSIVE TEMPORAL ANALYSIS OF CORAL BLEACHING RESPONSES",
      subtitle = "Multi-perspective visualization of coral condition changes from 2023-2025",
      caption = "This integrated visualization reveals coral response patterns across multiple perspectives.\nPanel A shows individual site trajectories classified by data-driven pattern analysis.\nPanel B reveals population-level trends with statistical confidence intervals.\nPanel C displays the evolution of response distributions across time periods.\nTogether, these perspectives provide comprehensive insight into coral recovery dynamics.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_comprehensive_temporal_analysis.png", temporal_combined, 
         width = 16, height = 12, dpi = 300, bg = "white")
  cat("Saved: 05_plot_comprehensive_temporal_analysis.png\n")
  
} else {
  cat("Temporal data not available - skipping temporal trajectory analysis\n")
}

# ============================================================================
# ADVANCED RECOVERY RESPONSE ANALYSIS DASHBOARD
# ============================================================================

cat("\nSTEP 4: Creating advanced recovery response analysis dashboard\n")
cat("-------------------------------------------------------------\n")

if("combined_response_metrics" %in% names(loaded_data) && "site_response_patterns" %in% names(loaded_data)) {
  
  response_data <- loaded_data$combined_response_metrics
  pattern_data <- loaded_data$site_response_patterns
  
  # Calculate data-driven categories for visualization
  response_categories <- response_data %>%
    mutate(
      recovery_category_detailed = case_when(
        recovery_achieved <= recovery_quartiles[2] ~ "Minimal\n(0-25th %ile)",
        recovery_achieved <= recovery_quartiles[3] ~ "Moderate\n(25th-50th %ile)",
        recovery_achieved <= recovery_quartiles[4] ~ "Strong\n(50th-75th %ile)",
        recovery_achieved > recovery_quartiles[4] ~ "Exceptional\n(>75th %ile)",
        TRUE ~ "None"
      ),
      recovery_category_detailed = factor(recovery_category_detailed,
                                         levels = c("None", "Minimal\n(0-25th %ile)", "Moderate\n(25th-50th %ile)",
                                                   "Strong\n(50th-75th %ile)", "Exceptional\n(>75th %ile)")),
      period_label = ifelse(period == "2023_to_2024_PBL", "2023→2024 PBL", "2024→2025 PBL")
    )
  
  # Panel A: Recovery magnitude comparison with detailed statistics
  p2a <- ggplot(response_categories, aes(x = period_label, y = recovery_achieved, fill = period_label)) +
    geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.5, width = 0.2, size = 1.5) +
    stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = 18) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = "red", width = 0.1, size = 1) +
    scale_fill_viridis_d(option = "viridis", begin = 0.3, end = 0.8) +
    labs(
      title = "Recovery Magnitude Distribution Comparison",
      subtitle = "Violin plots show full distribution; red indicates mean ± 95% CI",
      x = "Response Period",
      y = "Recovery Achieved (% bleaching reduction)"
    ) +
    theme_coral_analysis +
    theme(legend.position = "none")
  
  # Panel B: Recovery categories stacked comparison
  category_summary <- response_categories %>%
    group_by(period_label, recovery_category_detailed) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(period_label) %>%
    mutate(percentage = count / sum(count) * 100)
  
  p2b <- ggplot(category_summary, aes(x = period_label, y = percentage, fill = recovery_category_detailed)) +
    geom_col(position = "stack", alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%d", count)), 
              position = position_stack(vjust = 0.5), 
              size = 3.5, color = "white", fontface = "bold") +
    scale_fill_viridis_d(name = "Recovery\nCategory", option = "plasma") +
    labs(
      title = "Recovery Category Composition",
      subtitle = "Data-driven quartile categories with site counts",
      x = "Response Period",
      y = "Percentage of Sites (%)"
    ) +
    theme_coral_analysis
  
  # Panel C: Site consistency analysis
  if(nrow(pattern_data) > 0) {
    consistency_data <- pattern_data %>%
      mutate(
        consistency_category = case_when(
          abs(recovery_achieved_p2 - recovery_achieved_p1) <= 5 ~ "Highly Consistent\n(±5%)",
          abs(recovery_achieved_p2 - recovery_achieved_p1) <= 15 ~ "Moderately Consistent\n(±15%)",
          recovery_achieved_p2 > recovery_achieved_p1 + 15 ~ "Improving Performance\n(>15% gain)",
          recovery_achieved_p1 > recovery_achieved_p2 + 15 ~ "Declining Performance\n(>15% loss)",
          TRUE ~ "Variable"
        )
      )
    
    p2c <- ggplot(consistency_data, aes(x = recovery_achieved_p1, y = recovery_achieved_p2, 
                                       color = consistency_category)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      geom_point(size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 1) +
      scale_color_viridis_d(name = "Consistency\nPattern") +
      labs(
        title = "Site Response Consistency Analysis",
        subtitle = "Diagonal line shows perfect consistency; trend line shows overall relationship",
        x = "Period 1 Recovery (2023→2024 PBL, %)",
        y = "Period 2 Recovery (2024→2025 PBL, %)"
      ) +
      theme_coral_analysis
    
  } else {
    p2c <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Site pattern data not available"), 
                size = 6) +
      theme_void()
  }
  
  # Panel D: Recovery efficiency analysis
  efficiency_data <- response_categories %>%
    filter(initial_bleaching > 0) %>%  # Only sites with initial bleaching
    mutate(
      recovery_efficiency = recovery_achieved / initial_bleaching,
      efficiency_category = case_when(
        recovery_efficiency <= 0.25 ~ "Low Efficiency\n(<25% recovered)",
        recovery_efficiency <= 0.5 ~ "Moderate Efficiency\n(25-50% recovered)",
        recovery_efficiency <= 0.75 ~ "High Efficiency\n(50-75% recovered)",
        recovery_efficiency > 0.75 ~ "Very High Efficiency\n(>75% recovered)",
        TRUE ~ "No Recovery"
      )
    )
  
  p2d <- ggplot(efficiency_data, aes(x = initial_bleaching, y = recovery_achieved, 
                                    color = efficiency_category, size = recovery_efficiency)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    scale_color_viridis_d(name = "Recovery\nEfficiency") +
    scale_size_continuous(name = "Efficiency\nRatio", range = c(2, 8)) +
    facet_wrap(~period_label) +
    labs(
      title = "Recovery Efficiency by Initial Impact",
      subtitle = "Point size shows efficiency ratio; trend line reveals relationship",
      x = "Initial Bleaching Extent (%)",
      y = "Recovery Achieved (%)"
    ) +
    theme_coral_analysis
  
  # Combine into comprehensive recovery dashboard
  recovery_dashboard <- (p2a | p2b) / (p2c | p2d) +
    plot_annotation(
      title = "COMPREHENSIVE RECOVERY RESPONSE ANALYSIS DASHBOARD",
      subtitle = "Multi-dimensional analysis of coral recovery patterns and efficiency",
      caption = "This dashboard provides comprehensive analysis of coral recovery responses.\nPanel A compares recovery magnitude distributions between time periods using violin plots.\nPanel B shows recovery category composition using data-driven quartile classifications.\nPanel C analyzes site consistency patterns across response periods.\nPanel D examines recovery efficiency relative to initial bleaching impact.\nTogether, these analyses reveal the complexity and variability of coral recovery processes.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_recovery_response_dashboard.png", recovery_dashboard, 
         width = 18, height = 14, dpi = 300, bg = "white")
  cat("Saved: 05_plot_recovery_response_dashboard.png\n")
  
} else {
  cat("Response data not available - skipping recovery dashboard\n")
}

# ============================================================================
# ADVANCED PREDICTIVE RELATIONSHIP VISUALIZATION
# ============================================================================

cat("\nSTEP 5: Creating advanced predictive relationship visualizations\n")
cat("---------------------------------------------------------------\n")

if("predictive_dataset_complete" %in% names(loaded_data)) {
  
  predictive_data <- loaded_data$predictive_dataset_complete
  
  # Create comprehensive predictive analysis visualization
  # Justification: Multi-panel predictive visualization reveals the complex
  # relationships between different predictor variables and coral responses
  
  # Panel A: Previous bleaching vs current response with thermal context
  if("max_dhw_2024" %in% names(predictive_data)) {
    p3a <- ggplot(predictive_data, aes(x = baseline_2023_annual, y = recovery_2024_to_2025)) +
      geom_point(aes(color = max_dhw_2024, size = thermal_variability), alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1.5) +
      scale_color_viridis_c(name = "2024 DHW", option = "plasma") +
      scale_size_continuous(name = "Thermal\nVariability", range = c(2, 8)) +
      labs(
        title = "Historical Bleaching Impact vs Current Recovery",
        subtitle = "Point color = 2024 DHW, Point size = thermal variability",
        x = "2023 Annual Bleaching Extent (%)",
        y = "2024→2025 Recovery Achieved (%)"
      ) +
      theme_coral_analysis
  } else {
    p3a <- ggplot(predictive_data, aes(x = baseline_2023_annual, y = recovery_2024_to_2025)) +
      geom_point(aes(color = recovery_2023_to_2024), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1.5) +
      scale_color_viridis_c(name = "Previous\nRecovery (%)") +
      labs(
        title = "Historical Bleaching Impact vs Current Recovery",
        subtitle = "Point color shows previous recovery performance",
        x = "2023 Annual Bleaching Extent (%)",
        y = "2024→2025 Recovery Achieved (%)"
      ) +
      theme_coral_analysis
  }
  
  # Panel B: Current thermal stress vs response
  if("max_dhw_2024" %in% names(predictive_data)) {
    p3b <- ggplot(predictive_data, aes(x = max_dhw_2024, y = recovery_2024_to_2025)) +
      geom_point(aes(color = baseline_2023_annual, size = thermal_variability), alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "red", size = 1.5) +
      scale_color_viridis_c(name = "2023 Bleaching\nExtent (%)") +
      scale_size_continuous(name = "Thermal\nVariability", range = c(2, 8)) +
      labs(
        title = "Current Thermal Stress vs Recovery Response",
        subtitle = "Point color = 2023 bleaching, Point size = thermal variability",
        x = "2024 Maximum DHW",
        y = "2024→2025 Recovery Achieved (%)"
      ) +
      theme_coral_analysis
  } else {
    p3b <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Thermal stress data not available"), 
                size = 6) +
      theme_void()
  }
  
  # Panel C: Residual analysis for model validation
  if("max_dhw_2024" %in% names(predictive_data)) {
    # Fit models and extract residuals
    model_prev <- lm(recovery_2024_to_2025 ~ baseline_2023_annual, data = predictive_data)
    model_dhw <- lm(recovery_2024_to_2025 ~ max_dhw_2024, data = predictive_data)
    
    residual_data <- data.frame(
      site = rep(predictive_data$site, 2),
      predictor = rep(c("Previous Bleaching", "Current DHW"), each = nrow(predictive_data)),
      fitted = c(fitted(model_prev), fitted(model_dhw)),
      residual = c(residuals(model_prev), residuals(model_dhw)),
      r_squared = rep(c(summary(model_prev)$r.squared, summary(model_dhw)$r.squared), 
                     each = nrow(predictive_data))
    ) %>%
      mutate(
        predictor = paste0(predictor, "\n(R² = ", sprintf("%.3f", r_squared), ")")
      )
    
    p3c <- ggplot(residual_data, aes(x = fitted, y = residual)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "red", size = 1) +
      facet_wrap(~predictor, scales = "free_x") +
      labs(
        title = "Model Residual Analysis",
        subtitle = "Assessing model fit quality and assumptions",
        x = "Fitted Values",
        y = "Residuals"
      ) +
      theme_coral_analysis
  } else {
    p3c <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient data for residual analysis"), 
                size = 6) +
      theme_void()
  }
  
  # Panel D: Prediction intervals and uncertainty
  prediction_data <- predictive_data %>%
    filter(!is.na(baseline_2023_annual) & !is.na(recovery_2024_to_2025))
  
  if(nrow(prediction_data) > 10) {
    p3d <- ggplot(prediction_data, aes(x = baseline_2023_annual, y = recovery_2024_to_2025)) +
      geom_smooth(method = "lm", se = TRUE, level = 0.95, fill = "lightblue", alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE, level = 0.68, fill = "darkblue", alpha = 0.4) +
      geom_point(alpha = 0.7, size = 2, color = "darkblue") +
      labs(
        title = "Prediction Intervals and Uncertainty",
        subtitle = "Dark blue = 68% CI, Light blue = 95% CI",
        x = "2023 Annual Bleaching Extent (%)",
        y = "2024→2025 Recovery Achieved (%)"
      ) +
      theme_coral_analysis
  } else {
    p3d <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient data for prediction intervals"), 
                size = 6) +
      theme_void()
  }
  
  # Combine predictive analysis panels
  predictive_analysis <- (p3a | p3b) / (p3c | p3d) +
    plot_annotation(
      title = "COMPREHENSIVE PREDICTIVE RELATIONSHIP ANALYSIS",
      subtitle = "Examining the predictive power of historical vs. current factors",
      caption = "This analysis compares different predictive approaches for coral recovery.\nPanel A examines historical bleaching impact as a predictor with thermal context.\nPanel B evaluates current thermal stress as a predictor with historical context.\nPanel C provides residual analysis for model validation and assumption checking.\nPanel D shows prediction intervals and uncertainty quantification.\nTogether, these analyses reveal the relative importance of different predictive factors.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_predictive_relationship_analysis.png", predictive_analysis, 
         width = 18, height = 14, dpi = 300, bg = "white")
  cat("Saved: 05_plot_predictive_relationship_analysis.png\n")
  
} else {
  cat("Predictive data not available - skipping predictive relationship analysis\n")
}

# ============================================================================
# MODEL PERFORMANCE COMPARISON VISUALIZATION
# ============================================================================

cat("\nSTEP 6: Creating advanced model performance comparison visualization\n")
cat("-------------------------------------------------------------------\n")

if("model_performance_comparison" %in% names(loaded_data)) {
  
  model_data <- loaded_data$model_performance_comparison
  
  # Create comprehensive model comparison visualization
  # Justification: Multi-metric model comparison reveals different aspects
  # of predictive performance and helps identify optimal modeling approaches
  
  # Prepare data for visualization
  model_comparison_long <- model_data %>%
    mutate(Model_Clean = str_replace_all(Model, "_", " ")) %>%
    select(Model_Clean, R_Squared, Adj_R_Squared, RMSE, AIC, N_Predictors) %>%
    pivot_longer(cols = c(R_Squared, Adj_R_Squared, RMSE, AIC), 
                 names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric_Label = case_when(
        Metric == "R_Squared" ~ "R²",
        Metric == "Adj_R_Squared" ~ "Adjusted R²",
        Metric == "RMSE" ~ "RMSE",
        Metric == "AIC" ~ "AIC"
      ),
      Performance_Direction = case_when(
        Metric %in% c("R_Squared", "Adj_R_Squared") ~ "Higher is Better",
        Metric %in% c("RMSE", "AIC") ~ "Lower is Better"
      )
    )
  
  # Panel A: R-squared comparison
  p4a <- model_comparison_long %>%
    filter(Metric %in% c("R_Squared", "Adj_R_Squared")) %>%
    ggplot(aes(x = reorder(Model_Clean, Value), y = Value, fill = Metric_Label)) +
    geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.3f", Value)), 
              position = position_dodge(width = 0.9), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_fill_viridis_d(name = "R² Metric") +
    coord_flip() +
    labs(
      title = "Model Explanatory Power Comparison",
      subtitle = "Higher values indicate better model fit",
      x = "Model",
      y = "R² Value"
    ) +
    theme_coral_analysis
  
  # Panel B: Error metrics comparison
  p4b <- model_comparison_long %>%
    filter(Metric %in% c("RMSE", "AIC")) %>%
    ggplot(aes(x = reorder(Model_Clean, -Value), y = Value, fill = Metric_Label)) +
    geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f", Value)), 
              position = position_dodge(width = 0.9), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_fill_viridis_d(name = "Error Metric", option = "plasma") +
    coord_flip() +
    labs(
      title = "Model Error and Complexity Comparison", 
      subtitle = "Lower values indicate better performance",
      x = "Model",
      y = "Error Value"
    ) +
    theme_coral_analysis
  
  # Panel C: Complexity vs. Performance trade-off
  p4c <- model_data %>%
    ggplot(aes(x = N_Predictors, y = Adj_R_Squared)) +
    geom_point(aes(size = RMSE, color = Model), alpha = 0.8) +
    geom_text_repel(aes(label = str_replace_all(Model, "_", " ")), 
                    size = 3, fontface = "bold") +
    scale_color_viridis_d(name = "Model") +
    scale_size_continuous(name = "RMSE", range = c(3, 10), guide = "legend") +
    labs(
      title = "Model Complexity vs. Performance Trade-off",
      subtitle = "Point size shows RMSE (smaller = better)",
      x = "Number of Predictors",
      y = "Adjusted R²"
    ) +
    theme_coral_analysis +
    theme(legend.position = "none")
  
  # Panel D: Model ranking summary
  model_ranking <- model_data %>%
    mutate(
      R2_rank = rank(desc(Adj_R_Squared)),
      RMSE_rank = rank(RMSE),
      AIC_rank = rank(AIC),
      Overall_rank = (R2_rank + RMSE_rank + AIC_rank) / 3,
      Model_Clean = str_replace_all(Model, "_", " ")
    ) %>%
    arrange(Overall_rank) %>%
    slice_head(n = min(8, nrow(.)))
  
  p4d <- ggplot(model_ranking, aes(x = reorder(Model_Clean, -Overall_rank), y = Overall_rank)) +
    geom_col(fill = "steelblue", alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f", Overall_rank)), 
              hjust = -0.1, size = 4, fontface = "bold") +
    coord_flip() +
    labs(
      title = "Overall Model Ranking",
      subtitle = "Combined ranking across all performance metrics",
      x = "Model",
      y = "Average Rank (Lower is Better)"
    ) +
    theme_coral_analysis
  
  # Combine model comparison panels
  model_comparison_viz <- (p4a | p4b) / (p4c | p4d) +
    plot_annotation(
      title = "COMPREHENSIVE MODEL PERFORMANCE COMPARISON",
      subtitle = "Multi-metric evaluation of predictive model performance",
      caption = "This comprehensive comparison evaluates models across multiple performance dimensions.\nPanel A compares explanatory power using R² metrics (higher is better).\nPanel B examines error metrics and model complexity (lower is better).\nPanel C reveals the complexity vs. performance trade-off relationship.\nPanel D provides an overall ranking combining all performance metrics.\nTogether, these analyses identify the optimal modeling approach for coral recovery prediction.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_model_performance_comparison.png", model_comparison_viz, 
         width = 18, height = 14, dpi = 300, bg = "white")
  cat("Saved: 05_plot_model_performance_comparison.png\n")
  
} else {
  cat("Model performance data not available - skipping model comparison\n")
}

# ============================================================================
# CORRELATION NETWORK ANALYSIS VISUALIZATION
# ============================================================================

cat("\nSTEP 7: Creating advanced correlation network analysis\n")
cat("-----------------------------------------------------\n")

if("correlation_analysis_detailed" %in% names(loaded_data)) {
  
  correlation_data <- loaded_data$correlation_analysis_detailed
  
  # Create network-style correlation visualization
  # Justification: Network visualization reveals complex relationships between
  # multiple variables and identifies key nodes in the predictive network
  
  # Prepare correlation matrix for network analysis
  correlation_matrix_data <- correlation_data %>%
    filter(abs(Correlation) > 0.1) %>%  # Filter weak correlations
    mutate(
      correlation_strength = case_when(
        abs(Correlation) >= 0.5 ~ "Strong",
        abs(Correlation) >= 0.3 ~ "Moderate", 
        abs(Correlation) >= 0.1 ~ "Weak"
      ),
      correlation_direction = ifelse(Correlation > 0, "Positive", "Negative")
    )
  
  # Create correlation heatmap with advanced styling
  if(nrow(correlation_matrix_data) > 0) {
    p5a <- ggplot(correlation_matrix_data, aes(x = Predictor, y = Response, fill = Correlation)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = sprintf("%.2f", Correlation)), 
                color = "white", fontface = "bold", size = 3) +
      scale_fill_gradient2(name = "Correlation\nCoefficient", 
                          low = "darkblue", mid = "white", high = "darkred", 
                          midpoint = 0, limits = c(-1, 1)) +
      labs(
        title = "Predictor-Response Correlation Matrix",
        subtitle = "Color intensity indicates correlation strength",
        x = "Predictor Variables",
        y = "Response Variables"
      ) +
      theme_coral_analysis +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Create correlation strength distribution
    p5b <- ggplot(correlation_matrix_data, aes(x = abs(Correlation), fill = correlation_strength)) +
      geom_histogram(binwidth = 0.05, alpha = 0.8, color = "white") +
      geom_vline(xintercept = c(0.3, 0.5), linetype = "dashed", color = "red") +
      scale_fill_viridis_d(name = "Correlation\nStrength") +
      labs(
        title = "Distribution of Correlation Strengths",
        subtitle = "Dashed lines mark moderate (0.3) and strong (0.5) thresholds",
        x = "Absolute Correlation Coefficient",
        y = "Count"
      ) +
      theme_coral_analysis
    
    # Create predictor importance ranking
    predictor_importance <- correlation_matrix_data %>%
      group_by(Predictor) %>%
      summarise(
        mean_abs_correlation = mean(abs(Correlation)),
        max_correlation = max(abs(Correlation)),
        n_significant = sum(abs(Correlation) > 0.3),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_abs_correlation))
    
    p5c <- ggplot(predictor_importance, aes(x = reorder(Predictor, mean_abs_correlation), 
                                           y = mean_abs_correlation)) +
      geom_col(aes(fill = n_significant), alpha = 0.8, color = "white") +
      geom_text(aes(label = sprintf("%.2f", mean_abs_correlation)), 
                hjust = -0.1, size = 3.5, fontface = "bold") +
      scale_fill_viridis_c(name = "# Strong\nCorrelations") +
      coord_flip() +
      labs(
        title = "Predictor Variable Importance Ranking",
        subtitle = "Based on mean absolute correlation across all responses",
        x = "Predictor Variable",
        y = "Mean Absolute Correlation"
      ) +
      theme_coral_analysis
    
    # Create significance pattern analysis
    significance_data <- correlation_matrix_data %>%
      mutate(
        significance_level = case_when(
          P_Value < 0.001 ~ "p < 0.001",
          P_Value < 0.01 ~ "p < 0.01",
          P_Value < 0.05 ~ "p < 0.05",
          TRUE ~ "p ≥ 0.05"
        )
      )
    
    p5d <- ggplot(significance_data, aes(x = abs(Correlation), y = -log10(P_Value), 
                                        color = significance_level, size = N_Obs)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
      geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
      scale_color_viridis_d(name = "Significance\nLevel") +
      scale_size_continuous(name = "Sample Size", range = c(2, 8)) +
      labs(
        title = "Correlation Significance Analysis",
        subtitle = "Volcano plot showing correlation strength vs. statistical significance",
        x = "Absolute Correlation Coefficient",
        y = "-log₁₀(p-value)"
      ) +
      theme_coral_analysis
    
  } else {
    # Fallback plots if correlation data is insufficient
    p5a <- p5b <- p5c <- p5d <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient correlation data"), 
                size = 6) +
      theme_void()
  }
  
  # Combine correlation analysis panels
  correlation_analysis <- (p5a | p5b) / (p5c | p5d) +
    plot_annotation(
      title = "COMPREHENSIVE CORRELATION NETWORK ANALYSIS",
      subtitle = "Multi-dimensional analysis of predictor-response relationships",
      caption = "This analysis provides comprehensive examination of correlation patterns.\nPanel A shows the correlation matrix with color-coded relationship strengths.\nPanel B displays the distribution of correlation strengths across all relationships.\nPanel C ranks predictor variables by their overall importance in the network.\nPanel D provides significance analysis combining correlation strength and statistical confidence.\nTogether, these analyses reveal the structure of predictive relationships in the data.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_correlation_network_analysis.png", correlation_analysis, 
         width = 18, height = 14, dpi = 300, bg = "white")
  cat("Saved: 05_plot_correlation_network_analysis.png\n")
  
} else {
  cat("Correlation data not available - skipping correlation network analysis\n")
}

# ============================================================================
# EXECUTIVE SUMMARY DASHBOARD
# ============================================================================

cat("\nSTEP 8: Creating executive summary dashboard\n")
cat("-------------------------------------------\n")

# Create a comprehensive executive summary that synthesizes key findings
# Justification: Executive dashboard provides high-level synthesis for
# decision-makers and stakeholders requiring key insights at a glance

if(length(loaded_data) >= 2) {
  
  # Calculate key summary statistics
  summary_stats <- list()
  
  if("combined_response_metrics" %in% names(loaded_data)) {
    response_summary <- loaded_data$combined_response_metrics %>%
      group_by(period) %>%
      summarise(
        n_sites = n(),
        mean_recovery = mean(recovery_achieved, na.rm = TRUE),
        percent_recovering = mean(recovery_achieved > 5, na.rm = TRUE) * 100,
        percent_worsening = mean(response_magnitude > 5, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    summary_stats$response <- response_summary
  }
  
  if("model_performance_comparison" %in% names(loaded_data)) {
    best_model <- loaded_data$model_performance_comparison %>%
      slice_max(Adj_R_Squared, n = 1)
    summary_stats$best_model <- best_model
  }
  
  # Create summary text elements
  summary_text_data <- data.frame(
    x = c(0.1, 0.1, 0.1, 0.1),
    y = c(0.8, 0.6, 0.4, 0.2),
    text = c(
      "KEY FINDINGS SUMMARY",
      if(exists("response_summary")) {
        sprintf("• %d sites analyzed across 2 time periods", 
                max(response_summary$n_sites, na.rm = TRUE))
      } else { "• Comprehensive coral response analysis" },
      if(exists("response_summary")) {
        sprintf("• %.1f%% of sites showed recovery (>5%% reduction)", 
                mean(response_summary$percent_recovering, na.rm = TRUE))
      } else { "• Recovery patterns documented" },
      if(exists("best_model")) {
        sprintf("• Best predictive model: %s (R² = %.3f)", 
                str_replace_all(best_model$Model, "_", " "), 
                best_model$Adj_R_Squared)
      } else { "• Predictive relationships analyzed" }
    )
  )
  
  # Panel A: Key findings text summary
  p6a <- ggplot(summary_text_data, aes(x = x, y = y, label = text)) +
    geom_text(hjust = 0, vjust = 1, size = c(6, 4, 4, 4), 
              fontface = c("bold", "plain", "plain", "plain"),
              color = c("darkblue", "black", "black", "black")) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    labs(title = "Executive Summary")
  
  # Panel B: Recovery overview (if data available)
  if("combined_response_metrics" %in% names(loaded_data)) {
    recovery_overview <- loaded_data$combined_response_metrics %>%
      mutate(
        outcome_category = case_when(
          recovery_achieved > 15 ~ "Strong Recovery",
          recovery_achieved > 5 ~ "Moderate Recovery",
          abs(response_magnitude) <= 5 ~ "Stable",
          response_magnitude > 5 ~ "Worsening",
          TRUE ~ "Other"
        ),
        period_label = ifelse(period == "2023_to_2024_PBL", "2023→2024", "2024→2025")
      ) %>%
      count(period_label, outcome_category) %>%
      group_by(period_label) %>%
      mutate(percentage = n / sum(n) * 100)
    
    p6b <- ggplot(recovery_overview, aes(x = period_label, y = percentage, fill = outcome_category)) +
      geom_col(position = "stack", alpha = 0.8) +
      geom_text(aes(label = sprintf("%d", n)), 
                position = position_stack(vjust = 0.5), 
                color = "white", fontface = "bold") +
      scale_fill_viridis_d(name = "Outcome") +
      labs(
        title = "Recovery Outcomes by Period",
        x = "Time Period",
        y = "Percentage of Sites (%)"
      ) +
      theme_coral_analysis
  } else {
    p6b <- ggplot() + theme_void()
  }
  
  # Panel C: Model performance summary (if data available)
  if("model_performance_comparison" %in% names(loaded_data)) {
    top_models <- loaded_data$model_performance_comparison %>%
      slice_max(Adj_R_Squared, n = 5) %>%
      mutate(Model_Clean = str_replace_all(Model, "_", " "))
    
    p6c <- ggplot(top_models, aes(x = reorder(Model_Clean, Adj_R_Squared), y = Adj_R_Squared)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.3f", Adj_R_Squared)), 
                hjust = -0.1, fontface = "bold") +
      coord_flip() +
      labs(
        title = "Top Predictive Models",
        x = "Model",
        y = "Adjusted R²"
      ) +
      theme_coral_analysis
  } else {
    p6c <- ggplot() + theme_void()
  }
  
  # Panel D: Data coverage summary
  data_coverage <- data.frame(
    Dataset = c("Response Metrics", "Thermal Data", "Site Patterns", "Predictions"),
    Available = c(
      "combined_response_metrics" %in% names(loaded_data),
      "comprehensive_thermal_data" %in% names(loaded_data),
      "site_response_patterns" %in% names(loaded_data),
      "predictive_dataset_complete" %in% names(loaded_data)
    ),
    Records = c(
      if("combined_response_metrics" %in% names(loaded_data)) nrow(loaded_data$combined_response_metrics) else 0,
      if("comprehensive_thermal_data" %in% names(loaded_data)) nrow(loaded_data$comprehensive_thermal_data) else 0,
      if("site_response_patterns" %in% names(loaded_data)) nrow(loaded_data$site_response_patterns) else 0,
      if("predictive_dataset_complete" %in% names(loaded_data)) nrow(loaded_data$predictive_dataset_complete) else 0
    )
  )
  
  p6d <- ggplot(data_coverage, aes(x = Dataset, y = Records, fill = Available)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = Records), vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen"), 
                     name = "Data Available") +
    labs(
      title = "Data Coverage Summary",
      x = "Dataset Type",
      y = "Number of Records"
    ) +
    theme_coral_analysis +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine executive summary panels
  executive_summary <- (p6a | p6b) / (p6c | p6d) +
    plot_annotation(
      title = "EXECUTIVE SUMMARY DASHBOARD",
      subtitle = "High-level synthesis of coral bleaching response analysis",
      caption = "This executive dashboard provides key findings and data coverage summary.\nThe analysis reveals coral recovery patterns, predictive relationships, and model performance.\nData-driven quartile classifications ensure objective categorizations throughout the analysis.\nComprehensive visualization approaches provide multiple perspectives on coral resilience dynamics.",
      theme = theme(plot.title = element_text(size = 18, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   plot.caption = element_text(size = 11, hjust = 0))
    )
  
  ggsave("05_plot_executive_summary_dashboard.png", executive_summary, 
         width = 16, height = 12, dpi = 300, bg = "white")
  cat("Saved: 05_plot_executive_summary_dashboard.png\n")
  
} else {
  cat("Insufficient data available for executive summary dashboard\n")
}

# ============================================================================
# FINAL SUMMARY AND DOCUMENTATION
# ============================================================================

cat("\nFINAL VISUALIZATION ANALYSIS SUMMARY\n")
cat("====================================\n")

generated_visualizations <- c(
  "05_plot_comprehensive_temporal_analysis.png",
  "05_plot_recovery_response_dashboard.png", 
  "05_plot_predictive_relationship_analysis.png",
  "05_plot_model_performance_comparison.png",
  "05_plot_correlation_network_analysis.png",
  "05_plot_executive_summary_dashboard.png"
)

existing_visualizations <- generated_visualizations[file.exists(generated_visualizations)]

cat(sprintf("Generated %d comprehensive visualizations:\n", length(existing_visualizations)))
for(viz in existing_visualizations) {
  cat(sprintf("  ✓ %s\n", viz))
}

cat("\nVisualization Features:\n")
cat("  • Data-driven quartile-based categorizations throughout\n")
cat("  • Publication-quality themes and color schemes\n")
cat("  • Comprehensive captions explaining analytical choices\n")
cat("  • Multi-panel layouts revealing different perspectives\n")
cat("  • Statistical validation and uncertainty quantification\n")
cat("  • Executive-level summary for stakeholder communication\n")

cat("\nVisualization Themes Addressed:\n")
cat("  1. Temporal trajectory analysis with pattern classification\n")
cat("  2. Recovery response analysis with efficiency metrics\n")
cat("  3. Predictive relationship analysis with model validation\n")
cat("  4. Model performance comparison across multiple metrics\n")
cat("  5. Correlation network analysis with significance testing\n")
cat("  6. Executive summary dashboard for high-level insights\n")

cat("\n============================================================================\n")
cat("STEP 5 COMPLETE: Comprehensive visualization analysis with publication-quality graphics\n")
cat("Next: Site-specific analysis and characterization\n")
cat("============================================================================\n")