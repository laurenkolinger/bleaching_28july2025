# ===============================================================
# Coral Bleaching and Degree Heating Weeks Analysis
# TCRMP Data Analysis Script
# ===============================================================

## OVERALL : 
# how important was the preceding years recovery in determining the respon to the 2024-2025 bleaching event?
# especially relative to site specific factors such as species, composition, and depth, and baseline coral cover  
# background, we know basically what we expect to have for a coral response to bleaching based on a variety of factors, but we don't yet know how these reefs will respond to back to back bleaching events that are projected to become more common essay could either get more or less recoverable, depending on whether the


# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(viridis)
library(zoo)  # For rolling window calculations

# Set working directory to the project root (adjust as needed)
setwd(".")

# ===============================================================
# 1. DATA IMPORT
# ===============================================================

# Import bleaching extent data
bleaching_extent <- read.csv("data/s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv", 
                             stringsAsFactors = FALSE)

# Import bleaching prevalence data  
bleaching_prevalence <- read.csv("data/s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv", 
                                 stringsAsFactors = FALSE)

# Import DHW data
dhw_data <- read.csv("data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv", 
                     stringsAsFactors = FALSE)

# ===============================================================
# 2. DATA PREPROCESSING
# ===============================================================

# Convert date columns to proper date format
bleaching_extent$date <- as.Date(bleaching_extent$date)
bleaching_prevalence$date <- as.Date(bleaching_prevalence$date)

# Create week numbers for bleaching data to match with DHW data
bleaching_extent$week <- week(bleaching_extent$date)
bleaching_prevalence$week <- week(bleaching_prevalence$date)

# Check site overlap between datasets
common_sites <- intersect(unique(bleaching_extent$site), unique(dhw_data$site))
cat("Common sites between bleaching and DHW data:", length(common_sites), "\n")
cat("Sites in common:\n")
print(sort(common_sites))

# Prepare DHW data for weekly matching (DHW is already 12-week rolling average)
dhw_weekly <- dhw_data %>%
  select(year, week, site, dhw, weekly_max_temp, BT, depth, program) %>%
  rename(pre_survey_dhw = dhw,
         pre_survey_temp = weekly_max_temp,
         bleaching_threshold = BT)

# ===============================================================
# 3. CREATE ANALYSIS DATASETS
# ===============================================================


# Function to summarize bleaching data by site-year-period
calculate_bleaching_metrics_by_site_year_period <- function(data, metric_prefix) {
  data %>%
    group_by(site, year, period) %>%
    summarise(
      n_replicates = n(),
      survey_date = first(date),
      survey_week = first(week),
      pre_survey_week = pmax(1, first(week) - 1),  # Week before survey, minimum week 1
      across(
        all_of(grep("^(ext_|prev_)", names(data), value = TRUE)),
        ~ mean(., na.rm = TRUE),
        .names = "mean_{.col}"
      ),
      .groups="drop")
}

# Calculate bleaching metrics by grouping
bleaching_extent_by_site_year <- calculate_bleaching_metrics_by_site_year_period(bleaching_extent, "ext")
bleaching_prevalence_by_site_year <- calculate_bleaching_metrics_by_site_year_period(bleaching_prevalence, "prev")

# Add number of colonies for prevalence data
bleaching_prevalence_by_site_year <- bleaching_prevalence_by_site_year %>%
  left_join(
    bleaching_prevalence %>%
      group_by(site, year, period) %>%
      summarise(mean_ncolonies = mean(ncolonies, na.rm = TRUE), .groups = 'drop'),
    by = c("site", "year", "period")
  )


# ===============================================================
# 4. MERGE BLEACHING AND DHW DATA
# ===============================================================

# Merge extent data with DHW for the week before survey
bleaching_extent_with_dhw <- bleaching_extent_by_site_year %>%
  filter(site %in% common_sites) %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data for that week

# Merge prevalence data with DHW for the week before survey
bleaching_prevalence_with_dhw <- bleaching_prevalence_by_site_year %>%
  filter(site %in% common_sites) %>%
  left_join(dhw_weekly, by = c("site", "year", "pre_survey_week" = "week")) %>%
  filter(!is.na(pre_survey_dhw))  # Keep only records with DHW data for that week

# ===============================================================
# 5. BASIC PLOTTING 
# ===============================================================

# order sites  and their levels by depth 
bleaching_extent_with_dhw$site <- 
  factor(bleaching_extent_with_dhw$site,
         levels = unique(bleaching_extent_with_dhw$site[order(bleaching_extent_with_dhw$depth)]))

bleaching_prevalence_with_dhw$site <- 
  factor(bleaching_prevalence_with_dhw$site,
         levels = unique(bleaching_prevalence_with_dhw$site[order(bleaching_prevalence_with_dhw$depth)]))

# add mean prevalence of any bleaching to the extent df 
bleaching_extent_with_dhw <- bleaching_extent_with_dhw %>%
  left_join(
    bleaching_prevalence_with_dhw %>%
      select(site, year, period, mean_prev_anybleaching, mean_ncolonies),
    by = c("site", "year", "period")
  )

# find median date for each year-period combination 
median_dates <- bleaching_extent_with_dhw %>%
  group_by(year, period) %>%
  summarise(median_date = median(survey_date, na.rm = TRUE), .groups = 'drop')

# find median value for each year-period combination 
mean_extent_values <- bleaching_extent %>%
  group_by(year, period) %>% 
  summarise(mean_ext_anybleaching = mean(ext_anybleaching, na.rm = TRUE),
            sd_ext_anybleaching = sd(ext_anybleaching, na.rm = TRUE)) %>% 
  left_join(median_dates, by = c("year", "period"))

mean_prevalence_values <- bleaching_prevalence %>%
  group_by(year, period) %>% 
  summarise(mean_prev_anybleaching = mean(prev_anybleaching, na.rm = TRUE),
            sd_prev_anybleaching = sd(prev_anybleaching, na.rm = TRUE)) %>% 
  left_join(median_dates, by = c("year", "period"))

# add segment connecting 2023 Annual and 2024 PBL and 2024 Annual and 2025 PBL for each site , including the bleaching exttent and prevalence during those times 

segment_extent <- bleaching_extent_with_dhw %>%
  filter((year == 2023 & period == "Annual") | (year == 2024 & period == "PBL")) %>%
  select(site, year, period, survey_date, mean_ext_anybleaching, mean_prev_anybleaching) %>% 
  mutate(year_period = paste(year, period, sep = "_")) %>%
  select(-c(year, period)) %>%
  pivot_wider(names_from = year_period, values_from = c(survey_date, mean_ext_anybleaching, mean_prev_anybleaching))
  

segment_extent_2 <- bleaching_extent_with_dhw %>%
  filter((year == 2024 & period == "Annual") | (year == 2025 & period == "PBL")) %>%
  select(site, year, period, survey_date, mean_ext_anybleaching, mean_prev_anybleaching) %>% 
  mutate(year_period = paste(year, period, sep = "_")) %>%
  select(-c(year, period)) %>%
  pivot_wider(names_from = year_period, values_from = c(survey_date, mean_ext_anybleaching, mean_prev_anybleaching))


# Plot bleaching extent over time
ggplot() +
  geom_point(data = bleaching_extent_with_dhw, aes(x = survey_date, y = mean_ext_anybleaching, color = site, group = site, size = mean_prev_anybleaching)) +
  labs(title = "Bleaching Extent Over Time",
       # x = "Survey Date",
       y = "Mean Extent Bleaching") +
geom_segment(data = segment_extent, 
               aes(x = survey_date_2023_Annual, xend = survey_date_2024_PBL, 
                   y = mean_ext_anybleaching_2023_Annual, yend = mean_ext_anybleaching_2024_PBL, color = site), linetype = "dashed") +
  geom_segment(data = segment_extent_2, 
               aes(x = survey_date_2024_Annual, xend = survey_date_2025_PBL, 
                   y = mean_ext_anybleaching_2024_Annual, yend = mean_ext_anybleaching_2025_PBL, color = site), linetype = "dashed") +
  # geom_vline(data = median_dates, aes(xintercept = median_date), linetype = "dashed", color = "grey50") +
  geom_point(data = mean_extent_values, aes(x = median_date, y = mean_ext_anybleaching), color = "black")+
  geom_errorbar(data = mean_extent_values,
               aes(x = median_date, ymin = mean_ext_anybleaching - sd_ext_anybleaching,
                   ymax = mean_ext_anybleaching + sd_ext_anybleaching),
               width = 50, color = "black", alpha = 1, linewidth = 0.5) + 
  theme_bw() + 
  scale_x_date(breaks = sort(median_dates$median_date),
               labels = c("'22 Ann", "'23 Bleach", "'24 Recov", "'24 Bleach", "'25 Recov")) + 
  scale_color_viridis_d()


ggplot() + 
  geom_point(data = bleaching_prevalence_with_dhw, 
             aes(x = survey_date, y = mean_prev_anybleaching, color = site, group = site, size= mean_ncolonies)) +
  labs(title = "Bleaching Prevalence Over Time",
       # x = "Survey Date",
       y = "Mean Prevalence Bleaching") +
  geom_segment(data = segment_extent, 
               aes(x = survey_date_2023_Annual, xend = survey_date_2024_PBL, 
                   y = mean_prev_anybleaching_2023_Annual, yend = mean_prev_anybleaching_2024_PBL, color = site), linetype = "dashed") +
  geom_segment(data = segment_extent_2,
               aes(x = survey_date_2024_Annual, xend = survey_date_2025_PBL, 
                   y = mean_prev_anybleaching_2024_Annual, yend = mean_prev_anybleaching_2025_PBL, color = site), linetype = "dashed") +
  # geom_vline(data = median_dates, aes(xintercept = median_date), linetype = "dashed", color = "grey50") +
  geom_point(data = mean_prevalence_values, aes(x = median_date, y = mean_prev_anybleaching), color = "black")+
  geom_errorbar(data = mean_prevalence_values,
                aes(x = median_date, ymin = mean_prev_anybleaching - sd_prev_anybleaching,
                    ymax = mean_prev_anybleaching + sd_prev_anybleaching),
                width = 50, color = "black", alpha = 1, linewidth = 0.5) + 
  theme_bw() + 
  scale_x_date(breaks = sort(median_dates$median_date),
               labels = c("'22 Ann", "'23 Bleach", "'24 Recov", "'24 Bleach", "'25 Recov")) + 
  scale_color_viridis_d()

# next ideas: 
 # correlation of the two slopes? 
 # does slope 1 predict slope 2 , when also consider depth? species comp? dhw


# plot extent versus dhw 
ggplot(bleaching_extent_with_dhw, aes(x = pre_survey_dhw, y = mean_ext_anybleaching, color = site)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_point(aes(size = mean_prev_anybleaching, shape = interaction(year,period))) +
  labs(title = "Bleaching Extent vs Pre-Survey DHW",
       x = "DHW @ time of survey",
       y = "Mean Extent Bleaching") +
  theme_minimal() +
  # facet_wrap(~ site, scales = "free_y") +
  scale_color_viridis_d()

# plot prevalence versus dhw
ggplot(bleaching_prevalence_with_dhw, aes(x = pre_survey_dhw, y = mean_prev_anybleaching, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bleaching Prevalence vs Pre-Survey DHW",
       x = "Pre-Survey DHW",
       y = "Mean Prevalence Bleaching") +
  theme_minimal() +
  # facet_wrap(~ site, scales = "free_y") +
  scale_color_viridis_d()

# theme_minimal() +
# facet_wrap(~ site, scales = "fixed") +


