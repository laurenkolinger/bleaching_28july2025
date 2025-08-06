




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

dhw_data <- dhw_data %>% filter(year > 2021)

#convert year and week to date 
dhw_data <- dhw_data %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u")) 


bp_summ <- bleaching_prevalence %>% group_by(site,year, period) %>% 
  summarise(survey_date = date(first(date)), .groups = 'drop') 

p1 <- dhw_data %>% ggplot(aes(x =date, y = dhw, color = site)) +
  geom_line() +
  labs(title = "DHW Over Time by Site", x = "Date", y = "Degree Heating Weeks (DHW)") +
  theme_minimal() +
  scale_color_viridis_d() +
  facet_wrap(~ site, scales = "free_y") +
  theme(legend.position = "bottom")

p2 <- p1 + geom_vline(data = bp_summ, aes(xintercept = survey_date), 
                     color = "black", size = 0.5) +
  labs(title = "DHW with Survey Dates Marked", x = "Date", y = "Degree Heating Weeks (DHW)") + theme (legend.position= "none")

# calculate number of weeks between max dhw and the survey date for 2023 Annual and 2024 annual
surveyoffset <- bp_summ %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(period == "Annual")

dhw_max <- dhw_data %>% 
  filter(year %in% c(2023,2024)) %>% 
  filter(annual_max == 1)

surveyoffset <- surveyoffset %>%
  left_join(dhw_max, by = c("site", "year")) %>%
  mutate(days_to_survey = survey_date - date) 


hist(surveyoffset %>% filter(year==2023) %>% pull(days_to_survey) %>% as.numeric(), 
     main = "Days to Survey from Max DHW in 2023", 
     xlab = "Days", 
     ylab = "Frequency",
     breaks = 30)

hist(surveyoffset %>% filter(year==2024) %>% pull(days_to_survey) %>% as.numeric(), 
     main = "Days to Survey from Max DHW in 2023", 
     xlab = "Days", 
     ylab = "Frequency",
     breaks = 30)

#scatter plot of 2023 and 2024 days from max dhw, each poitn is a site
surveyoffset %>% filter(days_to_survey < 100) %>% select(year, site, days_to_survey) %>% pivot_wider(names_from = year, values_from = days_to_survey) %>% 
  ggplot(aes(x = `2023`, y = `2024`, color = site)) +
  geom_rect(aes(xmin = -20, xmax = 20, ymin = -20, ymax = 20), 
            fill = "grey80", color=NA) + 
  coord_fixed() +
  geom_point() +
  labs(title = "Days to Survey from Max DHW in 2023 vs 2024", 
       x = "Days to Survey from Max DHW in 2023", 
       y = "Days to Survey from Max DHW in 2024") +
  theme_minimal() +
  # scale_color_viridis_d() +
  theme(legend.position = "none") +
  # add line with slope of 1 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # add text label for each point
  geom_text(aes(label = site), vjust = -0.5, size = 3)

  #add y axis hist and x axis histogram  
 
sitekp <- surveyoffset %>% filter(days_to_survey < 100) %>% select(year, site, days_to_survey) %>% pivot_wider(names_from = year, values_from = days_to_survey) %>%
  # filter by the difftime column of days 
  mutate(`2024` = abs(as.numeric(`2024`))) %>%
  mutate(`2023` = abs(as.numeric(`2023`))) %>%
  filter(`2024` <= 21 & `2023` <= 21) %>% 
  pull(site)

 
 
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
