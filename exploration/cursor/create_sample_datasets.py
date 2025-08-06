import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random

# Set random seed for reproducibility
np.random.seed(42)
random.seed(42)

# Create site information
sites_33 = [f"Site_{i:02d}" for i in range(1, 34)]
sites_45 = [f"Site_{i:02d}" for i in range(1, 46)]

# Common locations/regions for sites
regions = ['Northern_GBR', 'Central_GBR', 'Southern_GBR', 'Coral_Triangle', 'Caribbean', 'Red_Sea']

def create_bleaching_extent_data():
    """Create coral condition bleaching extent data for 33 sites (2022-2025)"""
    
    # Time periods for bleaching surveys
    periods = ['2022_Annual', '2023_PBL', '2023_Annual', '2024_PBL', '2024_Annual', '2025_PBL']
    
    data = []
    
    for site in sites_33:
        # Assign region to site
        region = random.choice(regions)
        lat = np.random.uniform(-25, 25)  # Tropical latitudes
        lon = np.random.uniform(-180, 180)
        
        # Site characteristics that influence bleaching
        depth = np.random.uniform(2, 25)  # meters
        exposure = random.choice(['Protected', 'Semi-exposed', 'Exposed'])
        coral_cover_baseline = np.random.uniform(10, 80)  # % coral cover
        
        for period in periods:
            # Simulate bleaching extent (% of corals showing bleaching)
            base_bleaching = np.random.uniform(0, 40)
            
            # Add temporal patterns (higher bleaching in certain years)
            if '2023' in period:
                # Major bleaching event in 2023
                base_bleaching += np.random.uniform(20, 60)
            elif '2024' in period and 'Annual' in period:
                # Some recovery but still elevated
                base_bleaching += np.random.uniform(5, 25)
            
            # Add some site-specific effects
            if exposure == 'Exposed':
                base_bleaching += np.random.uniform(5, 15)
            elif exposure == 'Protected':
                base_bleaching -= np.random.uniform(0, 10)
                
            # Ensure realistic bounds
            bleaching_extent = max(0, min(100, base_bleaching + np.random.normal(0, 5)))
            
            data.append({
                'Site_ID': site,
                'Region': region,
                'Latitude': lat,
                'Longitude': lon,
                'Depth_m': depth,
                'Exposure': exposure,
                'Period': period,
                'Year': int(period.split('_')[0]),
                'Survey_Type': period.split('_')[1],
                'Bleaching_Extent_Percent': round(bleaching_extent, 1),
                'Coral_Cover_Percent': round(max(0, coral_cover_baseline - bleaching_extent * 0.3), 1),
                'Survey_Date': f"{period.split('_')[0]}-{random.choice(['03', '06', '09', '12'])}-{random.randint(1, 28):02d}"
            })
    
    df = pd.DataFrame(data)
    return df

def create_bleaching_prevalence_data():
    """Create coral condition bleaching prevalence data for 33 sites (2022-2025)"""
    
    periods = ['2022_Annual', '2023_PBL', '2023_Annual', '2024_PBL', '2024_Annual', '2025_PBL']
    
    data = []
    
    for site in sites_33:
        region = random.choice(regions)
        lat = np.random.uniform(-25, 25)
        lon = np.random.uniform(-180, 180)
        
        for period in periods:
            # Bleaching prevalence (proportion of sites/quadrats with bleaching)
            base_prevalence = np.random.uniform(0, 0.8)
            
            # Temporal patterns
            if '2023' in period:
                base_prevalence += np.random.uniform(0.2, 0.4)
            elif '2024' in period and 'Annual' in period:
                base_prevalence += np.random.uniform(0.1, 0.3)
            
            prevalence = max(0, min(1, base_prevalence + np.random.normal(0, 0.1)))
            
            # Severity categories
            severity_mild = np.random.uniform(0.1, 0.4) * prevalence
            severity_moderate = np.random.uniform(0.2, 0.5) * prevalence
            severity_severe = max(0, prevalence - severity_mild - severity_moderate)
            
            data.append({
                'Site_ID': site,
                'Region': region,
                'Latitude': lat,
                'Longitude': lon,
                'Period': period,
                'Year': int(period.split('_')[0]),
                'Survey_Type': period.split('_')[1],
                'Bleaching_Prevalence': round(prevalence, 3),
                'Severity_Mild_Prop': round(severity_mild, 3),
                'Severity_Moderate_Prop': round(severity_moderate, 3),
                'Severity_Severe_Prop': round(severity_severe, 3),
                'Total_Quadrats_Surveyed': random.randint(20, 100),
                'Survey_Date': f"{period.split('_')[0]}-{random.choice(['03', '06', '09', '12'])}-{random.randint(1, 28):02d}"
            })
    
    df = pd.DataFrame(data)
    return df

def create_temperature_dhw_data():
    """Create temperature and DHW data for 45 sites (2003-2025)"""
    
    data = []
    
    # Create weekly data from 2003 to 2025
    start_date = datetime(2003, 1, 1)
    end_date = datetime(2025, 12, 31)
    
    for site in sites_45:
        region = random.choice(regions)
        lat = np.random.uniform(-25, 25)
        lon = np.random.uniform(-180, 180)
        
        # Site-specific baseline temperature
        baseline_temp = 25 + (abs(lat) * -0.5) + np.random.normal(0, 2)
        
        current_date = start_date
        site_dhw_accumulation = 0
        
        while current_date <= end_date:
            # Seasonal temperature variation
            day_of_year = current_date.timetuple().tm_yday
            seasonal_temp = baseline_temp + 3 * np.sin(2 * np.pi * day_of_year / 365.25)
            
            # Long-term warming trend
            years_since_2003 = (current_date.year - 2003)
            warming_trend = years_since_2003 * 0.02  # ~0.2°C per decade
            
            # Year-specific anomalies (El Niño/La Niña effects)
            year_anomaly = 0
            if current_date.year in [2015, 2016, 2023]:  # Strong El Niño years
                year_anomaly = np.random.uniform(1, 3)
            elif current_date.year in [2007, 2008, 2010, 2011]:  # La Niña years
                year_anomaly = np.random.uniform(-2, -0.5)
            
            # Weekly temperature with noise
            weekly_temp = seasonal_temp + warming_trend + year_anomaly + np.random.normal(0, 0.5)
            
            # Calculate DHW (Degree Heating Weeks)
            # DHW accumulates when temp exceeds long-term maximum + 1°C
            threshold_temp = baseline_temp + 4  # Approximate bleaching threshold
            
            if weekly_temp > threshold_temp:
                dhw_this_week = (weekly_temp - threshold_temp)
                site_dhw_accumulation += dhw_this_week
            
            # DHW decays over time (12-week rolling window)
            if current_date > start_date + timedelta(weeks=12):
                site_dhw_accumulation *= 0.92  # Decay factor
            
            data.append({
                'Site_ID': site,
                'Region': region,
                'Latitude': lat,
                'Longitude': lon,
                'Date': current_date.strftime('%Y-%m-%d'),
                'Year': current_date.year,
                'Week': current_date.isocalendar()[1],
                'Sea_Surface_Temperature_C': round(weekly_temp, 2),
                'Temperature_Anomaly_C': round(weekly_temp - baseline_temp, 2),
                'DHW_DegreeWeeks': round(max(0, site_dhw_accumulation), 2),
                'Bleaching_Threshold_Exceeded': weekly_temp > threshold_temp
            })
            
            current_date += timedelta(weeks=1)
    
    df = pd.DataFrame(data)
    return df

# Generate the datasets
print("Creating bleaching extent dataset...")
bleaching_extent_df = create_bleaching_extent_data()
bleaching_extent_df.to_csv('/workspace/data/s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv', index=False)

print("Creating bleaching prevalence dataset...")
bleaching_prevalence_df = create_bleaching_prevalence_data()
bleaching_prevalence_df.to_csv('/workspace/data/s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv', index=False)

print("Creating temperature/DHW dataset...")
temp_dhw_df = create_temperature_dhw_data()
temp_dhw_df.to_csv('/workspace/data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv', index=False)

print("\nDataset summaries:")
print(f"Bleaching extent: {bleaching_extent_df.shape[0]} records")
print(f"Bleaching prevalence: {bleaching_prevalence_df.shape[0]} records") 
print(f"Temperature/DHW: {temp_dhw_df.shape[0]} records")

print("\nSample of bleaching extent data:")
print(bleaching_extent_df.head())

print("\nSample of temperature/DHW data:")
print(temp_dhw_df.head())