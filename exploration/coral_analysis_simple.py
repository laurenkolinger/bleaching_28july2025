#!/usr/bin/env python3
"""
Comprehensive Coral Bleaching Response Analysis
Analysis Period: 2023-2025 Bleaching Events  
Focus: 2024 Annual → 2025 PBL Response Prediction
Using Python standard library only
"""

import csv
import statistics
from collections import defaultdict, Counter
import math

print("="*60)
print("CORAL BLEACHING ANALYSIS PIPELINE")
print("Analysis Period: 2023-2025")
print("Objective: Evaluate 2024 coral response prediction")
print("="*60)
print()

def read_csv_file(filename):
    """Read CSV file and return headers and data"""
    with open(filename, 'r') as file:
        reader = csv.reader(file)
        headers = next(reader)
        data = list(reader)
    return headers, data

def safe_float(value):
    """Convert to float safely"""
    try:
        return float(value)
    except (ValueError, TypeError):
        return None

def calculate_correlation(x_list, y_list):
    """Calculate Pearson correlation coefficient"""
    # Remove pairs where either value is None
    pairs = [(x, y) for x, y in zip(x_list, y_list) if x is not None and y is not None]
    if len(pairs) < 2:
        return None
    
    x_vals = [p[0] for p in pairs]
    y_vals = [p[1] for p in pairs]
    
    n = len(pairs)
    x_mean = sum(x_vals) / n
    y_mean = sum(y_vals) / n
    
    numerator = sum((x - x_mean) * (y - y_mean) for x, y in pairs)
    x_var = sum((x - x_mean) ** 2 for x in x_vals)
    y_var = sum((y - y_mean) ** 2 for y in y_vals)
    
    if x_var == 0 or y_var == 0:
        return None
    
    return numerator / math.sqrt(x_var * y_var)

# Load datasets
print("Loading datasets...")

# Load extent data
extent_headers, extent_data = read_csv_file("s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv")
print(f"Extent data: {len(extent_data)} rows")

# Load temperature data  
temp_headers, temp_data = read_csv_file("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")
print(f"Temperature data: {len(temp_data)} rows")
print()

# Find column indices
extent_site_idx = extent_headers.index('site')
extent_year_idx = extent_headers.index('year')
extent_period_idx = extent_headers.index('period')
extent_bleaching_idx = extent_headers.index('ext_anybleaching')

temp_site_idx = temp_headers.index('site')
temp_year_idx = temp_headers.index('year')
temp_dhw_idx = temp_headers.index('dhw')
temp_max_idx = temp_headers.index('weekly_max_temp')

# Process extent data by site, year, period
print("Processing coral condition data...")
extent_processed = defaultdict(list)

for row in extent_data:
    site = row[extent_site_idx].strip('"')
    year = int(row[extent_year_idx])
    period = row[extent_period_idx].strip('"')
    bleaching = safe_float(row[extent_bleaching_idx])
    
    if bleaching is not None:
        extent_processed[(site, year, period)].append(bleaching)

# Calculate site-level means
extent_means = {}
for (site, year, period), values in extent_processed.items():
    extent_means[(site, year, period)] = statistics.mean(values)

print(f"Processed {len(extent_means)} site-year-period combinations")

# Process temperature data
print("Processing temperature data...")
temp_processed = defaultdict(lambda: {'dhw': [], 'temp': []})

for row in temp_data:
    try:
        year = int(row[temp_year_idx])
        if year not in [2023, 2024]:
            continue
        
        site = row[temp_site_idx].strip('"')
        dhw = safe_float(row[temp_dhw_idx])
        max_temp = safe_float(row[temp_max_idx])
        
        if dhw is not None:
            temp_processed[(site, year)]['dhw'].append(dhw)
        if max_temp is not None:
            temp_processed[(site, year)]['temp'].append(max_temp)
    except (ValueError, IndexError):
        continue

# Calculate temperature metrics
temp_metrics = {}
for (site, year), data in temp_processed.items():
    metrics = {}
    if data['dhw']:
        metrics['max_dhw'] = max(data['dhw'])
        metrics['total_dhw'] = sum(data['dhw'])
    if data['temp']:
        metrics['max_temp'] = max(data['temp'])
        metrics['mean_temp'] = statistics.mean(data['temp'])
        if len(data['temp']) > 1:
            metrics['temp_sd'] = statistics.stdev(data['temp'])
        else:
            metrics['temp_sd'] = 0
    
    temp_metrics[(site, year)] = metrics

print(f"Processed temperature data for {len(temp_metrics)} site-year combinations")
print()

# Extract key timepoints
print("Extracting key timepoints...")
timepoints = {
    '2023_Annual': {},
    '2024_PBL': {},
    '2024_Annual': {},
    '2025_PBL': {}
}

for (site, year, period), bleaching in extent_means.items():
    timepoint = f"{year}_{period}"
    if timepoint in timepoints:
        timepoints[timepoint][site] = bleaching

print("Data availability by timepoint:")
for tp, data in timepoints.items():
    print(f"  {tp}: {len(data)} sites")
print()

# Find sites with all four timepoints
all_timepoint_sites = set(timepoints['2023_Annual'].keys())
for tp_data in timepoints.values():
    all_timepoint_sites &= set(tp_data.keys())

print(f"Sites with all four timepoints: {len(all_timepoint_sites)}")

# Create analysis dataset
print("Creating comprehensive analysis dataset...")
analysis_data = []

for site in all_timepoint_sites:
    record = {
        'site': site,
        'bleaching_2023_annual': timepoints['2023_Annual'][site],
        'bleaching_2024_pbl': timepoints['2024_PBL'][site],
        'bleaching_2024_annual': timepoints['2024_Annual'][site],
        'bleaching_2025_pbl': timepoints['2025_PBL'][site]
    }
    
    # Add temperature data
    if (site, 2023) in temp_metrics:
        record['dhw_2023'] = temp_metrics[(site, 2023)].get('max_dhw', 0)
        record['temp_sd_2023'] = temp_metrics[(site, 2023)].get('temp_sd', 0)
    else:
        record['dhw_2023'] = 0
        record['temp_sd_2023'] = 0
        
    if (site, 2024) in temp_metrics:
        record['dhw_2024'] = temp_metrics[(site, 2024)].get('max_dhw', 0)
        record['temp_sd_2024'] = temp_metrics[(site, 2024)].get('temp_sd', 0)
    else:
        record['dhw_2024'] = 0
        record['temp_sd_2024'] = 0
    
    # Calculate response metrics
    record['response_2024_to_2025'] = record['bleaching_2025_pbl'] - record['bleaching_2024_annual']
    record['recovery_achieved'] = max(0, record['bleaching_2024_annual'] - record['bleaching_2025_pbl'])
    record['response_2023_to_2024'] = record['bleaching_2024_pbl'] - record['bleaching_2023_annual']
    
    # Additional metrics
    record['temp_instability'] = record['temp_sd_2023'] + record['temp_sd_2024']
    record['cumulative_dhw'] = record['dhw_2023'] + record['dhw_2024']
    record['max_annual_dhw'] = max(record['dhw_2023'], record['dhw_2024'])
    
    analysis_data.append(record)

print(f"Complete analysis dataset: {len(analysis_data)} sites")
print()

# Correlation analysis
print("=== PREDICTIVE POWER ANALYSIS ===")
predictors = {
    '2023_Bleaching': 'bleaching_2023_annual',
    '2024_DHW': 'dhw_2024',
    '2023_DHW': 'dhw_2023', 
    'Cumulative_DHW': 'cumulative_dhw',
    'Max_DHW': 'max_annual_dhw',
    'Temp_Instability': 'temp_instability'
}

correlations = {}
response_values = [record['response_2024_to_2025'] for record in analysis_data]

for name, col in predictors.items():
    predictor_values = [record[col] for record in analysis_data]
    corr = calculate_correlation(predictor_values, response_values)
    if corr is not None:
        correlations[name] = corr

# Sort by absolute correlation
sorted_correlations = sorted(correlations.items(), key=lambda x: abs(x[1]), reverse=True)

print("Correlations with 2024→2025 response magnitude:")
for i, (predictor, corr) in enumerate(sorted_correlations, 1):
    print(f"{i}. {predictor}: r = {corr:.3f}")
print()

# Site classification and analysis
print("=== SITE RESPONSE ANALYSIS ===")

# Summary statistics
total_sites = len(analysis_data)
recovery_sites = sum(1 for record in analysis_data if record['recovery_achieved'] > 5)
worsening_sites = sum(1 for record in analysis_data if record['response_2024_to_2025'] > 5)
stable_sites = sum(1 for record in analysis_data if abs(record['response_2024_to_2025']) <= 5)

print(f"Total analyzed sites: {total_sites}")
print(f"Sites showing recovery (>5% reduction): {recovery_sites} ({recovery_sites/total_sites*100:.1f}%)")
print(f"Sites showing worsening (>5% increase): {worsening_sites} ({worsening_sites/total_sites*100:.1f}%)")
print(f"Stable sites (±5%): {stable_sites} ({stable_sites/total_sites*100:.1f}%)")
print()

# Top performers
print("=== TOP PERFORMING SITES (HIGHEST RECOVERY) ===")
top_recovery = sorted(analysis_data, key=lambda x: x['recovery_achieved'], reverse=True)[:10]

for i, site_data in enumerate(top_recovery, 1):
    print(f"{i}. {site_data['site']}")
    print(f"   Recovery: {site_data['recovery_achieved']:.1f}% reduction ({site_data['bleaching_2024_annual']:.1f}% → {site_data['bleaching_2025_pbl']:.1f}%)")
    print(f"   2023 Bleaching: {site_data['bleaching_2023_annual']:.1f}%, 2024 DHW: {site_data['dhw_2024']:.1f}")
    print()

# Worst performers
print("=== WORST PERFORMING SITES (HIGHEST WORSENING) ===")
worst_response = sorted(analysis_data, key=lambda x: x['response_2024_to_2025'], reverse=True)

worsening_count = 0
for i, site_data in enumerate(worst_response, 1):
    if site_data['response_2024_to_2025'] > 5 and worsening_count < 8:
        worsening_count += 1
        print(f"{worsening_count}. {site_data['site']}")
        print(f"   Worsening: +{site_data['response_2024_to_2025']:.1f}% increase ({site_data['bleaching_2024_annual']:.1f}% → {site_data['bleaching_2025_pbl']:.1f}%)")
        print(f"   2023 Bleaching: {site_data['bleaching_2023_annual']:.1f}%, 2024 DHW: {site_data['dhw_2024']:.1f}")
        print()

# Key insights
print("=== KEY INSIGHTS ===")

# Calculate means
mean_2023_bleaching = statistics.mean(record['bleaching_2023_annual'] for record in analysis_data)
mean_2024_dhw = statistics.mean(record['dhw_2024'] for record in analysis_data)
mean_response = statistics.mean(record['response_2024_to_2025'] for record in analysis_data)

dhw_correlation = correlations.get('2024_DHW', 0)
bleaching_correlation = correlations.get('2023_Bleaching', 0)

print("1. PREDICTIVE RELATIONSHIPS:")
if sorted_correlations:
    best_predictor = sorted_correlations[0]
    print(f"   - Strongest predictor: {best_predictor[0]} (r = {best_predictor[1]:.3f})")
print(f"   - 2024 DHW correlation: {dhw_correlation:.3f}")
print(f"   - 2023 Bleaching correlation: {bleaching_correlation:.3f}")

stronger_predictor = "2024 DHW" if abs(dhw_correlation) > abs(bleaching_correlation) else "2023 Bleaching"
print(f"   - {stronger_predictor} is the stronger predictor")
print()

print("2. THERMAL STRESS EFFECTS:")
high_dhw_sites = sum(1 for record in analysis_data if record['dhw_2024'] > 8)
extreme_dhw_sites = sum(1 for record in analysis_data if record['dhw_2024'] > 12)
print(f"   - Sites with high DHW (>8): {high_dhw_sites} ({high_dhw_sites/total_sites*100:.1f}%)")
print(f"   - Sites with extreme DHW (>12): {extreme_dhw_sites} ({extreme_dhw_sites/total_sites*100:.1f}%)")
print()

print("3. RECOVERY PATTERNS:")
exceptional_recovery = sum(1 for record in analysis_data if record['recovery_achieved'] > 30)
strong_recovery = sum(1 for record in analysis_data if record['recovery_achieved'] > 15)
print(f"   - Exceptional recovery sites (>30% reduction): {exceptional_recovery}")
print(f"   - Strong recovery sites (>15% reduction): {strong_recovery}")
print(f"   - Mean 2023 bleaching extent: {mean_2023_bleaching:.1f}%")
print(f"   - Mean 2024 DHW: {mean_2024_dhw:.1f}")
print(f"   - Mean 2024→2025 response: {mean_response:.1f}%")
print()

# Save results
print("=== SAVING RESULTS ===")
output_file = 'comprehensive_analysis_results.csv'
with open(output_file, 'w', newline='') as file:
    if analysis_data:
        fieldnames = analysis_data[0].keys()
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(analysis_data)

print(f"Saved: {output_file}")

# Create summary by thermal stress level
thermal_stress_summary = defaultdict(list)
for record in analysis_data:
    dhw = record['dhw_2024']
    if dhw > 12:
        stress_level = 'Extreme_Stress'
    elif dhw > 8:
        stress_level = 'High_Stress'
    elif dhw > 4:
        stress_level = 'Moderate_Stress'
    else:
        stress_level = 'Low_Stress'
    
    thermal_stress_summary[stress_level].append(record)

print("\n=== THERMAL STRESS ANALYSIS ===")
for stress_level, records in thermal_stress_summary.items():
    if records:
        n_sites = len(records)
        mean_response = statistics.mean(r['response_2024_to_2025'] for r in records)
        mean_recovery = statistics.mean(r['recovery_achieved'] for r in records)
        recovery_sites = sum(1 for r in records if r['recovery_achieved'] > 5)
        
        print(f"{stress_level}:")
        print(f"  Sites: {n_sites}")
        print(f"  Mean response: {mean_response:.1f}%")
        print(f"  Mean recovery: {mean_recovery:.1f}%")
        print(f"  Sites with recovery: {recovery_sites}")

print()
print("="*60)
print("ANALYSIS PIPELINE COMPLETED SUCCESSFULLY")
print("="*60)