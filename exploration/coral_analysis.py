#!/usr/bin/env python3
"""
Comprehensive Coral Bleaching Response Analysis
Analysis Period: 2023-2025 Bleaching Events
Focus: 2024 Annual → 2025 PBL Response Prediction
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
import warnings
warnings.filterwarnings('ignore')

print("="*60)
print("CORAL BLEACHING ANALYSIS PIPELINE")
print("Analysis Period: 2023-2025")
print("Objective: Evaluate 2024 coral response prediction")
print("="*60)
print()

# Load datasets
print("Loading datasets...")
extent_data = pd.read_csv("s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv")
prevalence_data = pd.read_csv("s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv")
mortality_data = pd.read_csv("s3pt1_coralcondition_mortalityprevalence_33sites_2022_2025.csv")
temp_data = pd.read_csv("s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")

print(f"Extent data: {extent_data.shape}")
print(f"Prevalence data: {prevalence_data.shape}")
print(f"Mortality data: {mortality_data.shape}")
print(f"Temperature data: {temp_data.shape}")
print()

# Calculate site-level means
print("Calculating site-level means...")
extent_means = extent_data.groupby(['site', 'year', 'period']).agg({
    'ext_bleached': 'mean',
    'ext_verypale': 'mean', 
    'ext_anybleaching': 'mean',
    'ext_nobleaching': 'mean'
}).reset_index()

prevalence_means = prevalence_data.groupby(['site', 'year', 'period']).agg({
    'ncolonies': 'mean',
    'prev_bleached': 'mean',
    'prev_verypale': 'mean',
    'prev_anybleaching': 'mean',
    'prev_nobleaching': 'mean'
}).reset_index()

print(f"Processed extent data: {extent_means.shape}")
print(f"Processed prevalence data: {prevalence_means.shape}")
print()

# Temperature analysis for 2023-2024
print("Analyzing temperature patterns...")
temp_2023_2024 = temp_data[(temp_data['year'].isin([2023, 2024])) & 
                           (temp_data['dhw'].notna()) & 
                           (temp_data['weekly_max_temp'].notna())]

temp_metrics = temp_2023_2024.groupby(['site', 'year']).agg({
    'dhw': ['max', 'sum'],
    'weekly_max_temp': ['max', 'mean', 'std'],
    'week': 'count'
}).reset_index()

# Flatten column names
temp_metrics.columns = ['site', 'year', 'max_dhw', 'total_dhw', 'max_temp', 'mean_temp', 'temp_sd', 'weeks_data']
temp_metrics['temp_range'] = temp_metrics.groupby(['site', 'year'])['max_temp'].transform('max') - \
                            temp_2023_2024.groupby(['site', 'year'])['weekly_max_temp'].min().values

print(f"Temperature metrics calculated for {temp_metrics['site'].nunique()} sites")
print()

# Extract key timepoints for analysis
print("Extracting key timepoints...")
key_timepoints = extent_means[
    ((extent_means['year'] == 2023) & (extent_means['period'] == 'Annual')) |
    ((extent_means['year'] == 2024) & (extent_means['period'] == 'PBL')) |
    ((extent_means['year'] == 2024) & (extent_means['period'] == 'Annual')) |
    ((extent_means['year'] == 2025) & (extent_means['period'] == 'PBL'))
].copy()

key_timepoints['timepoint'] = key_timepoints['year'].astype(str) + '_' + key_timepoints['period']
print(f"Key timepoints extracted: {key_timepoints.shape}")

# Check data availability for each timepoint
timepoint_counts = key_timepoints['timepoint'].value_counts()
print("Data availability by timepoint:")
for tp, count in timepoint_counts.items():
    print(f"  {tp}: {count} sites")
print()

# Create comprehensive analysis dataset
print("Creating comprehensive analysis dataset...")

# Get each timepoint separately
timepoint_2023_annual = key_timepoints[key_timepoints['timepoint'] == '2023_Annual'][['site', 'ext_anybleaching']].rename(columns={'ext_anybleaching': 'bleaching_2023_annual'})
timepoint_2024_pbl = key_timepoints[key_timepoints['timepoint'] == '2024_PBL'][['site', 'ext_anybleaching']].rename(columns={'ext_anybleaching': 'bleaching_2024_pbl'})
timepoint_2024_annual = key_timepoints[key_timepoints['timepoint'] == '2024_Annual'][['site', 'ext_anybleaching']].rename(columns={'ext_anybleaching': 'bleaching_2024_annual'})
timepoint_2025_pbl = key_timepoints[key_timepoints['timepoint'] == '2025_PBL'][['site', 'ext_anybleaching']].rename(columns={'ext_anybleaching': 'bleaching_2025_pbl'})

# Merge timepoints
analysis_data = timepoint_2023_annual.merge(timepoint_2024_pbl, on='site', how='inner')
analysis_data = analysis_data.merge(timepoint_2024_annual, on='site', how='inner')
analysis_data = analysis_data.merge(timepoint_2025_pbl, on='site', how='inner')

# Add temperature data
temp_2023 = temp_metrics[temp_metrics['year'] == 2023][['site', 'max_dhw', 'temp_sd']].rename(columns={'max_dhw': 'dhw_2023', 'temp_sd': 'temp_sd_2023'})
temp_2024 = temp_metrics[temp_metrics['year'] == 2024][['site', 'max_dhw', 'temp_sd']].rename(columns={'max_dhw': 'dhw_2024', 'temp_sd': 'temp_sd_2024'})

analysis_data = analysis_data.merge(temp_2023, on='site', how='left')
analysis_data = analysis_data.merge(temp_2024, on='site', how='left')

# Calculate response metrics
analysis_data['response_2024_to_2025'] = analysis_data['bleaching_2025_pbl'] - analysis_data['bleaching_2024_annual']
analysis_data['recovery_achieved'] = np.maximum(0, analysis_data['bleaching_2024_annual'] - analysis_data['bleaching_2025_pbl'])
analysis_data['response_2023_to_2024'] = analysis_data['bleaching_2024_pbl'] - analysis_data['bleaching_2023_annual']

# Temperature instability
analysis_data['temp_instability'] = analysis_data['temp_sd_2023'].fillna(0) + analysis_data['temp_sd_2024'].fillna(0)
analysis_data['cumulative_dhw'] = analysis_data['dhw_2023'].fillna(0) + analysis_data['dhw_2024'].fillna(0)
analysis_data['max_annual_dhw'] = np.maximum(analysis_data['dhw_2023'].fillna(0), analysis_data['dhw_2024'].fillna(0))

print(f"Complete analysis dataset: {analysis_data.shape[0]} sites")
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
for name, col in predictors.items():
    if col in analysis_data.columns:
        corr = analysis_data[col].corr(analysis_data['response_2024_to_2025'])
        correlations[name] = corr

# Sort by absolute correlation
sorted_correlations = sorted(correlations.items(), key=lambda x: abs(x[1]), reverse=True)

print("Correlations with 2024→2025 response magnitude:")
for i, (predictor, corr) in enumerate(sorted_correlations, 1):
    print(f"{i}. {predictor}: r = {corr:.3f}")
print()

# Linear regression models
print("Model Performance Comparison:")
models = {}

# Model 1: 2023 Bleaching only
valid_data = analysis_data.dropna(subset=['bleaching_2023_annual', 'response_2024_to_2025'])
if len(valid_data) > 0:
    X1 = valid_data[['bleaching_2023_annual']]
    y = valid_data['response_2024_to_2025']
    model1 = LinearRegression().fit(X1, y)
    r2_1 = r2_score(y, model1.predict(X1))
    models['2023_Bleaching_Only'] = r2_1

# Model 2: 2024 DHW only
valid_data = analysis_data.dropna(subset=['dhw_2024', 'response_2024_to_2025'])
if len(valid_data) > 0:
    X2 = valid_data[['dhw_2024']]
    y = valid_data['response_2024_to_2025']
    model2 = LinearRegression().fit(X2, y)
    r2_2 = r2_score(y, model2.predict(X2))
    models['2024_DHW_Only'] = r2_2

# Model 3: Combined
valid_data = analysis_data.dropna(subset=['bleaching_2023_annual', 'dhw_2024', 'response_2024_to_2025'])
if len(valid_data) > 0:
    X3 = valid_data[['bleaching_2023_annual', 'dhw_2024']]
    y = valid_data['response_2024_to_2025']
    model3 = LinearRegression().fit(X3, y)
    r2_3 = r2_score(y, model3.predict(X3))
    models['Combined'] = r2_3

for model_name, r2 in sorted(models.items(), key=lambda x: x[1], reverse=True):
    print(f"{model_name}: R² = {r2:.4f}")
print()

# Site classification and analysis
print("=== SITE RESPONSE ANALYSIS ===")

# Classify sites by response
analysis_data['response_category'] = pd.cut(
    analysis_data['recovery_achieved'],
    bins=[-float('inf'), 0, 5, 15, 30, float('inf')],
    labels=['Worsening', 'Stable', 'Moderate_Recovery', 'Strong_Recovery', 'Exceptional_Recovery']
)

# Thermal stress classification
analysis_data['thermal_stress'] = pd.cut(
    analysis_data['dhw_2024'].fillna(0),
    bins=[0, 4, 8, 12, float('inf')],
    labels=['Low_Stress', 'Moderate_Stress', 'High_Stress', 'Extreme_Stress']
)

# Summary statistics
total_sites = len(analysis_data)
recovery_sites = (analysis_data['recovery_achieved'] > 5).sum()
worsening_sites = (analysis_data['response_2024_to_2025'] > 5).sum()
stable_sites = (abs(analysis_data['response_2024_to_2025']) <= 5).sum()

print(f"Total analyzed sites: {total_sites}")
print(f"Sites showing recovery (>5% reduction): {recovery_sites} ({recovery_sites/total_sites*100:.1f}%)")
print(f"Sites showing worsening (>5% increase): {worsening_sites} ({worsening_sites/total_sites*100:.1f}%)")
print(f"Stable sites (±5%): {stable_sites} ({stable_sites/total_sites*100:.1f}%)")
print()

# Top performers
print("=== TOP PERFORMING SITES (HIGHEST RECOVERY) ===")
top_recovery = analysis_data.nlargest(10, 'recovery_achieved')

for i, (idx, site_data) in enumerate(top_recovery.iterrows(), 1):
    print(f"{i}. {site_data['site']}")
    print(f"   Recovery: {site_data['recovery_achieved']:.1f}% reduction ({site_data['bleaching_2024_annual']:.1f}% → {site_data['bleaching_2025_pbl']:.1f}%)")
    print(f"   2023 Bleaching: {site_data['bleaching_2023_annual']:.1f}%, 2024 DHW: {site_data['dhw_2024']:.1f}")
    print()

# Worst performers
print("=== WORST PERFORMING SITES (HIGHEST WORSENING) ===")
worst_response = analysis_data.nlargest(8, 'response_2024_to_2025')

for i, (idx, site_data) in enumerate(worst_response.iterrows(), 1):
    if site_data['response_2024_to_2025'] > 5:  # Only show actual worsening
        print(f"{i}. {site_data['site']}")
        print(f"   Worsening: +{site_data['response_2024_to_2025']:.1f}% increase ({site_data['bleaching_2024_annual']:.1f}% → {site_data['bleaching_2025_pbl']:.1f}%)")
        print(f"   2023 Bleaching: {site_data['bleaching_2023_annual']:.1f}%, 2024 DHW: {site_data['dhw_2024']:.1f}")
        print()

# Key numerical insights
print("=== KEY INSIGHTS ===")
mean_2023_bleaching = analysis_data['bleaching_2023_annual'].mean()
mean_2024_dhw = analysis_data['dhw_2024'].mean()
mean_response = analysis_data['response_2024_to_2025'].mean()

dhw_correlation = correlations.get('2024_DHW', 0)
bleaching_correlation = correlations.get('2023_Bleaching', 0)

print("1. PREDICTIVE RELATIONSHIPS:")
best_predictor = sorted_correlations[0]
print(f"   - Strongest predictor: {best_predictor[0]} (r = {best_predictor[1]:.3f})")
print(f"   - 2024 DHW correlation: {dhw_correlation:.3f}")
print(f"   - 2023 Bleaching correlation: {bleaching_correlation:.3f}")

stronger_predictor = "2024 DHW" if abs(dhw_correlation) > abs(bleaching_correlation) else "2023 Bleaching"
print(f"   - {stronger_predictor} is the stronger predictor")
print()

print("2. THERMAL STRESS EFFECTS:")
high_dhw_sites = (analysis_data['dhw_2024'] > 8).sum()
extreme_dhw_sites = (analysis_data['dhw_2024'] > 12).sum()
print(f"   - Sites with high DHW (>8): {high_dhw_sites} ({high_dhw_sites/total_sites*100:.1f}%)")
print(f"   - Sites with extreme DHW (>12): {extreme_dhw_sites} ({extreme_dhw_sites/total_sites*100:.1f}%)")
print()

print("3. RECOVERY PATTERNS:")
exceptional_recovery = (analysis_data['recovery_achieved'] > 30).sum()
strong_recovery = (analysis_data['recovery_achieved'] > 15).sum()
print(f"   - Exceptional recovery sites (>30% reduction): {exceptional_recovery}")
print(f"   - Strong recovery sites (>15% reduction): {strong_recovery}")
print()

# Save results
print("=== SAVING RESULTS ===")
analysis_data.to_csv('comprehensive_analysis_results.csv', index=False)
print("Saved: comprehensive_analysis_results.csv")

# Create summary statistics by category
category_summary = analysis_data.groupby('response_category').agg({
    'bleaching_2023_annual': 'mean',
    'dhw_2024': 'mean',
    'response_2024_to_2025': 'mean',
    'recovery_achieved': 'mean',
    'site': 'count'
}).round(2)
category_summary.columns = ['Mean_2023_Bleaching', 'Mean_2024_DHW', 'Mean_Response', 'Mean_Recovery', 'N_Sites']
category_summary.to_csv('response_category_summary.csv')
print("Saved: response_category_summary.csv")

print()
print("="*60)
print("ANALYSIS PIPELINE COMPLETED SUCCESSFULLY")
print("="*60)