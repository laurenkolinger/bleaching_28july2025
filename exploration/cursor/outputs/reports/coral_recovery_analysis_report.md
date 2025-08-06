# Coral Recovery Analysis Report

## Executive Summary

This analysis examines coral response patterns in 2024 and evaluates the predictive power of previous bleaching events versus environmental variables.

## Key Findings

1. BEST PREDICTIVE MODEL: Site_Characteristics (R² = 0.887)

2. Temperature_2024 is the strongest predictor (R² = 0.826)

3. RECOVERY PATTERNS:

   - Strong_Recovery: Average 2024 response = -5.2%

   - Moderate_Recovery: Average 2024 response = -5.0%

   - Stable: Average 2024 response = -17.6%

   - Decline: Average 2024 response = nan%

4. DHW-BLEACHING CORRELATION: 0.242 (2023 max DHW vs 2023 bleaching)

5. MOST RESPONSIVE REGION: Central_GBR (avg |response| = 13.9%)

## Methodology

- **Data Sources**: Coral bleaching extent/prevalence (33 sites, 2022-2025) and temperature/DHW data (45 sites, 2003-2025)
- **Target Variable**: Coral response in 2024 (change from 2024 Annual to 2025 PBL)
- **Predictor Variables**: Previous bleaching, temperature anomalies, DHW, site characteristics
- **Models**: Random Forest and Linear Regression with standardized features

## Detailed Results

### Predictive Power Comparison

**Previous_Bleaching**:
- Random Forest R²: 0.823
- Linear Regression R²: 0.012
- RMSE: 8.5

**Temperature_2023**:
- Random Forest R²: 0.825
- Linear Regression R²: 0.066
- RMSE: 8.4

**Temperature_2024**:
- Random Forest R²: 0.826
- Linear Regression R²: 0.043
- RMSE: 8.4

**Site_Characteristics**:
- Random Forest R²: 0.887
- Linear Regression R²: 0.020
- RMSE: 6.8

**All_Variables**:
- Random Forest R²: 0.860
- Linear Regression R²: 0.109
- RMSE: 7.5

### Recovery Pattern Analysis

                   count       mean        std   min    25%  50%   75%   max
Recovery_Category                                                           
Strong_Recovery     23.0  -5.156522  17.037894 -44.9 -11.25 -2.6   7.0  25.6
Moderate_Recovery    7.0  -5.042857  28.834173 -44.0 -27.60 -2.7  19.2  28.2
Stable               3.0 -17.633333  28.580646 -49.9 -28.70 -7.5  -1.5   4.5
Decline              0.0        NaN        NaN   NaN    NaN  NaN   NaN   NaN

### Regional Patterns

               Response_2024        Bleaching_2023 Max_DHW_2023
                        mean    std           mean         mean
Region                                                         
Caribbean              -3.75  19.35          41.30         9.37
Central_GBR           -13.95  20.71          76.68        10.00
Coral_Triangle         -3.05  35.85          59.55         7.84
Northern_GBR           -3.79  18.41          68.59         9.24
Red_Sea                -8.77  19.96          36.83         8.90
Southern_GBR           -5.93  28.18          62.50         8.99

## Implications

1. **Predictive Framework**: The analysis reveals which factors best predict coral recovery
2. **Management Insights**: Sites with different characteristics show varying response patterns
3. **Monitoring Priorities**: Results suggest where to focus monitoring efforts
4. **Recovery Potential**: Identifies sites and conditions associated with better recovery

## Data Summary

- Analysis dataset: 33 sites
- Time periods: 2023 Annual → 2024 PBL → 2024 Annual → 2025 PBL
- Response variable range: -49.9% to 28.2%
- Average 2023 bleaching: 58.8%
- Average 2024 response: -6.3%

