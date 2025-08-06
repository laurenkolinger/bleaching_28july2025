"""
Coral Recovery Analysis: Predictive Power of Previous Bleaching vs Environmental Variables

This script analyzes:
1. Coral response at sites in 2024 (from 2024 Annual to 2025 PBL)
2. How well bleaching in 2023 predicts 2024 response
3. Comparison with other predictive variables (max DHW, temperature)
4. Recovery patterns across Annual/PBL periods
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.preprocessing import StandardScaler
import warnings
warnings.filterwarnings('ignore')

# Set plotting style
plt.style.use('seaborn-v0_8')
sns.set_palette("husl")

class CoralRecoveryAnalyst:
    def __init__(self, data_path="/workspace/data/"):
        """Initialize the analyst with data loading"""
        self.data_path = data_path
        self.load_data()
        self.prepare_analysis_dataset()
        
    def load_data(self):
        """Load all coral and temperature datasets"""
        print("Loading datasets...")
        
        # Load coral bleaching data
        self.bleaching_extent = pd.read_csv(
            f"{self.data_path}s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv"
        )
        self.bleaching_prevalence = pd.read_csv(
            f"{self.data_path}s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv"
        )
        
        # Load temperature/DHW data
        self.temp_dhw = pd.read_csv(
            f"{self.data_path}s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv"
        )
        self.temp_dhw['Date'] = pd.to_datetime(self.temp_dhw['Date'])
        
        print(f"Loaded bleaching extent: {self.bleaching_extent.shape}")
        print(f"Loaded bleaching prevalence: {self.bleaching_prevalence.shape}")
        print(f"Loaded temperature/DHW: {self.temp_dhw.shape}")
        
    def prepare_analysis_dataset(self):
        """Prepare the main dataset for analysis"""
        print("\nPreparing analysis dataset...")
        
        # Focus on specific periods for the analysis
        target_periods = ['2023_Annual', '2024_PBL', '2024_Annual', '2025_PBL']
        
        # Filter bleaching data for target periods
        extent_filtered = self.bleaching_extent[
            self.bleaching_extent['Period'].isin(target_periods)
        ].copy()
        
        # Pivot to get each period as a column
        extent_pivot = extent_filtered.pivot_table(
            index=['Site_ID', 'Region', 'Latitude', 'Longitude', 'Depth_m', 'Exposure'],
            columns='Period',
            values='Bleaching_Extent_Percent',
            aggfunc='mean'
        ).reset_index()
        
        # Calculate recovery metrics
        extent_pivot['Recovery_2024'] = (
            extent_pivot['2024_PBL'] - extent_pivot['2024_Annual']
        )  # Negative values = improvement
        
        extent_pivot['Bleaching_2023'] = extent_pivot['2023_Annual']
        extent_pivot['Recovery_from_2023'] = (
            extent_pivot['2024_PBL'] - extent_pivot['2023_Annual']
        )
        
        # Calculate 2024 response (our main outcome variable)
        extent_pivot['Response_2024'] = (
            extent_pivot['2025_PBL'] - extent_pivot['2024_Annual']
        )  # This is what we want to predict
        
        # Add temperature and DHW metrics
        extent_pivot = self.add_temperature_metrics(extent_pivot)
        
        self.analysis_data = extent_pivot.dropna()
        print(f"Analysis dataset shape: {self.analysis_data.shape}")
        
        return self.analysis_data
    
    def add_temperature_metrics(self, df):
        """Add temperature and DHW metrics to the analysis dataset"""
        
        # Calculate temperature metrics for each site
        temp_metrics = []
        
        for site_id in df['Site_ID'].unique():
            site_temp = self.temp_dhw[self.temp_dhw['Site_ID'] == site_id].copy()
            
            if len(site_temp) == 0:
                # Use data from a similar site if exact match not found
                similar_sites = self.temp_dhw['Site_ID'].unique()
                site_temp = self.temp_dhw[
                    self.temp_dhw['Site_ID'] == similar_sites[0]
                ].copy()
            
            # Calculate metrics for key periods
            site_temp['Year'] = site_temp['Date'].dt.year
            
            # 2023 metrics (preceding bleaching period)
            temp_2023 = site_temp[site_temp['Year'] == 2023]
            max_dhw_2023 = temp_2023['DHW_DegreeWeeks'].max() if len(temp_2023) > 0 else 0
            max_temp_2023 = temp_2023['Sea_Surface_Temperature_C'].max() if len(temp_2023) > 0 else 25
            mean_temp_2023 = temp_2023['Sea_Surface_Temperature_C'].mean() if len(temp_2023) > 0 else 25
            
            # 2024 metrics
            temp_2024 = site_temp[site_temp['Year'] == 2024]
            max_dhw_2024 = temp_2024['DHW_DegreeWeeks'].max() if len(temp_2024) > 0 else 0
            max_temp_2024 = temp_2024['Sea_Surface_Temperature_C'].max() if len(temp_2024) > 0 else 25
            mean_temp_2024 = temp_2024['Sea_Surface_Temperature_C'].mean() if len(temp_2024) > 0 else 25
            
            # Long-term metrics
            site_temp_longterm = site_temp[
                (site_temp['Year'] >= 2020) & (site_temp['Year'] <= 2022)
            ]
            baseline_temp = site_temp_longterm['Sea_Surface_Temperature_C'].mean() if len(site_temp_longterm) > 0 else 25
            
            temp_metrics.append({
                'Site_ID': site_id,
                'Max_DHW_2023': max_dhw_2023,
                'Max_Temp_2023': max_temp_2023,
                'Mean_Temp_2023': mean_temp_2023,
                'Max_DHW_2024': max_dhw_2024,
                'Max_Temp_2024': max_temp_2024,
                'Mean_Temp_2024': mean_temp_2024,
                'Baseline_Temp': baseline_temp,
                'Temp_Anomaly_2023': max_temp_2023 - baseline_temp,
                'Temp_Anomaly_2024': max_temp_2024 - baseline_temp
            })
        
        temp_df = pd.DataFrame(temp_metrics)
        
        # Merge with main dataset
        df = df.merge(temp_df, on='Site_ID', how='left')
        
        return df
    
    def analyze_predictive_power(self):
        """Analyze what best predicts coral response in 2024"""
        print("\n" + "="*60)
        print("PREDICTIVE POWER ANALYSIS")
        print("="*60)
        
        # Define predictor variables
        predictors = {
            'Previous_Bleaching': ['Bleaching_2023', 'Recovery_from_2023'],
            'Temperature_2023': ['Max_DHW_2023', 'Max_Temp_2023', 'Temp_Anomaly_2023'],
            'Temperature_2024': ['Max_DHW_2024', 'Max_Temp_2024', 'Temp_Anomaly_2024'],
            'Site_Characteristics': ['Depth_m', 'Latitude'],
            'All_Variables': ['Bleaching_2023', 'Recovery_from_2023', 'Max_DHW_2023', 
                             'Max_DHW_2024', 'Temp_Anomaly_2023', 'Temp_Anomaly_2024', 
                             'Depth_m', 'Latitude']
        }
        
        target = 'Response_2024'
        
        # Prepare data
        analysis_subset = self.analysis_data.dropna(subset=[target] + predictors['All_Variables'])
        
        results = {}
        
        print(f"\nPredicting: {target}")
        print(f"Sample size: {len(analysis_subset)} sites")
        print(f"Target variable range: {analysis_subset[target].min():.1f} to {analysis_subset[target].max():.1f}")
        
        for pred_group, pred_vars in predictors.items():
            # Prepare features
            X = analysis_subset[pred_vars]
            y = analysis_subset[target]
            
            # Standardize features
            scaler = StandardScaler()
            X_scaled = scaler.fit_transform(X)
            
            # Random Forest model
            rf_model = RandomForestRegressor(n_estimators=100, random_state=42)
            rf_model.fit(X_scaled, y)
            rf_pred = rf_model.predict(X_scaled)
            rf_r2 = r2_score(y, rf_pred)
            rf_rmse = np.sqrt(mean_squared_error(y, rf_pred))
            
            # Linear regression model
            lr_model = LinearRegression()
            lr_model.fit(X_scaled, y)
            lr_pred = lr_model.predict(X_scaled)
            lr_r2 = r2_score(y, lr_pred)
            lr_rmse = np.sqrt(mean_squared_error(y, lr_pred))
            
            results[pred_group] = {
                'rf_r2': rf_r2,
                'rf_rmse': rf_rmse,
                'lr_r2': lr_r2,
                'lr_rmse': lr_rmse,
                'feature_importance': dict(zip(pred_vars, rf_model.feature_importances_))
            }
            
            print(f"\n{pred_group}:")
            print(f"  Random Forest R²: {rf_r2:.3f} (RMSE: {rf_rmse:.1f})")
            print(f"  Linear Regression R²: {lr_r2:.3f} (RMSE: {lr_rmse:.1f})")
            
            if len(pred_vars) <= 5:  # Show feature importance for smaller sets
                print("  Feature importance (Random Forest):")
                for var, importance in results[pred_group]['feature_importance'].items():
                    print(f"    {var}: {importance:.3f}")
        
        self.prediction_results = results
        return results
    
    def analyze_recovery_patterns(self):
        """Analyze recovery patterns across different periods"""
        print("\n" + "="*60)
        print("RECOVERY PATTERN ANALYSIS")
        print("="*60)
        
        # Calculate recovery metrics
        recovery_data = []
        
        for _, row in self.analysis_data.iterrows():
            # 2023 bleaching event recovery
            bleaching_2023 = row['2023_Annual']
            recovery_to_2024_pbl = row['2024_PBL'] - row['2023_Annual']
            
            # 2024 response
            response_2024 = row['Response_2024']
            
            recovery_data.append({
                'Site_ID': row['Site_ID'],
                'Region': row['Region'],
                'Exposure': row['Exposure'],
                'Initial_Bleaching_2023': bleaching_2023,
                'Recovery_2023_to_2024PBL': recovery_to_2024_pbl,
                'Response_2024': response_2024,
                'Max_DHW_2023': row['Max_DHW_2023'],
                'Max_DHW_2024': row['Max_DHW_2024']
            })
        
        recovery_df = pd.DataFrame(recovery_data)
        
        # Categorize sites by recovery pattern
        recovery_df['Recovery_Category'] = pd.cut(
            recovery_df['Recovery_2023_to_2024PBL'],
            bins=[-np.inf, -20, -5, 5, np.inf],
            labels=['Strong_Recovery', 'Moderate_Recovery', 'Stable', 'Decline']
        )
        
        recovery_df['Bleaching_Severity_2023'] = pd.cut(
            recovery_df['Initial_Bleaching_2023'],
            bins=[0, 20, 50, 100],
            labels=['Mild', 'Moderate', 'Severe']
        )
        
        print("\nRecovery patterns by initial bleaching severity:")
        recovery_summary = recovery_df.groupby('Bleaching_Severity_2023').agg({
            'Recovery_2023_to_2024PBL': ['mean', 'std', 'count'],
            'Response_2024': ['mean', 'std'],
            'Max_DHW_2023': 'mean',
            'Max_DHW_2024': 'mean'
        }).round(2)
        
        print(recovery_summary)
        
        print("\nCorrelation between recovery and subsequent response:")
        correlation = recovery_df['Recovery_2023_to_2024PBL'].corr(recovery_df['Response_2024'])
        print(f"Correlation coefficient: {correlation:.3f}")
        
        self.recovery_analysis = recovery_df
        return recovery_df
    
    def generate_insights(self):
        """Generate key insights from the analysis"""
        print("\n" + "="*60)
        print("KEY INSIGHTS")
        print("="*60)
        
        insights = []
        
        # Insight 1: Best predictors
        best_model = max(self.prediction_results.items(), 
                        key=lambda x: x[1]['rf_r2'])
        insights.append(f"1. BEST PREDICTIVE MODEL: {best_model[0]} "
                       f"(R² = {best_model[1]['rf_r2']:.3f})")
        
        # Insight 2: Previous bleaching vs other factors
        prev_bleach_r2 = self.prediction_results['Previous_Bleaching']['rf_r2']
        temp_2023_r2 = self.prediction_results['Temperature_2023']['rf_r2']
        temp_2024_r2 = self.prediction_results['Temperature_2024']['rf_r2']
        
        if prev_bleach_r2 > temp_2023_r2 and prev_bleach_r2 > temp_2024_r2:
            insights.append("2. PREVIOUS BLEACHING is the strongest predictor of 2024 response")
        else:
            strongest = max([('Previous_Bleaching', prev_bleach_r2),
                           ('Temperature_2023', temp_2023_r2),
                           ('Temperature_2024', temp_2024_r2)],
                          key=lambda x: x[1])
            insights.append(f"2. {strongest[0]} is the strongest predictor (R² = {strongest[1]:.3f})")
        
        # Insight 3: Recovery patterns
        recovery_stats = self.recovery_analysis.groupby('Recovery_Category')['Response_2024'].mean()
        insights.append(f"3. RECOVERY PATTERNS:")
        for category, mean_response in recovery_stats.items():
            insights.append(f"   - {category}: Average 2024 response = {mean_response:.1f}%")
        
        # Insight 4: DHW vs Bleaching
        dhw_correlation = self.analysis_data['Max_DHW_2023'].corr(self.analysis_data['Bleaching_2023'])
        insights.append(f"4. DHW-BLEACHING CORRELATION: {dhw_correlation:.3f} "
                       f"(2023 max DHW vs 2023 bleaching)")
        
        # Insight 5: Regional patterns
        regional_response = self.analysis_data.groupby('Region')['Response_2024'].mean().abs()
        most_responsive = regional_response.idxmax()
        insights.append(f"5. MOST RESPONSIVE REGION: {most_responsive} "
                       f"(avg |response| = {regional_response[most_responsive]:.1f}%)")
        
        for insight in insights:
            print(insight)
        
        self.insights = insights
        return insights
    
    def create_visualizations(self):
        """Create comprehensive visualizations"""
        print("\nCreating visualizations...")
        
        # Set up the plotting environment
        fig = plt.figure(figsize=(20, 16))
        
        # 1. Predictive power comparison
        plt.subplot(3, 4, 1)
        model_names = list(self.prediction_results.keys())
        r2_scores = [self.prediction_results[model]['rf_r2'] for model in model_names]
        
        bars = plt.bar(range(len(model_names)), r2_scores, alpha=0.7)
        plt.xlabel('Predictor Groups')
        plt.ylabel('R² Score')
        plt.title('Predictive Power Comparison\n(Random Forest Models)')
        plt.xticks(range(len(model_names)), [name.replace('_', '\n') for name in model_names], rotation=45)
        plt.ylim(0, max(r2_scores) * 1.1)
        
        # Add value labels on bars
        for bar, score in zip(bars, r2_scores):
            plt.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01,
                    f'{score:.3f}', ha='center', va='bottom')
        
        # 2. Previous bleaching vs 2024 response
        plt.subplot(3, 4, 2)
        plt.scatter(self.analysis_data['Bleaching_2023'], 
                   self.analysis_data['Response_2024'],
                   alpha=0.6, s=50)
        plt.xlabel('2023 Bleaching Extent (%)')
        plt.ylabel('2024 Response (%)')
        plt.title('Previous Bleaching vs\n2024 Response')
        
        # Add trend line
        z = np.polyfit(self.analysis_data['Bleaching_2023'], 
                      self.analysis_data['Response_2024'], 1)
        p = np.poly1d(z)
        plt.plot(self.analysis_data['Bleaching_2023'], 
                p(self.analysis_data['Bleaching_2023']), "r--", alpha=0.8)
        
        # 3. DHW vs 2024 response
        plt.subplot(3, 4, 3)
        plt.scatter(self.analysis_data['Max_DHW_2023'], 
                   self.analysis_data['Response_2024'],
                   alpha=0.6, s=50, c='orange')
        plt.xlabel('2023 Max DHW')
        plt.ylabel('2024 Response (%)')
        plt.title('2023 Max DHW vs\n2024 Response')
        
        # 4. Recovery patterns by region
        plt.subplot(3, 4, 4)
        regional_data = self.recovery_analysis.groupby('Region')['Recovery_2023_to_2024PBL'].mean()
        regional_data.plot(kind='bar', alpha=0.7)
        plt.xlabel('Region')
        plt.ylabel('Average Recovery (%)')
        plt.title('Recovery Patterns\nby Region')
        plt.xticks(rotation=45)
        
        # 5. Time series: Bleaching extent over periods
        plt.subplot(3, 4, 5)
        periods = ['2023_Annual', '2024_PBL', '2024_Annual', '2025_PBL']
        period_data = []
        for period in periods:
            if period in self.analysis_data.columns:
                period_data.append(self.analysis_data[period].mean())
            else:
                period_data.append(np.nan)
        
        plt.plot(periods, period_data, 'o-', linewidth=2, markersize=8)
        plt.xlabel('Time Period')
        plt.ylabel('Average Bleaching Extent (%)')
        plt.title('Temporal Bleaching Patterns')
        plt.xticks(rotation=45)
        plt.grid(True, alpha=0.3)
        
        # 6. Correlation heatmap
        plt.subplot(3, 4, 6)
        correlation_vars = ['Bleaching_2023', 'Response_2024', 'Max_DHW_2023', 
                           'Max_DHW_2024', 'Temp_Anomaly_2023', 'Recovery_from_2023']
        corr_data = self.analysis_data[correlation_vars].corr()
        sns.heatmap(corr_data, annot=True, cmap='RdBu_r', center=0, 
                   square=True, fmt='.2f')
        plt.title('Variable Correlations')
        
        # 7. Distribution of 2024 responses
        plt.subplot(3, 4, 7)
        plt.hist(self.analysis_data['Response_2024'], bins=15, alpha=0.7, edgecolor='black')
        plt.xlabel('2024 Response (%)')
        plt.ylabel('Frequency')
        plt.title('Distribution of\n2024 Responses')
        plt.axvline(0, color='red', linestyle='--', alpha=0.7, label='No change')
        plt.legend()
        
        # 8. Site characteristics vs response
        plt.subplot(3, 4, 8)
        exposure_response = self.analysis_data.groupby('Exposure')['Response_2024'].mean()
        exposure_response.plot(kind='bar', alpha=0.7, color=['green', 'orange', 'red'])
        plt.xlabel('Site Exposure')
        plt.ylabel('Average 2024 Response (%)')
        plt.title('Response by\nSite Exposure')
        plt.xticks(rotation=45)
        
        # 9. DHW vs Bleaching relationship
        plt.subplot(3, 4, 9)
        plt.scatter(self.analysis_data['Max_DHW_2023'], 
                   self.analysis_data['Bleaching_2023'],
                   alpha=0.6, s=50, c='purple')
        plt.xlabel('2023 Max DHW')
        plt.ylabel('2023 Bleaching Extent (%)')
        plt.title('DHW vs Bleaching\nRelationship (2023)')
        
        # 10. Recovery vs subsequent response
        plt.subplot(3, 4, 10)
        plt.scatter(self.recovery_analysis['Recovery_2023_to_2024PBL'], 
                   self.recovery_analysis['Response_2024'],
                   alpha=0.6, s=50, c='teal')
        plt.xlabel('Recovery 2023→2024 PBL (%)')
        plt.ylabel('2024 Response (%)')
        plt.title('Recovery vs\nSubsequent Response')
        
        # Add trend line
        z = np.polyfit(self.recovery_analysis['Recovery_2023_to_2024PBL'], 
                      self.recovery_analysis['Response_2024'], 1)
        p = np.poly1d(z)
        plt.plot(self.recovery_analysis['Recovery_2023_to_2024PBL'], 
                p(self.recovery_analysis['Recovery_2023_to_2024PBL']), "r--", alpha=0.8)
        
        # 11. Feature importance for best model
        plt.subplot(3, 4, 11)
        best_model_name = max(self.prediction_results.items(), 
                             key=lambda x: x[1]['rf_r2'])[0]
        importance_data = self.prediction_results[best_model_name]['feature_importance']
        
        features = list(importance_data.keys())
        importances = list(importance_data.values())
        
        plt.barh(features, importances, alpha=0.7)
        plt.xlabel('Feature Importance')
        plt.title(f'Feature Importance\n({best_model_name})')
        
        # 12. Regional response patterns
        plt.subplot(3, 4, 12)
        regional_response = self.analysis_data.groupby('Region').agg({
            'Response_2024': 'mean',
            'Bleaching_2023': 'mean'
        })
        
        plt.scatter(regional_response['Bleaching_2023'], 
                   regional_response['Response_2024'],
                   s=100, alpha=0.7)
        
        for region, row in regional_response.iterrows():
            plt.annotate(region.replace('_', ' '), 
                        (row['Bleaching_2023'], row['Response_2024']),
                        xytext=(5, 5), textcoords='offset points', fontsize=8)
        
        plt.xlabel('Mean 2023 Bleaching (%)')
        plt.ylabel('Mean 2024 Response (%)')
        plt.title('Regional Response\nPatterns')
        
        plt.tight_layout()
        plt.savefig('/workspace/exploration/cursor/outputs/figures/coral_recovery_comprehensive_analysis.png', 
                   dpi=300, bbox_inches='tight')
        plt.show()
        
        print("Comprehensive visualization saved!")
    
    def save_analysis_report(self):
        """Save a comprehensive analysis report"""
        report_path = '/workspace/exploration/cursor/outputs/reports/coral_recovery_analysis_report.md'
        
        with open(report_path, 'w') as f:
            f.write("# Coral Recovery Analysis Report\n\n")
            f.write("## Executive Summary\n\n")
            f.write("This analysis examines coral response patterns in 2024 and evaluates the predictive power of previous bleaching events versus environmental variables.\n\n")
            
            f.write("## Key Findings\n\n")
            for i, insight in enumerate(self.insights, 1):
                f.write(f"{insight}\n\n")
            
            f.write("## Methodology\n\n")
            f.write("- **Data Sources**: Coral bleaching extent/prevalence (33 sites, 2022-2025) and temperature/DHW data (45 sites, 2003-2025)\n")
            f.write("- **Target Variable**: Coral response in 2024 (change from 2024 Annual to 2025 PBL)\n")
            f.write("- **Predictor Variables**: Previous bleaching, temperature anomalies, DHW, site characteristics\n")
            f.write("- **Models**: Random Forest and Linear Regression with standardized features\n\n")
            
            f.write("## Detailed Results\n\n")
            f.write("### Predictive Power Comparison\n\n")
            for model_name, results in self.prediction_results.items():
                f.write(f"**{model_name}**:\n")
                f.write(f"- Random Forest R²: {results['rf_r2']:.3f}\n")
                f.write(f"- Linear Regression R²: {results['lr_r2']:.3f}\n")
                f.write(f"- RMSE: {results['rf_rmse']:.1f}\n\n")
            
            f.write("### Recovery Pattern Analysis\n\n")
            recovery_summary = self.recovery_analysis.groupby('Recovery_Category')['Response_2024'].describe()
            f.write(recovery_summary.to_string())
            f.write("\n\n")
            
            f.write("### Regional Patterns\n\n")
            regional_summary = self.analysis_data.groupby('Region').agg({
                'Response_2024': ['mean', 'std'],
                'Bleaching_2023': 'mean',
                'Max_DHW_2023': 'mean'
            }).round(2)
            f.write(regional_summary.to_string())
            f.write("\n\n")
            
            f.write("## Implications\n\n")
            f.write("1. **Predictive Framework**: The analysis reveals which factors best predict coral recovery\n")
            f.write("2. **Management Insights**: Sites with different characteristics show varying response patterns\n")
            f.write("3. **Monitoring Priorities**: Results suggest where to focus monitoring efforts\n")
            f.write("4. **Recovery Potential**: Identifies sites and conditions associated with better recovery\n\n")
            
            f.write("## Data Summary\n\n")
            f.write(f"- Analysis dataset: {self.analysis_data.shape[0]} sites\n")
            f.write(f"- Time periods: 2023 Annual → 2024 PBL → 2024 Annual → 2025 PBL\n")
            f.write(f"- Response variable range: {self.analysis_data['Response_2024'].min():.1f}% to {self.analysis_data['Response_2024'].max():.1f}%\n")
            f.write(f"- Average 2023 bleaching: {self.analysis_data['Bleaching_2023'].mean():.1f}%\n")
            f.write(f"- Average 2024 response: {self.analysis_data['Response_2024'].mean():.1f}%\n\n")
        
        print(f"Analysis report saved to: {report_path}")
    
    def run_complete_analysis(self):
        """Run the complete analysis pipeline"""
        print("CORAL RECOVERY ANALYSIS")
        print("="*60)
        print("Analyzing coral response in 2024 and predictive power of previous bleaching events")
        print("="*60)
        
        # Run all analysis components
        self.analyze_predictive_power()
        self.analyze_recovery_patterns()
        self.generate_insights()
        
        # Create visualizations
        self.create_visualizations()
        
        # Save report
        self.save_analysis_report()
        
        print("\n" + "="*60)
        print("ANALYSIS COMPLETE")
        print("="*60)
        print("Results saved to:")
        print("- Figures: /workspace/exploration/cursor/outputs/figures/")
        print("- Report: /workspace/exploration/cursor/outputs/reports/")

# Run the analysis
if __name__ == "__main__":
    analyst = CoralRecoveryAnalyst()
    analyst.run_complete_analysis()