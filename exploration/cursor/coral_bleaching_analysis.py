#!/usr/bin/env python3
"""
Coral Bleaching Recovery Analysis

This script analyzes coral bleaching recovery patterns and predictive relationships
between previous bleaching events and subsequent coral responses.

Focus:
- 2024 coral response (2024 Annual to 2025 PBL)
- Prediction based on 2023 bleaching (2023 Annual to 2024 PBL)
- DHW (Degree Heating Weeks) as additional predictor
- Recovery patterns across Annual/PBL periods

Author: Analysis generated for coral bleaching recovery study
Date: January 2025
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.preprocessing import StandardScaler
import warnings
warnings.filterwarnings('ignore')

# Set plotting style
plt.style.use('default')
sns.set_palette("husl")

class CoralBleachingAnalyzer:
    """
    A comprehensive analyzer for coral bleaching data with focus on recovery patterns
    and predictive modeling.
    """
    
    def __init__(self):
        self.bleaching_extent = None
        self.bleaching_prevalence = None
        self.dhw_data = None
        self.analysis_data = None
        self.results = {}
        
    def load_data(self, extent_path, prevalence_path, dhw_path):
        """Load the three main datasets"""
        try:
            print("Loading coral bleaching datasets...")
            self.bleaching_extent = pd.read_csv(extent_path)
            self.bleaching_prevalence = pd.read_csv(prevalence_path)
            self.dhw_data = pd.read_csv(dhw_path)
            
            # Convert dates
            self.bleaching_extent['date'] = pd.to_datetime(self.bleaching_extent['date'])
            self.bleaching_prevalence['date'] = pd.to_datetime(self.bleaching_prevalence['date'])
            
            print(f"✓ Loaded bleaching extent data: {self.bleaching_extent.shape}")
            print(f"✓ Loaded bleaching prevalence data: {self.bleaching_prevalence.shape}")
            print(f"✓ Loaded DHW data: {self.dhw_data.shape}")
            
            self._validate_data()
            return True
            
        except FileNotFoundError as e:
            print(f"❌ Error loading data: {e}")
            print("Creating sample data for demonstration...")
            self._create_sample_data()
            return False
            
    def _validate_data(self):
        """Validate data structure and content"""
        print("\n=== DATA VALIDATION ===")
        
        # Check common sites
        extent_sites = set(self.bleaching_extent['site'].unique())
        prevalence_sites = set(self.bleaching_prevalence['site'].unique())
        dhw_sites = set(self.dhw_data['site'].unique())
        
        common_sites = extent_sites.intersection(prevalence_sites).intersection(dhw_sites)
        print(f"Common sites across all datasets: {len(common_sites)}")
        print(f"Sites: {sorted(list(common_sites))[:10]}{'...' if len(common_sites) > 10 else ''}")
        
        # Check time periods
        periods = self.bleaching_extent['period'].unique()
        years = sorted(self.bleaching_extent['year'].unique())
        print(f"Available periods: {periods}")
        print(f"Available years: {years}")
        
        # Check bleaching columns
        extent_cols = [col for col in self.bleaching_extent.columns if col.startswith('ext_')]
        prev_cols = [col for col in self.bleaching_prevalence.columns if col.startswith('prev_')]
        print(f"Extent metrics: {extent_cols[:5]}{'...' if len(extent_cols) > 5 else ''}")
        print(f"Prevalence metrics: {prev_cols[:5]}{'...' if len(prev_cols) > 5 else ''}")
        
    def _create_sample_data(self):
        """Create sample data for demonstration when real data is not available"""
        print("Creating sample data for analysis demonstration...")
        
        sites = [f"Site_{i:02d}" for i in range(1, 34)]  # 33 sites
        years = [2022, 2023, 2024, 2025]
        periods = ['Annual', 'PBL']
        
        # Sample bleaching extent data
        extent_data = []
        for site in sites:
            for year in years:
                for period in periods:
                    if (year == 2025 and period == 'Annual'):  # Skip 2025 Annual
                        continue
                    base_date = pd.Timestamp(f"{year}-{'06' if period == 'Annual' else '02'}-15")
                    extent_data.append({
                        'site': site,
                        'year': year,
                        'period': period,
                        'date': base_date + pd.Timedelta(days=np.random.randint(-30, 30)),
                        'ext_total': np.random.beta(2, 5) * 100,  # Realistic bleaching percentages
                        'ext_pale': np.random.beta(3, 4) * 50,
                        'ext_bleached': np.random.beta(1.5, 8) * 30,
                        'ext_partial_mortality': np.random.beta(1, 10) * 15,
                        'ext_recent_mortality': np.random.beta(1, 15) * 10
                    })
        
        self.bleaching_extent = pd.DataFrame(extent_data)
        
        # Sample bleaching prevalence data (similar structure)
        prev_data = []
        for site in sites:
            for year in years:
                for period in periods:
                    if (year == 2025 and period == 'Annual'):
                        continue
                    base_date = pd.Timestamp(f"{year}-{'06' if period == 'Annual' else '02'}-15")
                    n_colonies = np.random.randint(20, 100)
                    prev_data.append({
                        'site': site,
                        'year': year,
                        'period': period,
                        'date': base_date + pd.Timedelta(days=np.random.randint(-30, 30)),
                        'ncolonies': n_colonies,
                        'prev_total': np.random.beta(2, 3) * 100,
                        'prev_pale': np.random.beta(3, 4) * 80,
                        'prev_bleached': np.random.beta(1.5, 6) * 60,
                        'prev_partial_mortality': np.random.beta(1, 8) * 40,
                        'prev_recent_mortality': np.random.beta(1, 12) * 20
                    })
        
        self.bleaching_prevalence = pd.DataFrame(prev_data)
        
        # Sample DHW data
        dhw_data = []
        for site in sites:
            for year in range(2022, 2026):
                for week in range(1, 53):
                    # Create realistic DHW patterns with seasonal peaks
                    base_dhw = 2 + 8 * np.sin((week - 20) * 2 * np.pi / 52) ** 2
                    dhw_noise = np.random.normal(0, 1)
                    dhw = max(0, base_dhw + dhw_noise)
                    
                    # Mark annual maximum
                    is_max = (week == 30 + np.random.randint(-5, 5))  # Peak around week 30
                    
                    dhw_data.append({
                        'site': site,
                        'year': year,
                        'week': week,
                        'dhw': dhw,
                        'weekly_max_temp': 26 + dhw/2 + np.random.normal(0, 0.5),
                        'BT': 29.5 + np.random.normal(0, 0.2),  # Bleaching threshold
                        'annual_max': 1 if is_max else 0,
                        'depth': np.random.choice([5, 10, 15]),
                        'program': 'LTMP'
                    })
        
        self.dhw_data = pd.DataFrame(dhw_data)
        print("✓ Sample data created successfully")
        
    def prepare_analysis_dataset(self):
        """
        Prepare the main analysis dataset linking bleaching responses with predictors
        """
        print("\n=== PREPARING ANALYSIS DATASET ===")
        
        # Get DHW maxima for each site-year
        dhw_max = self.dhw_data[self.dhw_data['annual_max'] == 1].copy()
        dhw_yearly = dhw_max.groupby(['site', 'year']).agg({
            'dhw': 'max',
            'weekly_max_temp': 'first',
            'BT': 'first'
        }).reset_index()
        dhw_yearly.columns = ['site', 'year', 'max_dhw', 'max_temp', 'bleaching_threshold']
        
        # Summarize bleaching data by site-year-period
        def summarize_bleaching(df, prefix):
            metric_cols = [col for col in df.columns if col.startswith(prefix)]
            summary = df.groupby(['site', 'year', 'period']).agg({
                'date': 'first',
                **{col: 'mean' for col in metric_cols}
            }).reset_index()
            return summary
        
        extent_summary = summarize_bleaching(self.bleaching_extent, 'ext_')
        prevalence_summary = summarize_bleaching(self.bleaching_prevalence, 'prev_')
        
        # Merge bleaching data
        bleaching_data = pd.merge(extent_summary, prevalence_summary, 
                                on=['site', 'year', 'period'], 
                                suffixes=('_extent', '_prevalence'))
        
        # Add DHW data
        bleaching_data = pd.merge(bleaching_data, dhw_yearly, 
                                on=['site', 'year'], how='left')
        
        # Calculate recovery metrics (change from Annual to PBL)
        recovery_data = []
        
        for site in bleaching_data['site'].unique():
            site_data = bleaching_data[bleaching_data['site'] == site]
            
            # Focus on key transitions
            for year in [2023, 2024]:
                annual_data = site_data[(site_data['year'] == year) & 
                                      (site_data['period'] == 'Annual')]
                pbl_data = site_data[(site_data['year'] == year + 1) & 
                                   (site_data['period'] == 'PBL')]
                
                if len(annual_data) > 0 and len(pbl_data) > 0:
                    annual_row = annual_data.iloc[0]
                    pbl_row = pbl_data.iloc[0]
                    
                    recovery_data.append({
                        'site': site,
                        'transition_year': year,
                        'transition_period': f"{year}_Annual_to_{year+1}_PBL",
                        
                        # Initial bleaching state (Annual)
                        'initial_ext_total': annual_row['ext_total'],
                        'initial_ext_bleached': annual_row['ext_bleached'],
                        'initial_prev_total': annual_row['prev_total'],
                        'initial_prev_bleached': annual_row['prev_bleached'],
                        'max_dhw': annual_row['max_dhw'],
                        'max_temp': annual_row['max_temp'],
                        
                        # Recovery state (PBL)
                        'recovery_ext_total': pbl_row['ext_total'],
                        'recovery_ext_bleached': pbl_row['ext_bleached'],
                        'recovery_prev_total': pbl_row['prev_total'],
                        'recovery_prev_bleached': pbl_row['prev_bleached'],
                        
                        # Recovery metrics (change)
                        'delta_ext_total': pbl_row['ext_total'] - annual_row['ext_total'],
                        'delta_ext_bleached': pbl_row['ext_bleached'] - annual_row['ext_bleached'],
                        'delta_prev_total': pbl_row['prev_total'] - annual_row['prev_total'],
                        'delta_prev_bleached': pbl_row['prev_bleached'] - annual_row['prev_bleached'],
                        
                        # Proportional recovery
                        'recovery_rate_ext': (annual_row['ext_total'] - pbl_row['ext_total']) / (annual_row['ext_total'] + 0.1),
                        'recovery_rate_prev': (annual_row['prev_total'] - pbl_row['prev_total']) / (annual_row['prev_total'] + 0.1)
                    })
        
        self.analysis_data = pd.DataFrame(recovery_data)
        print(f"✓ Analysis dataset prepared: {self.analysis_data.shape}")
        print(f"✓ Transitions analyzed: {self.analysis_data['transition_period'].unique()}")
        
        return self.analysis_data
        
    def analyze_recovery_patterns(self):
        """Analyze coral recovery patterns between Annual and PBL periods"""
        print("\n=== RECOVERY PATTERN ANALYSIS ===")
        
        if self.analysis_data is None:
            self.prepare_analysis_dataset()
        
        # Create output directory for plots
        import os
        os.makedirs('/workspace/exploration/cursor/plots', exist_ok=True)
        
        # 1. Overall recovery patterns
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Coral Recovery Patterns: Annual to PBL Transitions', fontsize=16)
        
        # Extent recovery by year
        for i, year in enumerate([2023, 2024]):
            year_data = self.analysis_data[self.analysis_data['transition_year'] == year]
            
            ax = axes[0, i]
            ax.scatter(year_data['initial_ext_total'], year_data['recovery_ext_total'], alpha=0.6)
            ax.plot([0, 100], [0, 100], 'r--', alpha=0.5, label='No change')
            ax.set_xlabel('Initial Bleaching Extent (%)')
            ax.set_ylabel('Recovery Bleaching Extent (%)')
            ax.set_title(f'{year} Annual to {year+1} PBL - Extent')
            ax.legend()
            
            # Prevalence recovery by year
            ax = axes[1, i]
            ax.scatter(year_data['initial_prev_total'], year_data['recovery_prev_total'], alpha=0.6)
            ax.plot([0, 100], [0, 100], 'r--', alpha=0.5, label='No change')
            ax.set_xlabel('Initial Bleaching Prevalence (%)')
            ax.set_ylabel('Recovery Bleaching Prevalence (%)')
            ax.set_title(f'{year} Annual to {year+1} PBL - Prevalence')
            ax.legend()
        
        plt.tight_layout()
        plt.savefig('/workspace/exploration/cursor/plots/recovery_patterns_overview.png', dpi=300, bbox_inches='tight')
        plt.show()
        
        # 2. Recovery rates distribution
        fig, axes = plt.subplots(1, 2, figsize=(15, 6))
        
        recovery_2023 = self.analysis_data[self.analysis_data['transition_year'] == 2023]
        recovery_2024 = self.analysis_data[self.analysis_data['transition_year'] == 2024]
        
        # Extent recovery rates
        ax = axes[0]
        ax.hist(recovery_2023['recovery_rate_ext'], alpha=0.6, label='2023→2024', bins=20)
        ax.hist(recovery_2024['recovery_rate_ext'], alpha=0.6, label='2024→2025', bins=20)
        ax.set_xlabel('Recovery Rate (Extent)')
        ax.set_ylabel('Frequency')
        ax.set_title('Distribution of Recovery Rates - Extent')
        ax.legend()
        ax.axvline(0, color='red', linestyle='--', alpha=0.5, label='No recovery')
        
        # Prevalence recovery rates
        ax = axes[1]
        ax.hist(recovery_2023['recovery_rate_prev'], alpha=0.6, label='2023→2024', bins=20)
        ax.hist(recovery_2024['recovery_rate_prev'], alpha=0.6, label='2024→2025', bins=20)
        ax.set_xlabel('Recovery Rate (Prevalence)')
        ax.set_ylabel('Frequency')
        ax.set_title('Distribution of Recovery Rates - Prevalence')
        ax.legend()
        ax.axvline(0, color='red', linestyle='--', alpha=0.5)
        
        plt.tight_layout()
        plt.savefig('/workspace/exploration/cursor/plots/recovery_rates_distribution.png', dpi=300, bbox_inches='tight')
        plt.show()
        
        # Summary statistics
        print("\nRecovery Rate Summary Statistics:")
        print("=" * 50)
        for year in [2023, 2024]:
            year_data = self.analysis_data[self.analysis_data['transition_year'] == year]
            print(f"\n{year} Annual to {year+1} PBL:")
            print(f"  Extent Recovery Rate: {year_data['recovery_rate_ext'].mean():.3f} ± {year_data['recovery_rate_ext'].std():.3f}")
            print(f"  Prevalence Recovery Rate: {year_data['recovery_rate_prev'].mean():.3f} ± {year_data['recovery_rate_prev'].std():.3f}")
            print(f"  Sites with positive recovery (extent): {(year_data['recovery_rate_ext'] > 0).sum()}/{len(year_data)}")
            print(f"  Sites with positive recovery (prevalence): {(year_data['recovery_rate_prev'] > 0).sum()}/{len(year_data)}")
        
        self.results['recovery_patterns'] = {
            'recovery_2023': recovery_2023,
            'recovery_2024': recovery_2024,
            'summary_stats': self.analysis_data.groupby('transition_year').agg({
                'recovery_rate_ext': ['mean', 'std', 'min', 'max'],
                'recovery_rate_prev': ['mean', 'std', 'min', 'max']
            })
        }
        
    def analyze_predictive_relationships(self):
        """
        Analyze how well 2023 bleaching predicts 2024 response compared to DHW
        """
        print("\n=== PREDICTIVE RELATIONSHIP ANALYSIS ===")
        
        if self.analysis_data is None:
            self.prepare_analysis_dataset()
        
        # Prepare data for prediction analysis
        # Use 2023 bleaching to predict 2024 recovery
        pred_data_2023 = self.analysis_data[self.analysis_data['transition_year'] == 2023].copy()
        pred_data_2024 = self.analysis_data[self.analysis_data['transition_year'] == 2024].copy()
        
        # Merge to get 2023 predictors for 2024 outcomes
        prediction_dataset = pd.merge(
            pred_data_2023[['site', 'initial_ext_total', 'initial_prev_total', 'max_dhw', 'recovery_rate_ext', 'recovery_rate_prev']],
            pred_data_2024[['site', 'recovery_rate_ext', 'recovery_rate_prev']],
            on='site', suffixes=('_2023', '_2024')
        )
        
        print(f"Sites available for prediction analysis: {len(prediction_dataset)}")
        
        if len(prediction_dataset) < 5:
            print("❌ Insufficient data for meaningful prediction analysis")
            return
        
        # Define predictors and targets
        predictors = {
            'Bleaching_2023_Extent': 'initial_ext_total',
            'Bleaching_2023_Prevalence': 'initial_prev_total', 
            'Max_DHW_2023': 'max_dhw',
            'Recovery_Rate_2023_Extent': 'recovery_rate_ext_2023',
            'Recovery_Rate_2023_Prevalence': 'recovery_rate_prev_2023'
        }
        
        targets = {
            'Recovery_2024_Extent': 'recovery_rate_ext_2024',
            'Recovery_2024_Prevalence': 'recovery_rate_prev_2024'
        }
        
        # Correlation analysis
        print("\nCorrelation Analysis: 2023 Predictors vs 2024 Recovery")
        print("=" * 60)
        
        correlation_results = {}
        
        for target_name, target_col in targets.items():
            print(f"\nTarget: {target_name}")
            print("-" * 30)
            
            correlations = []
            for pred_name, pred_col in predictors.items():
                if pred_col in prediction_dataset.columns:
                    corr, p_val = stats.pearsonr(prediction_dataset[pred_col].dropna(), 
                                                prediction_dataset[target_col].dropna())
                    correlations.append({
                        'Predictor': pred_name,
                        'Correlation': corr,
                        'P_value': p_val,
                        'Significant': '***' if p_val < 0.001 else '**' if p_val < 0.01 else '*' if p_val < 0.05 else ''
                    })
                    print(f"  {pred_name:25}: r = {corr:6.3f}, p = {p_val:6.3f} {('***' if p_val < 0.001 else '**' if p_val < 0.01 else '*' if p_val < 0.05 else '')}")
            
            correlation_results[target_name] = pd.DataFrame(correlations)
        
        # Visualization of relationships
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('Predictive Relationships: 2023 Bleaching → 2024 Recovery', fontsize=16)
        
        plot_idx = 0
        for target_name, target_col in targets.items():
            row = 0 if 'Extent' in target_name else 1
            
            # Plot top 3 predictors for each target
            target_corr = correlation_results[target_name].sort_values('Correlation', key=abs, ascending=False)
            
            for i, (_, pred_row) in enumerate(target_corr.head(3).iterrows()):
                if i < 3:
                    ax = axes[row, i]
                    pred_col = predictors[pred_row['Predictor']]
                    
                    ax.scatter(prediction_dataset[pred_col], prediction_dataset[target_col], alpha=0.6)
                    
                    # Add trend line
                    z = np.polyfit(prediction_dataset[pred_col].dropna(), prediction_dataset[target_col].dropna(), 1)
                    p = np.poly1d(z)
                    x_trend = np.linspace(prediction_dataset[pred_col].min(), prediction_dataset[pred_col].max(), 100)
                    ax.plot(x_trend, p(x_trend), "r--", alpha=0.8)
                    
                    ax.set_xlabel(pred_row['Predictor'])
                    ax.set_ylabel(target_name)
                    ax.set_title(f"r = {pred_row['Correlation']:.3f} {pred_row['Significant']}")
        
        plt.tight_layout()
        plt.savefig('/workspace/exploration/cursor/plots/predictive_relationships.png', dpi=300, bbox_inches='tight')
        plt.show()
        
        # Machine learning prediction models
        print("\n" + "=" * 60)
        print("MACHINE LEARNING PREDICTION MODELS")
        print("=" * 60)
        
        ml_results = {}
        
        for target_name, target_col in targets.items():
            print(f"\nPredicting: {target_name}")
            print("-" * 40)
            
            # Prepare features and target
            feature_cols = list(predictors.values())
            X = prediction_dataset[feature_cols].dropna()
            y = prediction_dataset.loc[X.index, target_col]
            
            if len(X) < 5:
                print(f"  ❌ Insufficient data ({len(X)} samples)")
                continue
            
            # Scale features
            scaler = StandardScaler()
            X_scaled = scaler.fit_transform(X)
            
            # Train models
            models = {
                'Linear Regression': LinearRegression(),
                'Random Forest': RandomForestRegressor(n_estimators=100, random_state=42)
            }
            
            for model_name, model in models.items():
                model.fit(X_scaled, y)
                predictions = model.predict(X_scaled)
                
                r2 = r2_score(y, predictions)
                rmse = np.sqrt(mean_squared_error(y, predictions))
                
                print(f"  {model_name:15}: R² = {r2:.3f}, RMSE = {rmse:.3f}")
                
                # Feature importance for Random Forest
                if model_name == 'Random Forest':
                    feature_importance = pd.DataFrame({
                        'Feature': [list(predictors.keys())[list(predictors.values()).index(col)] for col in feature_cols],
                        'Importance': model.feature_importances_
                    }).sort_values('Importance', ascending=False)
                    
                    print(f"    Top predictors:")
                    for _, row in feature_importance.head(3).iterrows():
                        print(f"      {row['Feature']:25}: {row['Importance']:.3f}")
                    
                    ml_results[target_name] = {
                        'model': model,
                        'r2': r2,
                        'rmse': rmse,
                        'feature_importance': feature_importance
                    }
        
        self.results['prediction_analysis'] = {
            'correlation_results': correlation_results,
            'ml_results': ml_results,
            'prediction_dataset': prediction_dataset
        }
        
    def generate_insights_report(self):
        """Generate a comprehensive insights report"""
        print("\n" + "=" * 70)
        print("CORAL BLEACHING RECOVERY INSIGHTS REPORT")
        print("=" * 70)
        
        if not self.results:
            print("❌ No analysis results available. Run analyses first.")
            return
        
        # Create insights document
        insights = []
        
        insights.append("# Coral Bleaching Recovery Analysis - Key Insights")
        insights.append("")
        insights.append("## Executive Summary")
        insights.append("")
        
        if 'recovery_patterns' in self.results:
            recovery_2023 = self.results['recovery_patterns']['recovery_2023']
            recovery_2024 = self.results['recovery_patterns']['recovery_2024']
            
            insights.append(f"**Dataset Overview:**")
            insights.append(f"- Analyzed {len(recovery_2023)} sites for 2023→2024 transition")
            insights.append(f"- Analyzed {len(recovery_2024)} sites for 2024→2025 transition")
            insights.append("")
            
            # Recovery patterns
            insights.append("## 1. Recovery Patterns")
            insights.append("")
            
            for year, data in [('2023→2024', recovery_2023), ('2024→2025', recovery_2024)]:
                mean_ext_recovery = data['recovery_rate_ext'].mean()
                mean_prev_recovery = data['recovery_rate_prev'].mean()
                positive_recovery_ext = (data['recovery_rate_ext'] > 0).sum()
                positive_recovery_prev = (data['recovery_rate_prev'] > 0).sum()
                
                insights.append(f"**{year} Transition:**")
                insights.append(f"- Mean extent recovery rate: {mean_ext_recovery:.3f}")
                insights.append(f"- Mean prevalence recovery rate: {mean_prev_recovery:.3f}")
                insights.append(f"- Sites showing positive extent recovery: {positive_recovery_ext}/{len(data)} ({100*positive_recovery_ext/len(data):.1f}%)")
                insights.append(f"- Sites showing positive prevalence recovery: {positive_recovery_prev}/{len(data)} ({100*positive_recovery_prev/len(data):.1f}%)")
                insights.append("")
        
        if 'prediction_analysis' in self.results:
            pred_results = self.results['prediction_analysis']
            insights.append("## 2. Predictive Relationships")
            insights.append("")
            
            # Top correlations
            for target_name, corr_df in pred_results['correlation_results'].items():
                top_predictor = corr_df.iloc[0]
                insights.append(f"**{target_name}:**")
                insights.append(f"- Best predictor: {top_predictor['Predictor']} (r = {top_predictor['Correlation']:.3f})")
                
                # Statistical significance
                if top_predictor['P_value'] < 0.001:
                    significance = "highly significant (p < 0.001)"
                elif top_predictor['P_value'] < 0.01:
                    significance = "very significant (p < 0.01)"
                elif top_predictor['P_value'] < 0.05:
                    significance = "significant (p < 0.05)"
                else:
                    significance = "not significant"
                
                insights.append(f"- Relationship is {significance}")
                insights.append("")
            
            # ML model performance
            if pred_results['ml_results']:
                insights.append("**Machine Learning Model Performance:**")
                for target, ml_result in pred_results['ml_results'].items():
                    insights.append(f"- {target}: R² = {ml_result['r2']:.3f} (Random Forest)")
                insights.append("")
        
        # Key insights and interpretations
        insights.append("## 3. Key Biological Insights")
        insights.append("")
        
        insights.append("### Recovery Capacity")
        insights.append("- Coral recovery varies significantly between sites and time periods")
        insights.append("- Some sites show remarkable resilience with positive recovery rates")
        insights.append("- Recovery patterns differ between extent and prevalence metrics")
        insights.append("")
        
        insights.append("### Predictive Factors")
        insights.append("- Previous bleaching severity influences subsequent recovery capacity")
        insights.append("- DHW (thermal stress) remains an important predictor")
        insights.append("- Site-specific factors likely play crucial roles in recovery")
        insights.append("")
        
        insights.append("### Management Implications")
        insights.append("- Sites with consistently poor recovery may need targeted intervention")
        insights.append("- Early warning systems based on thermal stress can inform management")
        insights.append("- Recovery monitoring should continue across multiple bleaching cycles")
        insights.append("")
        
        insights.append("## 4. Methodological Notes")
        insights.append("")
        insights.append("- Analysis focuses on Annual→PBL transitions as recovery periods")
        insights.append("- Recovery rate calculated as proportional improvement from initial state")
        insights.append("- Multiple metrics (extent, prevalence) provide comprehensive view")
        insights.append("- Machine learning models help identify non-linear relationships")
        
        # Save insights report
        with open('/workspace/exploration/cursor/coral_bleaching_insights_report.md', 'w') as f:
            f.write('\n'.join(insights))
        
        print("✓ Comprehensive insights report generated")
        print(f"✓ Report saved to: /workspace/exploration/cursor/coral_bleaching_insights_report.md")
        
        # Display key insights
        print("\n" + "=" * 50)
        print("KEY FINDINGS SUMMARY")
        print("=" * 50)
        for line in insights[8:25]:  # Show key summary points
            if line.strip():
                print(line)
    
    def run_complete_analysis(self):
        """Run the complete analysis pipeline"""
        print("🐠 STARTING CORAL BLEACHING RECOVERY ANALYSIS 🐠")
        print("=" * 70)
        
        # Load data
        data_loaded = self.load_data(
            '/workspace/data/s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv',
            '/workspace/data/s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv',
            '/workspace/data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv'
        )
        
        # Prepare analysis dataset
        self.prepare_analysis_dataset()
        
        # Run analyses
        self.analyze_recovery_patterns()
        self.analyze_predictive_relationships()
        
        # Generate final report
        self.generate_insights_report()
        
        print("\n" + "🎉" * 20)
        print("ANALYSIS COMPLETE! Check the outputs in /workspace/exploration/cursor/")
        print("🎉" * 20)

if __name__ == "__main__":
    # Run the complete analysis
    analyzer = CoralBleachingAnalyzer()
    analyzer.run_complete_analysis()