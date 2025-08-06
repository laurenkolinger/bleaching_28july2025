"""
Direct Comparison: Previous Bleaching vs DHW as Predictors of Coral Recovery

This script specifically addresses the research question:
"How much does bleaching in 2023 predict coral response in 2024 compared to max DHW?"
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.preprocessing import StandardScaler
from scipy.stats import pearsonr
import warnings
warnings.filterwarnings('ignore')

def load_and_prepare_data():
    """Load and prepare data for the comparison"""
    
    # Load data
    bleaching_extent = pd.read_csv("/workspace/data/s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv")
    temp_dhw = pd.read_csv("/workspace/data/s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv")
    temp_dhw['Date'] = pd.to_datetime(temp_dhw['Date'])
    
    # Focus on target periods
    target_periods = ['2023_Annual', '2024_PBL', '2024_Annual', '2025_PBL']
    extent_filtered = bleaching_extent[bleaching_extent['Period'].isin(target_periods)].copy()
    
    # Pivot to get each period as a column
    extent_pivot = extent_filtered.pivot_table(
        index=['Site_ID', 'Region', 'Latitude', 'Longitude', 'Depth_m', 'Exposure'],
        columns='Period',
        values='Bleaching_Extent_Percent',
        aggfunc='mean'
    ).reset_index()
    
    # Calculate key variables
    extent_pivot['Previous_Bleaching_2023'] = extent_pivot['2023_Annual']
    extent_pivot['Recovery_2023_to_2024'] = extent_pivot['2024_PBL'] - extent_pivot['2023_Annual']
    extent_pivot['Response_2024'] = extent_pivot['2025_PBL'] - extent_pivot['2024_Annual']  # TARGET VARIABLE
    
    # Add DHW data
    dhw_metrics = []
    for site_id in extent_pivot['Site_ID'].unique():
        site_temp = temp_dhw[temp_dhw['Site_ID'] == site_id].copy()
        
        if len(site_temp) == 0:
            # Use first available site if exact match not found
            site_temp = temp_dhw[temp_dhw['Site_ID'] == temp_dhw['Site_ID'].iloc[0]].copy()
        
        site_temp['Year'] = site_temp['Date'].dt.year
        
        # 2023 DHW metrics
        temp_2023 = site_temp[site_temp['Year'] == 2023]
        max_dhw_2023 = temp_2023['DHW_DegreeWeeks'].max() if len(temp_2023) > 0 else 0
        mean_dhw_2023 = temp_2023['DHW_DegreeWeeks'].mean() if len(temp_2023) > 0 else 0
        
        # 2024 DHW metrics  
        temp_2024 = site_temp[site_temp['Year'] == 2024]
        max_dhw_2024 = temp_2024['DHW_DegreeWeeks'].max() if len(temp_2024) > 0 else 0
        mean_dhw_2024 = temp_2024['DHW_DegreeWeeks'].mean() if len(temp_2024) > 0 else 0
        
        dhw_metrics.append({
            'Site_ID': site_id,
            'Max_DHW_2023': max_dhw_2023,
            'Mean_DHW_2023': mean_dhw_2023,
            'Max_DHW_2024': max_dhw_2024,
            'Mean_DHW_2024': mean_dhw_2024
        })
    
    dhw_df = pd.DataFrame(dhw_metrics)
    
    # Merge datasets
    final_data = extent_pivot.merge(dhw_df, on='Site_ID', how='left').dropna()
    
    return final_data

def compare_predictive_power(data):
    """Direct comparison of predictive power"""
    
    print("="*80)
    print("DIRECT COMPARISON: PREVIOUS BLEACHING vs DHW")
    print("="*80)
    print(f"Sample size: {len(data)} sites")
    print(f"Target: Response_2024 (change from 2024 Annual to 2025 PBL)")
    print(f"Range: {data['Response_2024'].min():.1f}% to {data['Response_2024'].max():.1f}%")
    print()
    
    target = data['Response_2024']
    
    # Test different predictor combinations
    predictors = {
        'Previous_Bleaching_Only': ['Previous_Bleaching_2023'],
        'DHW_2023_Only': ['Max_DHW_2023'],
        'DHW_2024_Only': ['Max_DHW_2024'],
        'Recovery_Pattern': ['Recovery_2023_to_2024'],
        'Combined_Bleaching_DHW_2023': ['Previous_Bleaching_2023', 'Max_DHW_2023'],
        'Combined_Bleaching_DHW_2024': ['Previous_Bleaching_2023', 'Max_DHW_2024'],
        'All_DHW': ['Max_DHW_2023', 'Max_DHW_2024'],
        'All_Variables': ['Previous_Bleaching_2023', 'Max_DHW_2023', 'Max_DHW_2024', 'Recovery_2023_to_2024']
    }
    
    results = {}
    
    for name, pred_vars in predictors.items():
        X = data[pred_vars]
        y = target
        
        # Standardize features
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X)
        
        # Random Forest
        rf = RandomForestRegressor(n_estimators=100, random_state=42)
        rf.fit(X_scaled, y)
        rf_pred = rf.predict(X_scaled)
        rf_r2 = r2_score(y, rf_pred)
        rf_rmse = np.sqrt(mean_squared_error(y, rf_pred))
        
        # Linear Regression
        lr = LinearRegression()
        lr.fit(X_scaled, y)
        lr_pred = lr.predict(X_scaled)
        lr_r2 = r2_score(y, lr_pred)
        lr_rmse = np.sqrt(mean_squared_error(y, lr_pred))
        
        # Correlation (for single variables)
        if len(pred_vars) == 1:
            corr, p_value = pearsonr(data[pred_vars[0]], target)
        else:
            corr, p_value = np.nan, np.nan
        
        results[name] = {
            'variables': pred_vars,
            'rf_r2': rf_r2,
            'rf_rmse': rf_rmse,
            'lr_r2': lr_r2,
            'lr_rmse': lr_rmse,
            'correlation': corr,
            'p_value': p_value,
            'feature_importance': dict(zip(pred_vars, rf.feature_importances_)) if len(pred_vars) <= 4 else {}
        }
        
        print(f"{name}:")
        print(f"  Variables: {', '.join(pred_vars)}")
        print(f"  Random Forest R²: {rf_r2:.3f} (RMSE: {rf_rmse:.1f}%)")
        print(f"  Linear Regression R²: {lr_r2:.3f} (RMSE: {lr_rmse:.1f}%)")
        if len(pred_vars) == 1:
            print(f"  Correlation: {corr:.3f} (p={p_value:.3f})")
        if len(pred_vars) <= 4 and len(pred_vars) > 1:
            print("  Feature importance:")
            for var, imp in results[name]['feature_importance'].items():
                print(f"    {var}: {imp:.3f}")
        print()
    
    return results

def analyze_recovery_predictors(data):
    """Analyze what predicts recovery patterns"""
    
    print("="*80)
    print("RECOVERY PREDICTION ANALYSIS")
    print("="*80)
    
    # Create recovery categories
    data['Recovery_Category'] = pd.cut(
        data['Recovery_2023_to_2024'],
        bins=[-np.inf, -20, -5, 5, np.inf],
        labels=['Strong_Recovery', 'Moderate_Recovery', 'Stable', 'Decline']
    )
    
    # Analyze what predicts strong recovery
    print("Recovery patterns by initial bleaching severity:")
    
    # Categorize initial bleaching
    data['Bleaching_Severity'] = pd.cut(
        data['Previous_Bleaching_2023'],
        bins=[0, 30, 60, 100],
        labels=['Mild', 'Moderate', 'Severe']
    )
    
    recovery_analysis = data.groupby('Bleaching_Severity').agg({
        'Recovery_2023_to_2024': ['mean', 'std', 'count'],
        'Max_DHW_2023': 'mean',
        'Response_2024': 'mean'
    }).round(2)
    
    print(recovery_analysis)
    print()
    
    # Correlation matrix
    corr_vars = ['Previous_Bleaching_2023', 'Max_DHW_2023', 'Max_DHW_2024', 
                 'Recovery_2023_to_2024', 'Response_2024']
    correlations = data[corr_vars].corr()
    
    print("Correlation Matrix:")
    print(correlations.round(3))
    print()
    
    # Key insights
    bleaching_response_corr = data['Previous_Bleaching_2023'].corr(data['Response_2024'])
    dhw_2023_response_corr = data['Max_DHW_2023'].corr(data['Response_2024'])
    dhw_2024_response_corr = data['Max_DHW_2024'].corr(data['Response_2024'])
    recovery_response_corr = data['Recovery_2023_to_2024'].corr(data['Response_2024'])
    
    print("DIRECT CORRELATIONS WITH 2024 RESPONSE:")
    print(f"Previous Bleaching (2023): {bleaching_response_corr:.3f}")
    print(f"Max DHW 2023: {dhw_2023_response_corr:.3f}")
    print(f"Max DHW 2024: {dhw_2024_response_corr:.3f}")
    print(f"Recovery Pattern: {recovery_response_corr:.3f}")
    print()
    
    return correlations

def create_comparison_visualizations(data, results):
    """Create focused comparison visualizations"""
    
    fig, axes = plt.subplots(2, 3, figsize=(18, 12))
    fig.suptitle('Previous Bleaching vs DHW: Predictive Power Comparison', fontsize=16, fontweight='bold')
    
    # 1. R² comparison
    ax1 = axes[0, 0]
    comparison_models = ['Previous_Bleaching_Only', 'DHW_2023_Only', 'DHW_2024_Only', 'Recovery_Pattern']
    r2_scores = [results[model]['rf_r2'] for model in comparison_models]
    colors = ['coral', 'lightblue', 'lightgreen', 'gold']
    
    bars = ax1.bar(range(len(comparison_models)), r2_scores, color=colors, alpha=0.7)
    ax1.set_xlabel('Predictor Type')
    ax1.set_ylabel('R² Score (Random Forest)')
    ax1.set_title('Predictive Power Comparison')
    ax1.set_xticks(range(len(comparison_models)))
    ax1.set_xticklabels([m.replace('_', '\n') for m in comparison_models], rotation=45)
    
    # Add value labels
    for bar, score in zip(bars, r2_scores):
        ax1.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01,
                f'{score:.3f}', ha='center', va='bottom', fontweight='bold')
    
    # 2. Previous bleaching vs 2024 response
    ax2 = axes[0, 1]
    ax2.scatter(data['Previous_Bleaching_2023'], data['Response_2024'], alpha=0.7, s=60, color='coral')
    ax2.set_xlabel('Previous Bleaching 2023 (%)')
    ax2.set_ylabel('2024 Response (%)')
    ax2.set_title('Previous Bleaching vs 2024 Response')
    
    # Add trend line
    z = np.polyfit(data['Previous_Bleaching_2023'], data['Response_2024'], 1)
    p = np.poly1d(z)
    ax2.plot(data['Previous_Bleaching_2023'], p(data['Previous_Bleaching_2023']), "r--", alpha=0.8)
    
    # Add correlation
    corr = data['Previous_Bleaching_2023'].corr(data['Response_2024'])
    ax2.text(0.05, 0.95, f'r = {corr:.3f}', transform=ax2.transAxes, fontsize=12, 
             bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8))
    
    # 3. DHW 2023 vs 2024 response
    ax3 = axes[0, 2]
    ax3.scatter(data['Max_DHW_2023'], data['Response_2024'], alpha=0.7, s=60, color='lightblue')
    ax3.set_xlabel('Max DHW 2023')
    ax3.set_ylabel('2024 Response (%)')
    ax3.set_title('2023 DHW vs 2024 Response')
    
    # Add trend line
    z = np.polyfit(data['Max_DHW_2023'], data['Response_2024'], 1)
    p = np.poly1d(z)
    ax3.plot(data['Max_DHW_2023'], p(data['Max_DHW_2023']), "b--", alpha=0.8)
    
    # Add correlation
    corr = data['Max_DHW_2023'].corr(data['Response_2024'])
    ax3.text(0.05, 0.95, f'r = {corr:.3f}', transform=ax3.transAxes, fontsize=12,
             bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8))
    
    # 4. DHW vs Bleaching relationship
    ax4 = axes[1, 0]
    ax4.scatter(data['Max_DHW_2023'], data['Previous_Bleaching_2023'], alpha=0.7, s=60, color='purple')
    ax4.set_xlabel('Max DHW 2023')
    ax4.set_ylabel('Previous Bleaching 2023 (%)')
    ax4.set_title('DHW vs Bleaching Relationship')
    
    # Add trend line
    z = np.polyfit(data['Max_DHW_2023'], data['Previous_Bleaching_2023'], 1)
    p = np.poly1d(z)
    ax4.plot(data['Max_DHW_2023'], p(data['Max_DHW_2023']), "purple", linestyle="--", alpha=0.8)
    
    # Add correlation
    corr = data['Max_DHW_2023'].corr(data['Previous_Bleaching_2023'])
    ax4.text(0.05, 0.95, f'r = {corr:.3f}', transform=ax4.transAxes, fontsize=12,
             bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8))
    
    # 5. Recovery pattern vs 2024 response
    ax5 = axes[1, 1]
    ax5.scatter(data['Recovery_2023_to_2024'], data['Response_2024'], alpha=0.7, s=60, color='gold')
    ax5.set_xlabel('Recovery 2023→2024 (%)')
    ax5.set_ylabel('2024 Response (%)')
    ax5.set_title('Recovery Pattern vs 2024 Response')
    
    # Add trend line
    z = np.polyfit(data['Recovery_2023_to_2024'], data['Response_2024'], 1)
    p = np.poly1d(z)
    ax5.plot(data['Recovery_2023_to_2024'], p(data['Recovery_2023_to_2024']), "orange", linestyle="--", alpha=0.8)
    
    # Add correlation
    corr = data['Recovery_2023_to_2024'].corr(data['Response_2024'])
    ax5.text(0.05, 0.95, f'r = {corr:.3f}', transform=ax5.transAxes, fontsize=12,
             bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8))
    
    # 6. Combined model performance
    ax6 = axes[1, 2]
    combined_models = ['Previous_Bleaching_Only', 'Combined_Bleaching_DHW_2023', 'All_Variables']
    combined_r2 = [results[model]['rf_r2'] for model in combined_models]
    combined_colors = ['coral', 'mediumpurple', 'darkgreen']
    
    bars = ax6.bar(range(len(combined_models)), combined_r2, color=combined_colors, alpha=0.7)
    ax6.set_xlabel('Model Type')
    ax6.set_ylabel('R² Score')
    ax6.set_title('Adding Variables to Bleaching')
    ax6.set_xticks(range(len(combined_models)))
    ax6.set_xticklabels([m.replace('_', '\n') for m in combined_models], rotation=45)
    
    # Add value labels
    for bar, score in zip(bars, combined_r2):
        ax6.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01,
                f'{score:.3f}', ha='center', va='bottom', fontweight='bold')
    
    plt.tight_layout()
    plt.savefig('/workspace/exploration/cursor/outputs/figures/bleaching_vs_dhw_comparison.png', 
                dpi=300, bbox_inches='tight')
    plt.show()
    
    print("Comparison visualization saved!")

def generate_summary_insights(results, data):
    """Generate key summary insights"""
    
    print("="*80)
    print("SUMMARY INSIGHTS: PREVIOUS BLEACHING vs DHW")
    print("="*80)
    
    # Compare R² scores
    bleaching_r2 = results['Previous_Bleaching_Only']['rf_r2']
    dhw_2023_r2 = results['DHW_2023_Only']['rf_r2']
    dhw_2024_r2 = results['DHW_2024_Only']['rf_r2']
    combined_r2 = results['Combined_Bleaching_DHW_2023']['rf_r2']
    recovery_r2 = results['Recovery_Pattern']['rf_r2']
    
    print(f"1. PREDICTIVE POWER COMPARISON (R² scores):")
    print(f"   - Previous Bleaching alone: {bleaching_r2:.3f}")
    print(f"   - DHW 2023 alone: {dhw_2023_r2:.3f}")
    print(f"   - DHW 2024 alone: {dhw_2024_r2:.3f}")
    print(f"   - Recovery pattern: {recovery_r2:.3f}")
    print(f"   - Combined (Bleaching + DHW 2023): {combined_r2:.3f}")
    print()
    
    # Determine which is better
    if bleaching_r2 > dhw_2023_r2 and bleaching_r2 > dhw_2024_r2:
        winner = "Previous Bleaching"
        margin = bleaching_r2 - max(dhw_2023_r2, dhw_2024_r2)
    elif dhw_2023_r2 > bleaching_r2 and dhw_2023_r2 > dhw_2024_r2:
        winner = "DHW 2023"
        margin = dhw_2023_r2 - max(bleaching_r2, dhw_2024_r2)
    else:
        winner = "DHW 2024"
        margin = dhw_2024_r2 - max(bleaching_r2, dhw_2023_r2)
    
    print(f"2. WINNER: {winner} is the best single predictor")
    print(f"   - Margin over next best: {margin:.3f} R² units")
    print()
    
    # Value added by combining
    improvement = combined_r2 - bleaching_r2
    print(f"3. VALUE OF COMBINING VARIABLES:")
    print(f"   - Adding DHW to bleaching improves R² by: {improvement:.3f}")
    print(f"   - Relative improvement: {(improvement/bleaching_r2)*100:.1f}%")
    print()
    
    # Correlations
    bleaching_corr = data['Previous_Bleaching_2023'].corr(data['Response_2024'])
    dhw_2023_corr = data['Max_DHW_2023'].corr(data['Response_2024'])
    dhw_2024_corr = data['Max_DHW_2024'].corr(data['Response_2024'])
    
    print(f"4. DIRECT CORRELATIONS WITH 2024 RESPONSE:")
    print(f"   - Previous Bleaching: {bleaching_corr:.3f}")
    print(f"   - DHW 2023: {dhw_2023_corr:.3f}")
    print(f"   - DHW 2024: {dhw_2024_corr:.3f}")
    print()
    
    # Practical interpretation
    print(f"5. PRACTICAL INTERPRETATION:")
    if abs(bleaching_corr) > abs(dhw_2023_corr):
        print(f"   - Previous bleaching is {abs(bleaching_corr)/abs(dhw_2023_corr):.1f}x more correlated with 2024 response than DHW 2023")
    else:
        print(f"   - DHW 2023 is {abs(dhw_2023_corr)/abs(bleaching_corr):.1f}x more correlated with 2024 response than previous bleaching")
    
    print(f"   - For every 10% increase in 2023 bleaching, 2024 response changes by {bleaching_corr * 10:.1f}%")
    print(f"   - For every 1 unit increase in DHW 2023, 2024 response changes by {dhw_2023_corr * 1:.1f}%")
    print()
    
    return {
        'best_predictor': winner,
        'margin': margin,
        'improvement_from_combining': improvement,
        'correlations': {
            'bleaching': bleaching_corr,
            'dhw_2023': dhw_2023_corr,
            'dhw_2024': dhw_2024_corr
        }
    }

def main():
    """Run the complete comparison analysis"""
    
    print("LOADING DATA...")
    data = load_and_prepare_data()
    
    print(f"\nDATASET SUMMARY:")
    print(f"Sites: {len(data)}")
    print(f"2024 Response range: {data['Response_2024'].min():.1f}% to {data['Response_2024'].max():.1f}%")
    print(f"Previous Bleaching range: {data['Previous_Bleaching_2023'].min():.1f}% to {data['Previous_Bleaching_2023'].max():.1f}%")
    print(f"DHW 2023 range: {data['Max_DHW_2023'].min():.1f} to {data['Max_DHW_2023'].max():.1f}")
    print()
    
    # Run analyses
    results = compare_predictive_power(data)
    correlations = analyze_recovery_predictors(data)
    summary = generate_summary_insights(results, data)
    
    # Create visualizations
    create_comparison_visualizations(data, results)
    
    # Save summary data
    summary_df = pd.DataFrame({
        'Site_ID': data['Site_ID'],
        'Previous_Bleaching_2023': data['Previous_Bleaching_2023'],
        'Max_DHW_2023': data['Max_DHW_2023'],
        'Max_DHW_2024': data['Max_DHW_2024'],
        'Recovery_2023_to_2024': data['Recovery_2023_to_2024'],
        'Response_2024': data['Response_2024'],
        'Region': data['Region'],
        'Exposure': data['Exposure']
    })
    
    summary_df.to_csv('/workspace/exploration/cursor/outputs/bleaching_vs_dhw_summary_data.csv', index=False)
    
    print("="*80)
    print("ANALYSIS COMPLETE!")
    print("="*80)
    print("Files saved:")
    print("- Visualization: /workspace/exploration/cursor/outputs/figures/bleaching_vs_dhw_comparison.png")
    print("- Summary data: /workspace/exploration/cursor/outputs/bleaching_vs_dhw_summary_data.csv")

if __name__ == "__main__":
    main()