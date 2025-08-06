#!/usr/bin/env python3
"""
Simplified Coral Bleaching Recovery Analysis (No External Dependencies)

This script demonstrates the coral bleaching analysis framework using only
built-in Python libraries. When pandas, numpy, matplotlib, etc. are available,
use the full coral_bleaching_analysis.py script instead.
"""

import csv
import random
import json
from datetime import datetime, timedelta
import math
import os

class SimplifiedCoralAnalyzer:
    """Simplified coral bleaching analyzer using only built-in Python libraries"""
    
    def __init__(self):
        self.bleaching_data = []
        self.dhw_data = []
        self.analysis_results = {}
        
    def generate_sample_data(self):
        """Generate realistic sample data for demonstration"""
        print("🐠 Generating sample coral bleaching data...")
        
        # Set random seed for reproducible results
        random.seed(42)
        
        sites = [f"Site_{i:02d}" for i in range(1, 34)]  # 33 sites
        years = [2022, 2023, 2024, 2025]
        periods = ['Annual', 'PBL']
        
        # Generate bleaching extent and prevalence data
        self.bleaching_data = []
        for site in sites:
            site_resilience = random.uniform(0.5, 1.5)  # Site-specific resilience factor
            
            for year in years:
                for period in periods:
                    if year == 2025 and period == 'Annual':  # Skip 2025 Annual
                        continue
                    
                    # Create realistic bleaching data with temporal patterns
                    seasonal_factor = 1.5 if period == 'Annual' else 0.7  # Higher in Annual season
                    year_trend = 1.0 + (year - 2022) * 0.1  # Slight increasing trend
                    
                    base_bleaching = random.uniform(8, 35) * seasonal_factor * year_trend / site_resilience
                    
                    record = {
                        'site': site,
                        'year': year,
                        'period': period,
                        'date': f"{year}-{'06' if period == 'Annual' else '02'}-{random.randint(10, 20):02d}",
                        'ext_total': max(0, min(100, base_bleaching + random.uniform(-5, 5))),
                        'ext_bleached': max(0, min(100, base_bleaching * 0.6 + random.uniform(-3, 3))),
                        'prev_total': max(0, min(100, base_bleaching * 1.1 + random.uniform(-8, 8))),
                        'prev_bleached': max(0, min(100, base_bleaching * 0.7 + random.uniform(-4, 4)))
                    }
                    self.bleaching_data.append(record)
        
        # Generate DHW data
        self.dhw_data = []
        for site in sites:
            site_thermal_sensitivity = random.uniform(0.8, 1.2)
            
            for year in range(2022, 2026):
                # Generate annual maximum DHW for each site-year with realistic patterns
                base_dhw = random.uniform(3, 12) * site_thermal_sensitivity
                year_effect = 1.0 + (year - 2022) * 0.15  # Increasing thermal stress over time
                
                annual_max_dhw = base_dhw * year_effect + random.uniform(-2, 2)
                
                record = {
                    'site': site,
                    'year': year,
                    'max_dhw': round(max(0, annual_max_dhw), 2),
                    'max_temp': round(26 + annual_max_dhw/2 + random.uniform(-0.8, 0.8), 2),
                    'bleaching_threshold': round(29.5 + random.uniform(-0.3, 0.3), 2)
                }
                self.dhw_data.append(record)
        
        print(f"✓ Generated {len(self.bleaching_data)} bleaching records")
        print(f"✓ Generated {len(self.dhw_data)} DHW records")
        
        # Save data to CSV files for inspection
        self.save_data_to_csv()
        
    def save_data_to_csv(self):
        """Save generated data to CSV files"""
        os.makedirs('outputs/data', exist_ok=True)
        
        # Save bleaching data
        with open('outputs/data/sample_bleaching_data.csv', 'w', newline='') as f:
            if self.bleaching_data:
                writer = csv.DictWriter(f, fieldnames=self.bleaching_data[0].keys())
                writer.writeheader()
                writer.writerows(self.bleaching_data)
        
        # Save DHW data
        with open('outputs/data/sample_dhw_data.csv', 'w', newline='') as f:
            if self.dhw_data:
                writer = csv.DictWriter(f, fieldnames=self.dhw_data[0].keys())
                writer.writeheader()
                writer.writerows(self.dhw_data)
        
        print("✓ Sample data saved to outputs/data/")
    
    def prepare_recovery_analysis(self):
        """Prepare data for recovery analysis"""
        print("\n=== PREPARING RECOVERY ANALYSIS ===")
        
        # Group data by site
        site_data = {}
        for record in self.bleaching_data:
            site = record['site']
            if site not in site_data:
                site_data[site] = []
            site_data[site].append(record)
        
        # Calculate recovery metrics for Annual → PBL transitions
        recovery_data = []
        
        for site, records in site_data.items():
            # Sort by year and period
            records.sort(key=lambda x: (x['year'], x['period']))
            
            # Find DHW data for this site
            site_dhw = {r['year']: r for r in self.dhw_data if r['site'] == site}
            
            # Create dictionaries for easy lookup
            annual_records = {r['year']: r for r in records if r['period'] == 'Annual'}
            pbl_records = {r['year']: r for r in records if r['period'] == 'PBL'}
            
            # Find Annual → PBL transitions
            for year in [2023, 2024]:  # Focus on key transition years
                if year in annual_records and (year + 1) in pbl_records:
                    annual = annual_records[year]
                    pbl = pbl_records[year + 1]
                    
                    # Calculate recovery metrics
                    ext_recovery = annual['ext_total'] - pbl['ext_total']
                    prev_recovery = annual['prev_total'] - pbl['prev_total']
                    
                    # Get DHW for the bleaching year
                    dhw_info = site_dhw.get(year, {})
                    
                    recovery_record = {
                        'site': site,
                        'transition_year': year,
                        'transition': f"{year}_Annual_to_{year+1}_PBL",
                        'initial_ext_total': annual['ext_total'],
                        'recovery_ext_total': pbl['ext_total'],
                        'initial_prev_total': annual['prev_total'],
                        'recovery_prev_total': pbl['prev_total'],
                        'delta_ext_total': ext_recovery,
                        'delta_prev_total': prev_recovery,
                        'recovery_rate_ext': ext_recovery / (annual['ext_total'] + 0.1),
                        'recovery_rate_prev': prev_recovery / (annual['prev_total'] + 0.1),
                        'max_dhw': dhw_info.get('max_dhw', 0),
                        'max_temp': dhw_info.get('max_temp', 0)
                    }
                    recovery_data.append(recovery_record)
        
        self.recovery_data = recovery_data
        print(f"✓ Prepared {len(recovery_data)} recovery transitions")
        
        # Save recovery data
        os.makedirs('outputs/data', exist_ok=True)
        with open('outputs/data/recovery_analysis_data.csv', 'w', newline='') as f:
            if recovery_data:
                writer = csv.DictWriter(f, fieldnames=recovery_data[0].keys())
                writer.writeheader()
                writer.writerows(recovery_data)
        
        return recovery_data
    
    def analyze_recovery_patterns(self):
        """Analyze recovery patterns"""
        print("\n=== RECOVERY PATTERN ANALYSIS ===")
        
        if not hasattr(self, 'recovery_data'):
            self.prepare_recovery_analysis()
        
        # Group by transition year
        transitions_2023 = [r for r in self.recovery_data if r['transition_year'] == 2023]
        transitions_2024 = [r for r in self.recovery_data if r['transition_year'] == 2024]
        
        # Calculate summary statistics
        def calculate_stats(data, metric):
            values = [r[metric] for r in data if r[metric] is not None]
            if not values:
                return {'count': 0, 'mean': 0, 'min': 0, 'max': 0}
            
            return {
                'count': len(values),
                'mean': sum(values) / len(values),
                'min': min(values),
                'max': max(values)
            }
        
        # Analyze 2023→2024 transition
        print("\n2023→2024 Transition Recovery:")
        if transitions_2023:
            ext_stats_2023 = calculate_stats(transitions_2023, 'recovery_rate_ext')
            prev_stats_2023 = calculate_stats(transitions_2023, 'recovery_rate_prev')
            
            print(f"  Sites analyzed: {len(transitions_2023)}")
            print(f"  Extent Recovery Rate: {ext_stats_2023['mean']:.3f} (range: {ext_stats_2023['min']:.3f} to {ext_stats_2023['max']:.3f})")
            print(f"  Prevalence Recovery Rate: {prev_stats_2023['mean']:.3f} (range: {prev_stats_2023['min']:.3f} to {prev_stats_2023['max']:.3f})")
            
            positive_ext_2023 = sum(1 for r in transitions_2023 if r['recovery_rate_ext'] > 0)
            positive_prev_2023 = sum(1 for r in transitions_2023 if r['recovery_rate_prev'] > 0)
            
            print(f"  Sites with positive extent recovery: {positive_ext_2023}/{len(transitions_2023)} ({100*positive_ext_2023/len(transitions_2023):.1f}%)")
            print(f"  Sites with positive prevalence recovery: {positive_prev_2023}/{len(transitions_2023)} ({100*positive_prev_2023/len(transitions_2023):.1f}%)")
        else:
            ext_stats_2023 = prev_stats_2023 = {'count': 0, 'mean': 0, 'min': 0, 'max': 0}
            print("  No transitions found for this period")
        
        # Analyze 2024→2025 transition
        print("\n2024→2025 Transition Recovery:")
        if transitions_2024:
            ext_stats_2024 = calculate_stats(transitions_2024, 'recovery_rate_ext')
            prev_stats_2024 = calculate_stats(transitions_2024, 'recovery_rate_prev')
            
            print(f"  Sites analyzed: {len(transitions_2024)}")
            print(f"  Extent Recovery Rate: {ext_stats_2024['mean']:.3f} (range: {ext_stats_2024['min']:.3f} to {ext_stats_2024['max']:.3f})")
            print(f"  Prevalence Recovery Rate: {prev_stats_2024['mean']:.3f} (range: {prev_stats_2024['min']:.3f} to {prev_stats_2024['max']:.3f})")
            
            positive_ext_2024 = sum(1 for r in transitions_2024 if r['recovery_rate_ext'] > 0)
            positive_prev_2024 = sum(1 for r in transitions_2024 if r['recovery_rate_prev'] > 0)
            
            print(f"  Sites with positive extent recovery: {positive_ext_2024}/{len(transitions_2024)} ({100*positive_ext_2024/len(transitions_2024):.1f}%)")
            print(f"  Sites with positive prevalence recovery: {positive_prev_2024}/{len(transitions_2024)} ({100*positive_prev_2024/len(transitions_2024):.1f}%)")
        else:
            ext_stats_2024 = prev_stats_2024 = {'count': 0, 'mean': 0, 'min': 0, 'max': 0}
            print("  No transitions found for this period")
        
        # Store results
        self.analysis_results['recovery_patterns'] = {
            'transitions_2023': transitions_2023,
            'transitions_2024': transitions_2024,
            'stats_2023': {'extent': ext_stats_2023, 'prevalence': prev_stats_2023},
            'stats_2024': {'extent': ext_stats_2024, 'prevalence': prev_stats_2024}
        }
    
    def analyze_predictive_relationships(self):
        """Analyze predictive relationships between 2023 bleaching and 2024 recovery"""
        print("\n=== PREDICTIVE RELATIONSHIP ANALYSIS ===")
        
        if not hasattr(self, 'recovery_data'):
            self.prepare_recovery_analysis()
        
        # Prepare prediction dataset using 2023 data to predict 2024 outcomes
        data_2023 = {r['site']: r for r in self.recovery_data if r['transition_year'] == 2023}
        data_2024 = {r['site']: r for r in self.recovery_data if r['transition_year'] == 2024}
        
        # Find common sites
        common_sites = set(data_2023.keys()).intersection(set(data_2024.keys()))
        print(f"Sites available for prediction analysis: {len(common_sites)}")
        
        if len(common_sites) < 5:
            print("❌ Insufficient data for meaningful prediction analysis")
            return
        
        # Manual correlation calculation (since we don't have scipy)
        def calculate_correlation(x_values, y_values):
            n = len(x_values)
            if n < 2:
                return 0, 1
            
            # Calculate means
            mean_x = sum(x_values) / n
            mean_y = sum(y_values) / n
            
            # Calculate correlation coefficient
            numerator = sum((x_values[i] - mean_x) * (y_values[i] - mean_y) for i in range(n))
            sum_sq_x = sum((x_values[i] - mean_x) ** 2 for i in range(n))
            sum_sq_y = sum((y_values[i] - mean_y) ** 2 for i in range(n))
            
            if sum_sq_x == 0 or sum_sq_y == 0:
                return 0, 1
            
            correlation = numerator / math.sqrt(sum_sq_x * sum_sq_y)
            
            # Simple approximation for p-value
            t_stat = abs(correlation) * math.sqrt(n - 2) / math.sqrt(1 - correlation**2) if abs(correlation) < 1 else float('inf')
            p_value = 0.01 if t_stat > 3 else 0.05 if t_stat > 2 else 0.1
            
            return correlation, p_value
        
        # Analyze correlations
        predictors = {
            'Initial_Extent_2023': [data_2023[site]['initial_ext_total'] for site in common_sites],
            'Initial_Prevalence_2023': [data_2023[site]['initial_prev_total'] for site in common_sites],
            'Max_DHW_2023': [data_2023[site]['max_dhw'] for site in common_sites],
            'Recovery_Rate_Ext_2023': [data_2023[site]['recovery_rate_ext'] for site in common_sites],
            'Recovery_Rate_Prev_2023': [data_2023[site]['recovery_rate_prev'] for site in common_sites]
        }
        
        targets = {
            'Recovery_Rate_Ext_2024': [data_2024[site]['recovery_rate_ext'] for site in common_sites],
            'Recovery_Rate_Prev_2024': [data_2024[site]['recovery_rate_prev'] for site in common_sites]
        }
        
        print("\nCorrelation Analysis: 2023 Predictors vs 2024 Recovery")
        print("=" * 60)
        
        correlation_results = {}
        
        for target_name, target_values in targets.items():
            print(f"\nTarget: {target_name}")
            print("-" * 30)
            
            correlations = []
            for pred_name, pred_values in predictors.items():
                corr, p_val = calculate_correlation(pred_values, target_values)
                significance = '***' if p_val < 0.001 else '**' if p_val < 0.01 else '*' if p_val < 0.05 else ''
                
                correlations.append({
                    'predictor': pred_name,
                    'correlation': corr,
                    'p_value': p_val,
                    'significant': significance
                })
                
                print(f"  {pred_name:25}: r = {corr:6.3f}, p ≈ {p_val:6.3f} {significance}")
            
            correlation_results[target_name] = correlations
        
        # Store results
        self.analysis_results['prediction_analysis'] = {
            'correlation_results': correlation_results,
            'common_sites': list(common_sites),
            'predictors': predictors,
            'targets': targets
        }
    
    def generate_insights_report(self):
        """Generate a comprehensive insights report"""
        print("\n" + "=" * 70)
        print("CORAL BLEACHING RECOVERY INSIGHTS REPORT")
        print("=" * 70)
        
        # Create comprehensive report
        report_lines = []
        report_lines.append("# Coral Bleaching Recovery Analysis - Key Insights")
        report_lines.append(f"Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report_lines.append("")
        
        # Executive Summary
        report_lines.append("## Executive Summary")
        report_lines.append("")
        
        if 'recovery_patterns' in self.analysis_results:
            recovery_results = self.analysis_results['recovery_patterns']
            
            n_sites_2023 = len(recovery_results['transitions_2023'])
            n_sites_2024 = len(recovery_results['transitions_2024'])
            
            report_lines.append(f"**Dataset Overview:**")
            report_lines.append(f"- Analyzed {n_sites_2023} sites for 2023→2024 transition")
            report_lines.append(f"- Analyzed {n_sites_2024} sites for 2024→2025 transition")
            report_lines.append(f"- Generated from sample data for demonstration purposes")
            report_lines.append("")
            
            # Recovery patterns summary
            report_lines.append("## 1. Recovery Patterns")
            report_lines.append("")
            
            for year, stats_key in [('2023→2024', 'stats_2023'), ('2024→2025', 'stats_2024')]:
                stats = recovery_results[stats_key]
                
                report_lines.append(f"**{year} Transition:**")
                report_lines.append(f"- Mean extent recovery rate: {stats['extent']['mean']:.3f}")
                report_lines.append(f"- Mean prevalence recovery rate: {stats['prevalence']['mean']:.3f}")
                report_lines.append(f"- Extent recovery range: {stats['extent']['min']:.3f} to {stats['extent']['max']:.3f}")
                report_lines.append(f"- Prevalence recovery range: {stats['prevalence']['min']:.3f} to {stats['prevalence']['max']:.3f}")
                report_lines.append("")
        
        if 'prediction_analysis' in self.analysis_results:
            pred_results = self.analysis_results['prediction_analysis']
            
            report_lines.append("## 2. Predictive Relationships")
            report_lines.append("")
            
            # Top correlations
            for target_name, correlations in pred_results['correlation_results'].items():
                # Sort by absolute correlation
                sorted_corr = sorted(correlations, key=lambda x: abs(x['correlation']), reverse=True)
                
                if sorted_corr:
                    top_predictor = sorted_corr[0]
                    report_lines.append(f"**{target_name}:**")
                    report_lines.append(f"- Best predictor: {top_predictor['predictor']} (r = {top_predictor['correlation']:.3f})")
                    
                    if top_predictor['p_value'] < 0.05:
                        significance = "statistically significant"
                    else:
                        significance = "not statistically significant"
                    
                    report_lines.append(f"- Relationship is {significance}")
                    report_lines.append("")
        
        # Key insights
        report_lines.append("## 3. Key Biological Insights")
        report_lines.append("")
        
        report_lines.append("### Recovery Capacity")
        report_lines.append("- Coral recovery varies significantly between sites and time periods")
        report_lines.append("- Recovery patterns differ between extent and prevalence metrics")
        report_lines.append("- Some sites show positive recovery while others show continued decline")
        report_lines.append("")
        
        report_lines.append("### Predictive Factors")
        report_lines.append("- Previous bleaching severity may influence subsequent recovery capacity")
        report_lines.append("- DHW (thermal stress) appears to be an important factor in recovery outcomes")
        report_lines.append("- Site-specific characteristics likely play crucial roles in resilience")
        report_lines.append("")
        
        report_lines.append("### Management Implications")
        report_lines.append("- Sites with consistently poor recovery may benefit from targeted intervention")
        report_lines.append("- Monitoring programs should track recovery across multiple bleaching cycles")
        report_lines.append("- Early warning systems could help predict and prepare for recovery outcomes")
        report_lines.append("- Thermal stress management through local stressor reduction may improve resilience")
        report_lines.append("")
        
        report_lines.append("## 4. Methodological Notes")
        report_lines.append("")
        report_lines.append("- This analysis used realistically generated sample data for demonstration")
        report_lines.append("- Recovery rates calculated as proportional change from initial bleaching state")
        report_lines.append("- Analysis focuses on Annual→PBL transitions as recovery periods")
        report_lines.append("- Statistical relationships should be validated with real monitoring data")
        report_lines.append("- Framework can be directly applied to actual coral monitoring datasets")
        
        # Save report
        report_content = '\n'.join(report_lines)
        os.makedirs('outputs/reports', exist_ok=True)
        with open('outputs/reports/coral_bleaching_insights_report.md', 'w') as f:
            f.write(report_content)
        
        print("✓ Insights report generated and saved")
        print(f"✓ Report location: outputs/reports/coral_bleaching_insights_report.md")
        
        # Display key findings
        print("\n" + "🔍" * 20)
        print("KEY FINDINGS SUMMARY")
        print("🔍" * 20)
        
        if 'recovery_patterns' in self.analysis_results:
            recovery_results = self.analysis_results['recovery_patterns']
            n_total = len(recovery_results['transitions_2023']) + len(recovery_results['transitions_2024'])
            print(f"📊 Analyzed recovery patterns for {n_total} site transitions")
            
            if recovery_results['stats_2023']['extent']['count'] > 0:
                mean_ext_2023 = recovery_results['stats_2023']['extent']['mean']
                print(f"🐠 Mean extent recovery 2023→2024: {mean_ext_2023:.3f}")
            
            if recovery_results['stats_2024']['extent']['count'] > 0:
                mean_ext_2024 = recovery_results['stats_2024']['extent']['mean']
                print(f"🐠 Mean extent recovery 2024→2025: {mean_ext_2024:.3f}")
        
        if 'prediction_analysis' in self.analysis_results:
            pred_results = self.analysis_results['prediction_analysis']
            print(f"🔮 Predictive analysis included {len(pred_results['common_sites'])} sites")
            
            # Show top correlation
            for target_name, correlations in pred_results['correlation_results'].items():
                sorted_corr = sorted(correlations, key=lambda x: abs(x['correlation']), reverse=True)
                if sorted_corr:
                    top = sorted_corr[0]
                    print(f"📈 Best predictor for {target_name}: {top['predictor']} (r = {top['correlation']:.3f})")
    
    def run_complete_analysis(self):
        """Run the complete simplified analysis"""
        print("🌊 CORAL BLEACHING RECOVERY ANALYSIS (SIMPLIFIED) 🌊")
        print("=" * 70)
        print("Note: This is a simplified version using only built-in Python libraries.")
        print("For full analysis capabilities with visualizations, install the required packages:")
        print("  pip install pandas numpy matplotlib seaborn scipy scikit-learn")
        print("=" * 70)
        
        # Generate sample data
        self.generate_sample_data()
        
        # Prepare analysis
        self.prepare_recovery_analysis()
        
        # Run analyses
        self.analyze_recovery_patterns()
        self.analyze_predictive_relationships()
        
        # Generate report
        self.generate_insights_report()
        
        print("\n" + "🎉" * 20)
        print("ANALYSIS COMPLETE!")
        print("Generated files:")
        print("- outputs/data/sample_bleaching_data.csv")
        print("- outputs/data/sample_dhw_data.csv") 
        print("- outputs/data/recovery_analysis_data.csv")
        print("- outputs/reports/coral_bleaching_insights_report.md")
        print("🎉" * 20)

def main():
    """Run the simplified coral bleaching analysis"""
    analyzer = SimplifiedCoralAnalyzer()
    analyzer.run_complete_analysis()

if __name__ == "__main__":
    main()