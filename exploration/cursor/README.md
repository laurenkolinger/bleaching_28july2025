# Coral Bleaching Recovery Analysis

This project analyzes coral bleaching recovery patterns and predictive relationships between previous bleaching events and subsequent coral responses.

## Research Questions

1. **Recovery Patterns**: How do corals recover between Annual and PBL monitoring periods?
2. **Predictive Relationships**: Can 2023 bleaching patterns predict 2024 coral responses?
3. **Thermal Stress Role**: What role does DHW (Degree Heating Weeks) play in recovery predictions?
4. **Resilience Insights**: What insights can we extract about coral resilience patterns?

## Analysis Focus

- **2024 coral response**: Changes from 2024 Annual to 2025 PBL
- **2023 bleaching predictors**: Use 2023 Annual to 2024 PBL patterns to predict 2024 responses
- **DHW integration**: Include thermal stress data as additional predictor
- **Recovery metrics**: Analyze both extent and prevalence recovery patterns

## Project Structure

```
exploration/cursor/
├── README.md                                    # This file
├── requirements.txt                             # Python dependencies
├── coral_bleaching_analysis.py                 # Full analysis (requires pandas, etc.)
├── coral_analysis_simple.py                    # Simplified analysis (built-in libs only)
├── run_analysis.py                             # Simple runner script
├── coral_bleaching_exploration.ipynb           # Jupyter notebook for interactive analysis
├── setup_environment.sh                        # Environment setup script
└── outputs/                                    # Generated analysis outputs
    ├── plots/                                  # Visualization outputs
    ├── data/                                   # Processed data files
    └── reports/                                # Analysis reports
```

## Expected Data Files

The analysis expects these data files in `/workspace/data/`:

1. **s3pt1_coralcondition_bleachingextent_33sites_2022_2025.csv**
   - Coral bleaching extent measurements
   - Columns: site, year, period, date, ext_total, ext_bleached, etc.

2. **s3pt1_coralcondition_bleachingprevalence_33sites_2022_2025.csv**
   - Coral bleaching prevalence measurements  
   - Columns: site, year, period, date, prev_total, prev_bleached, ncolonies, etc.

3. **s4pt5_temperatureWeeklyDHW_45sites_2003_2025.csv**
   - Weekly temperature and DHW data
   - Columns: year, week, site, dhw, weekly_max_temp, BT, annual_max, etc.

## Usage

### Option 1: Full Analysis (Recommended)
Requires: pandas, numpy, matplotlib, seaborn, scipy, scikit-learn

```bash
# Install dependencies
pip install -r requirements.txt

# Run complete analysis
python3 coral_bleaching_analysis.py

# Or use the runner
python3 run_analysis.py
```

### Option 2: Simplified Analysis (No Dependencies)
Uses only built-in Python libraries, generates sample data for demonstration:

```bash
python3 coral_analysis_simple.py
```

### Option 3: Interactive Jupyter Notebook
```bash
# Install jupyter if needed
pip install jupyter

# Start notebook server
jupyter notebook coral_bleaching_exploration.ipynb
```

## Analysis Methodology

### 1. Data Preparation
- Load and validate coral bleaching and DHW datasets
- Check for common sites across datasets
- Convert date formats and create weekly matches

### 2. Recovery Calculation
- Identify Annual → PBL transitions for each site
- Calculate recovery metrics:
  - Absolute change: `PBL_value - Annual_value`
  - Recovery rate: `(Annual_value - PBL_value) / (Annual_value + 0.1)`
- Focus on 2023→2024 and 2024→2025 transitions

### 3. Predictive Analysis
- Use 2023 data as predictors for 2024 recovery
- Predictors include:
  - Initial bleaching severity (extent/prevalence)
  - Previous recovery performance
  - Maximum DHW (thermal stress)
- Statistical correlations and machine learning models

### 4. Pattern Analysis
- Recovery rate distributions
- Site-specific resilience patterns
- Relationship between thermal stress and recovery
- Temporal trends across monitoring periods

## Key Outputs

1. **Recovery Pattern Plots**
   - Annual vs PBL bleaching comparisons
   - Recovery rate distributions
   - Site-specific performance trends

2. **Predictive Relationship Analysis**
   - Correlation matrices
   - Scatter plots with trend lines
   - Machine learning model performance

3. **Comprehensive Report**
   - Executive summary with key findings
   - Statistical results and interpretations
   - Management implications
   - Biological insights

## Key Findings (Example)

Based on the analysis framework, typical insights include:

### Recovery Capacity
- Coral recovery varies significantly between sites and time periods
- Some sites show remarkable resilience with positive recovery rates
- Recovery patterns differ between extent and prevalence metrics

### Predictive Factors
- Previous bleaching severity influences subsequent recovery capacity
- DHW (thermal stress) remains an important predictor
- Site-specific factors likely play crucial roles in recovery

### Management Implications
- Sites with consistently poor recovery may need targeted intervention
- Early warning systems based on thermal stress can inform management
- Recovery monitoring should continue across multiple bleaching cycles

## Technical Notes

### Recovery Rate Calculation
Recovery rate is calculated as proportional improvement:
```
recovery_rate = (initial_bleaching - recovery_bleaching) / (initial_bleaching + 0.1)
```
- Positive values indicate recovery (reduced bleaching)
- Negative values indicate worsening (increased bleaching)
- Small constant (0.1) prevents division by zero

### Data Quality Considerations
- Missing data is handled with appropriate defaults
- Outliers are identified but not automatically removed
- Site matching between datasets is carefully validated
- Temporal alignment uses survey dates for accurate matching

## Environment Setup

If you encounter dependency issues:

```bash
# Create virtual environment
python3 -m venv coral_env
source coral_env/bin/activate

# Install packages
pip install pandas numpy matplotlib seaborn scipy scikit-learn jupyter

# Run analysis
python3 coral_bleaching_analysis.py
```

For system package installation (Ubuntu/Debian):
```bash
sudo apt update
sudo apt install python3-pandas python3-numpy python3-matplotlib python3-seaborn python3-scipy python3-sklearn python3-jupyter
```

## Contributing

To extend this analysis:

1. **Add new metrics**: Modify the data loading and calculation functions
2. **Include additional predictors**: Extend the predictive modeling section
3. **Add visualizations**: Create new plotting functions in the analyzer class
4. **Improve models**: Experiment with different machine learning approaches

## Contact

For questions about this analysis framework or to request specific modifications, please refer to the research team conducting the coral bleaching study.

---

*Generated as part of coral bleaching recovery analysis project - January 2025*