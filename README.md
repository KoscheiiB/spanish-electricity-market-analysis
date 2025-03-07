# Spanish Electricity Market Concentration Analysis

## Overview
This repository contains a comprehensive tool for analyzing the concentration of the Spanish electricity market using data from OMIE (Operador del Mercado Ibérico de Energía) for the years 2020-2024. The analysis focuses on both purchase and production data for Spain, calculating various static and dynamic concentration indices to evaluate market power and competitiveness.

## Features
- Automatic data download from OMIE's website
- Calculation of key concentration metrics:
  - **Static Indices**:
    - R(k) (Concentration Ratios for k=1,2,3)
    - HHI (Herfindahl-Hirschman Index)
    - HTI (Hall-Tideman Index)
    - EI (Entropy Index)
    - EXP (Exponential Index)
    - CCI (Comprehensive Concentration Index)
  - **Dynamic Indices**:
    - Instability Index between consecutive years
- Visual representation of concentration trends
- Summary statistics exports in CSV format

## Requirements
- R (>= 4.0.0)
- Required packages:
  - logger (for logging operations)
  - readxl (for Excel file handling)
  - ggplot2 (for data visualization)
  - here (for project path management)

## Project Structure
```
spanish-electricity-market-analysis/
├── data/                  # Downloaded data files from OMIE
├── results/               # Output files, visualizations, and logs
│   ├── static_concentration_summary.csv   # Summary of static indices
│   ├── dynamic_instability_summary.csv    # Summary of instability indices
│   ├── market_concentration_results.RData # R data file with complete results
│   ├── hhi_plot.png                       # HHI visualization
│   ├── r1_plot.png                        # R(1) visualization
│   └── ...                                # Other visualization files
├── OMIE_Analysis.R        # Main analysis script
└── README.md              # This file
```

## Installation
Clone this repository to your local machine:

```bash
git clone https://github.com/KoscheiiB/spanish-electricity-market-analysis.git
cd spanish-electricity-market-analysis
```

## Usage
1. Open the `OMIE_Analysis.R` script in your R environment
2. Run the entire script:

```r
source("OMIE_Analysis.R")
```

By default, the script will:
- Download missing data files from OMIE
- Calculate all concentration indices
- Generate visualization plots for each index
- Save summary statistics to CSV files
- Create a log file with execution details

## Sample Output
The analysis generates multiple visualizations:

1. **HHI Index Plot**: Shows the Herfindahl-Hirschman Index for both purchase and production data over time
2. **R(k) Index Plots**: Shows the market share of the largest firm(s) in both segments (k=1,2,3)
3. **HTI Index Plot**: Shows the Hall-Tideman Index for both segments
4. **EI Index Plot**: Shows the Entropy Index for both segments
5. **EXP Index Plot**: Shows the Exponential Index for both segments
6. **CCI Index Plots**: Shows the Comprehensive Concentration Index for both segments (k=1,2,3)
7. **Instability Plot**: Shows the market instability between consecutive years

Results are saved in the `results/` directory.

## How to Interpret Results (Example/Sample)
- **HHI**: Values above 0.25 indicate high concentration, 0.15-0.25 moderate concentration, below 0.15 low concentration
- **R(1)**: The market share of the largest firm; values above 0.5 indicate dominant position
- **Instability Index**: Values close to 0 indicate market stability, values close to 1 indicate high market volatility

## Acknowledgments
- OMIE (Operador del Mercado Ibérico de Energía) for providing the market data
- References for concentration indices can be found in the [Concentration Indexes reference file](./Concentration_Indexes_Ref.pdf)
