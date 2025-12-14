# ETF-Factor-Exposure-App
This Shiny application analyzes the factor exposures of ETFs using the Fama–French 3-Factor Model, with region-specific factor data and rolling regressions to detect style drift over time.

The tool is designed for:

quick diagnostics of ETF factor tilts,
methodological correctness across geographic regions,
transparent and reproducible factor modeling.
Unlike many simplified factor tools, this app matches ETFs with the appropriate regional Fama–French factor datasets, avoiding a common source of empirical bias.

Key Features

Regional Factor Selection
Users can select the appropriate Fama–French dataset depending on the ETF’s investment universe:

USA
Europe
Developed ex US
Emerging Markets
Each selection loads the corresponding official dataset from Kenneth French’s data library.

Static Factor Regression
For a selected ETF and time period, the app estimates: ExcessReturn = Alpha + Beta_MKT × (Mkt − RF) + Beta_SMB × SMB + Beta_HML × HML + Error

Outputs include:

estimated factor betas,
confidence intervals,
statistical significance​
Rolling Factor Betas
The app computes rolling-window regressions (e.g. 12 months) to visualize:

time-varying factor exposures,
structural breaks,
style drift (e.g. growth → value, large → small).
This is particularly useful for:

ETF due diligence,
monitoring passive strategy purity,
understanding regime dependence.
Robust Data Handling
The app includes explicit cleaning steps to handle:

mixed monthly and annual rows in Fama–French CSVs,
non-numeric characters in factor returns,
mismatched calendar dates,
insufficient sample sizes for rolling estimation.
This ensures econometrically consistent inputs.

Data Sources

ETF Prices: Yahoo Finance (via quantmod)
Factor Returns: Kenneth R. French – Data Library (Official CSV files for each geographic region)
All factor returns are expressed in percent per month, consistent with the original datasets.

Why This Tool Is Useful Practical Relevance

Quickly assess whether an ETF truly delivers the factor exposure it claims.
Detect unintended tilts or drifting investment styles.
Compare ETFs across regions on a consistent methodological basis.
Methodological Strength

Avoids applying US factor premia to non-US assets.
Uses excess returns and correct time alignment.
Makes rolling estimation transparent and reproducible.
Uniqueness, Most publicly available factor tools:

assume US factors by default,
ignore regional heterogeneity,
provide only static exposures.
This app explicitly addresses all three issues.

Typical Use Cases

Portfolio construction and ETF selection
Factor investing research
Academic replication and teaching
How to Run

install required packages
install.packages(c( "shiny", "tidyverse", "lubridate", "quantmod", "broom", "zoo" ))

run the app
shiny::runApp()

Disclaimer

This tool is for educational and research purposes only. It does not constitute investment advice.

Author

Developed by Maximilian Peter MSc Economics & Data Science Focus: empirical asset pricing, factor investing, applied econometrics
