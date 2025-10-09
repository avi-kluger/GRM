# Create README.md file
readme_content <- '# GRM: Comprehensive Graded Response Model Analysis

[![R-CMD-check](https://github.com/YOUR_USERNAME/GRM/workflows/R-CMD-check/badge.svg)](https://github.com/YOUR_USERNAME/GRM/actions)

A comprehensive R package for conducting Graded Response Model (GRM) analysis using Item Response Theory. Provides a streamlined workflow for psychometric analysis of polytomous items with automated reporting and visualization.

## Features

- **One-function analysis**: Complete GRM workflow with `run_grm()`
- **Comprehensive diagnostics**: Reliability, dimensionality, model fit, local dependency
- **Automatic visualization**: Publication-ready plots saved and displayed
- **Best items identification**: Find optimal items for different ability levels
- **Multidimensional support**: Handles both unidimensional and multidimensional models
- **Enhanced reporting**: Automatic console display of key results

## Installation

Install from GitHub using:

```r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install GRM package
devtools::install_github("YOUR_USERNAME/GRM", build_vignettes = TRUE)