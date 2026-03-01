# coxdiag

An R package for automated diagnostics of Cox proportional hazards models.

## Overview

`coxdiag` takes a time-to-event dataset and a model formula as input, fits a
Cox regression model, checks the proportional hazards (PH) assumption, generates
diagnostic plots, and returns a structured summary of results.

## Installation

You can install the development version from GitHub:
```r
# install.packages("devtools")
devtools::install_github("douyadou0304-dev/coxdiag")
```

## Usage
```r
library(coxdiag)
library(survival)

# Run full diagnostics on the lung cancer dataset
result <- run_diagnostics(lung, Surv(time, status) ~ age + sex + ph.ecog)

# View Cox model coefficients
print(result$model)

# View proportional hazards test results
print(result$ph_test)

# View Schoenfeld residuals plot
print(result$ph_plot)

# View Martingale residuals plot
print(result$martingale_plot)

# Print human-readable summary
print_summary(result)
```

## Functions

- `run_diagnostics(data, model_formula)`: Fits a Cox model and runs all diagnostics. Returns a list containing the model, PH test results, and diagnostic plots.
- `print_summary(diag_result)`: Prints a formatted, human-readable summary of the diagnostic results.

## Example Output

When run on the `lung` dataset with formula `Surv(time, status) ~ age + sex + ph.ecog`:

- **sex** (HR = 0.575, p = 0.001): Female patients have significantly lower mortality risk
- **ph.ecog** (HR = 1.590, p < 0.001): Higher ECOG score associated with worse survival
- **age** (HR = 1.011, p = 0.232): Not significant after adjusting for other covariates
- **PH assumption**: All variables pass the Schoenfeld test (global p = 0.22)

## Dependencies

- `survival`: Cox model fitting and PH assumption testing
- `survminer`: Diagnostic plot generation
- `ggplot2`: Plot rendering
