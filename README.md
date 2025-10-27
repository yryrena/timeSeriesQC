
# timeSeriesQC

<!-- badges: start -->

[![R-CMD-check](https://github.com/yryrena/timeSeriesQC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yryrena/timeSeriesQC/actions)
[![Codecov test
coverage](https://codecov.io/gh/yryrena/timeSeriesQC/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yryrena/timeSeriesQC)
<!-- badges: end -->

**timeSeriesQC** provides *preflight quality control (QC)* for panel or long-format time series data.

Before modeling or forecasting, analysts often spend significant time inspecting and cleaning data for structural and statistical issues.  
Existing R packages (`forecast`, `fable`, `prophet`, etc.) focus on modeling, not on **systematic, scalable quality checks**.  
This package fills that gap by offering an automated, standardized QC pipeline for time series datasets.
 

## Core Functionality

The package implements a pipeline:

1.  **Scan** → identify issues in panel/long-format time series  
2.  **Fix** → apply standardized repair methods 
3.  **Report** → summarize issues, features, and applied fixes in an HTML report  

## Main Functions

**`tsqc_scan()`** scans time series data for common issues:

- duplicates  
- gaps (for regular series)  
- point outliers (using MAD or STL residuals)  
- structural breakpoints (if `strucchange` is available)  
- per-series features such as KPSS/ADF p-values (`tseries`) and seasonal strength (STL decomposition via `feasts`)


**`tsqc_fix()`** applies fixes such as:

- deduplication  
- gap filling  
- interpolation  
- winsorization (options include `method = "locf"`, `max_run`, etc.)

You can control repair behavior via options like `method = "locf"`, `max_run`, and quantile thresholds.

 
**`tsqc_suggest()`** generates transformation suggestions, not direct fixes, based on series diagnostics:
 
- log vs shifted-log transformation (based on sign of data)  
- first differencing recommendation (based on lag-1 autocorrelation)  
- seasonal adjustment suggestion (via STL seasonal strength > 0.2)

Internally, `timeSeriesQC` uses safe wrappers (`tsqc_trend()`, `tsqc_season()`) instead of directly calling unexported `feasts` functions, ensuring robust CRAN and CI compatibility.


**`tsqc_report()`** produces an interactive HTML report that summarizes:

- QC issues and fixes per series  
- seasonal and stationarity diagnostics  
- preview plots of the data before and after QC  



## Installation

``` r
# install.packages("pak")
pak::pak("yryrena/timeSeriesQC")

# alternatively, with remotes:
# install.packages("remotes")
# remotes::install_github("yryrena/timeSeriesQC")
```

## Quick Start

``` r
library(timeSeriesQC)

set.seed(1)
n <- length(seq.Date(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"))

d1 <- data.frame(
  id    = rep("A", n),
  time  = seq.Date(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
  value = c(rnorm(45, 10, 1), rep(NA, 5), rnorm(n - 50, 10, 1))
)
d1$value[20] <- 20  # inject an outlier

d2 <- data.frame(
  id    = rep("B", n),
  time  = seq.Date(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
  value = rnorm(n, 5, 2)
)

df <- rbind(d1, d2)

## step 1: Scan for quality issues
qc <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")

## step 2: Apply automated fixes
fix <- tsqc_fix(qc, rules = c("dedup", "fill_gaps", "interpolate", "winsorize"),
                method = "locf", max_run = 7, lower = 0.01, upper = 0.99)

## step 3: Generate transformation suggestions
sug <- tsqc_suggest(qc)

qc
fix
head(sug)

## step 4: Render HTML report
tsqc_report(qc, fix, out = "qc_report.html")
```

## Irregular Panel Example

Gap detection also works for irregularly spaced panel data:

``` r
df_irreg <- data.frame(
  id    = rep(c("X", "Y"), each = 15),
  time  = c(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 15),
            seq.Date(as.Date("2024-01-05"), by = "2 days", length.out = 15)),
  value = rnorm(30)
)

qc_irreg <- tsqc_scan(df_irreg, time = time, id = id, value = value, freq = "daily")
qc_irreg$issues  # shows detected gaps
```

## Optional: X-13 Seasonal Adjustment Support

To enable seasonal adjustment using the `seasonal` package:

``` r
install.packages("x13binary")
seasonal::checkX13()    ## should return TRUE
```
 
