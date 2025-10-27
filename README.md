
# timeSeriesQC

<!-- badges: start -->

[![R-CMD-check](https://github.com/yryrena/timeSeriesQC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yryrena/timeSeriesQC/actions)
[![Codecov test
coverage](https://codecov.io/gh/yryrena/timeSeriesQC/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yryrena/timeSeriesQC)
<!-- badges: end -->

**timeSeriesQC** provides preflight quality control (QC) for panel/long
time series data.

In time series forecasting and modeling, analysts often spend
significant time cleaning and checking data before modeling. Existing R
packages (`forecast`, `fable`, `prophet`, etc.) focus on modeling, not
**data quality checks at scale**. This package focuses on **systematic
QC for time series datasets**.

## Core Functionality

The package implements a pipeline:

1.  **Scan** → identify issues in panel/long-format time series.  
2.  **Fix** → apply standardized repair methods.  
3.  **Report** → summarize issues and fixes in an HTML report.

## Main Functions

**`tsqc_scan()`** scans time series data for common issues:

- Duplicates  
- Gaps (for regular series)  
- Point outliers (using MAD or STL residuals)  
- Structural breakpoints (if `strucchange` is available)  
- Series features (KPSS/ADF hints via `tseries`, seasonal strength via
  STL when feasible)

**`tsqc_fix()`** applies fixes such as:

- Deduplication  
- Gap filling  
- Interpolation  
- Winsorization (options include `method = "locf"`, `max_run`, etc.)

**`tsqc_suggest()`** generates recommended fixes based on issues found.

**`tsqc_report()`** produces an HTML report with summary tables, plots,
and traceable records of applied fixes.

## Installation

``` r
# install.packages("pak")
pak::pak("yryrena/timeSeriesQC")

# or with remotes:
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

df  <- rbind(d1, d2)

qc  <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
fix <- tsqc_fix(qc, rules = c("dedup","fill_gaps","interpolate","winsorize"),
                method = "locf", max_run = 7, lower = 0.01, upper = 0.99)
sug <- tsqc_suggest(qc)

qc
fix
head(sug)

tsqc_report(qc, fix, out = "qc_report.html")
```

## Irregular Panel Example

Gap detection also works for irregular panels:

``` r
df_irreg <- data.frame(
  id    = rep(c("X","Y"), each = 15),
  time  = c(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 15),
            seq.Date(as.Date("2024-01-05"), by = "2 days", length.out = 15)),
  value = rnorm(30)
)

qc_irreg <- tsqc_scan(df_irreg, time = time, id = id, value = value, freq = "daily")
qc_irreg$issues  # shows detected gaps
```

## Optional: X-13 Seasonal Adjustment

If you plan to use X-13 via the `seasonal` package:

``` r
install.packages("x13binary")
seasonal::checkX13()  # should be TRUE
```

## Knit it

From the package root:

``` r
rmarkdown::render("README.Rmd")
```
