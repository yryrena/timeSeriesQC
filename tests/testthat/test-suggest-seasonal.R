test_that("tsqc_suggest sets suggest_seasadj TRUE when seasonality is strong", {
  ## synthetic obviously-seasonal data
  set.seed(123)
  n <- 30
  df <- data.frame(
    id    = "A",
    time  = as.Date("2024-01-01") + 0:(n - 1),
    value = sin(2 * pi * (0:(n - 1)) / 7) + rnorm(n, sd = 0.05)
  )
  
  ### build a minimal fake tsqc_scan object instead of calling tsqc_scan()  
  fake_scan <- list(
    meta = list(
      raw_df = df,
      freq_resolved = "daily"
    )
  )
  class(fake_scan) <- "tsqc_scan"
  
  ### mock seasonal strength so we don't hit feasts::STL at all 
  fake_sss <- function(vals, fp_period) {
    ## pretend we detected strong seasonality
    list(strength = 0.9, ok = TRUE)
  }
  
  testthat::local_mocked_bindings(
    safe_season_strength = fake_sss,
    .package = "timeSeriesQC"
  )
  
  out <- tsqc_suggest(fake_scan)
  
  expect_true("suggest_seasadj" %in% names(out))
  expect_equal(out$suggest_seasadj, TRUE)
})