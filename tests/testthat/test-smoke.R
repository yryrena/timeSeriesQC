test_that("tsqc_scan and tsqc_fix run on a toy daily panel", {
  df <- data.frame(
    id    = "A",
    time  = as.Date("2024-01-01") + 0:9,
    value = c(1:5, NA, 7:10)
  )
  
  qc <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
  expect_s3_class(qc, "tsqc_scan")
  expect_true(is.list(qc$meta))
  expect_true(is.data.frame(qc$issues))
  
  fx <- tsqc_fix(qc, rules = c("fill_gaps","interpolate","winsorize"),
                 method = "locf", max_run = 2, lower = 0.01, upper = 0.99)
  expect_s3_class(fx, "tsqc_fix")
  expect_true(is.data.frame(fx$data))
  expect_true(nrow(fx$data) >= nrow(df))    ## fill_gaps may expand rows
})

test_that("helpers exist", {
  expect_true(is.function(get("build_regular_grid", envir = asNamespace("timeSeriesQC"))))
  expect_true(is.function(get("make_preview_sample", envir = asNamespace("timeSeriesQC"))))
})
