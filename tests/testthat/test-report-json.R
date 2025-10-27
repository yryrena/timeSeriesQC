test_that("build_report_json writes a valid payload file", {
  df <- data.frame(
    id    = rep("A", 5),
    time  = as.Date("2024-01-01") + 0:4,
    value = c(1, 2, 3, 4, 100)
  )
  
  scan <- tsqc_scan(
    df,
    time  = time,
    id    = id,
    value = value,
    freq  = "daily"
  )
  
  fix  <- tsqc_fix(scan)
  
  tmp <- tempfile(fileext = ".json")
  build_report_json(scan, fix, tmp)
  
  expect_true(file.exists(tmp))
  
  back <- jsonlite::read_json(tmp, simplifyVector = TRUE)
  
  expect_true(is.list(back))
  expect_true(all(c("meta","issues","features","fix_log","sample") %in% names(back)))
  expect_equal(back$meta$freq_resolved, "daily")
})