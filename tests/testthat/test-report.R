test_that("export_payload builds expected structure", {
  df <- data.frame(
    id    = rep("A", 5),
    time  = seq.Date(as.Date("2024-01-01"), by="day", length.out=5),
    value = c(1,2,3,4,100)
  )
  scan <- tsqc_scan(df, time=time, id=id, value=value, freq="daily")
  fix  <- tsqc_fix(scan)
  
  payload <- export_payload(scan, fix)
  
  expect_true(is.list(payload))
  expect_true(all(c("meta","issues","features","fix_log","sample") %in% names(payload)))
  expect_true(is.data.frame(payload$issues))
  expect_true(is.data.frame(payload$sample))
  
  ## basic json roundtrip
  tmp <- tempfile(fileext=".json")
  jsonlite::write_json(payload, tmp, auto_unbox=TRUE, pretty=TRUE)
  back <- jsonlite::read_json(tmp, simplifyVector = TRUE)
  expect_true(is.list(back))
})