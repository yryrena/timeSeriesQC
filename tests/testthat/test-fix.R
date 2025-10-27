test_that("tsqc_fix applies dedup, gap fill, interpolate, winsorize", {
  set.seed(1)
  tseq <- seq.Date(as.Date("2024-01-01"), by="day", length.out=5)
  
  df <- data.frame(
    id    = c(rep("A",6), rep("B",5)),
    time  = c(tseq[1:5], tseq[5], tseq),
    value = c(10,10,10,10,999,10,  1,2,NA,4,500)
  )
  
  scan <- tsqc_scan(df, time = time, id = id, value = value, freq="daily")
  
  fixed <- tsqc_fix(
    scan,
    rules = c("dedup","fill_gaps","interpolate","winsorize"),
    method = "locf",
    max_run = 2,
    lower = 0.01,
    upper = 0.99
  )
  
  expect_s3_class(fixed, "tsqc_fix")
  expect_true(is.data.frame(fixed$data))
  expect_true(is.data.frame(fixed$log))
  
  ## sanity: log has steps
  expect_gt(nrow(fixed$log), 0)
  
  ## sanity: columns exist
  expect_true(all(c("step", "rule", "changed_count") %in% names(fixed$log)))
})