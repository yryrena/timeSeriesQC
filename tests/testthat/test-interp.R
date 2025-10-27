test_that("interp_runs_one_series fills short NA runs with locf", {
  d <- data.frame(
    time  = as.Date("2024-01-01") + 0:6,
    value = c(1, 2, NA, NA, 5, NA, 7)
  )
  ## runs: [3:4] length 2, [6] length 1
  filled <- interp_runs_one_series(d, method = "locf", max_run = 2L)
  
  ## short runs (<=2) should be filled
  expect_false(any(is.na(filled$value[3:4])))
  expect_false(is.na(filled$value[6]))
})

test_that("interp_runs_one_series leaves long NA run untouched", {
  d <- data.frame(
    time  = as.Date("2024-01-01") + 0:6,
    value = c(1, NA, NA, NA, 5, 6, 7)    ## run length 3
  )
  
  filled <- interp_runs_one_series(d, method = "locf", max_run = 2L)
  
  ## long run (>2) should remain NA
  expect_true(all(is.na(filled$value[2:4])))
})