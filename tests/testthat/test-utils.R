test_that("standardize_df works with and without id", {
  df <- data.frame(
    unit = c("A","A","B"),
    t    = as.Date("2024-01-01") + 0:2,
    val  = c(1,2,3)
  )
  
  out1 <- standardize_df(df, time = t, id = unit, value = val)
  expect_true(all(c("id","time","value") %in% names(out1)))
  expect_equal(nrow(out1), 3)
  expect_equal(as.character(out1$id[1]), "A")
  
  out2 <- standardize_df(df, time = t, value = val)
  expect_true(all(c("id","time","value") %in% names(out2)))
  expect_true(all(out2$id == 1L))
})

test_that("infer_frequency returns daily/weekly/monthly/irregular", {
  daily_df <- data.frame(
    id=rep("X",5),
    time=as.Date("2024-01-01")+0:4,
    value=1:5
  )
  expect_equal(infer_frequency(daily_df), "daily")
})
 
test_that("flag_outliers_mad flags extreme values", {
  set.seed(123)
  x <- c(rnorm(20, mean = 10, sd = 1), 50)    ## 50 is a huge spike
  f <- flag_outliers_mad(x, k = 3.5)
  
  expect_true(any(f))
  
  ## the last point (value 50) should be flagged
  expect_true(f[length(f)])
})
