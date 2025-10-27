test_that("tsqc_trend / tsqc_season return spec-like lists", {
  tr <- tsqc_trend()
  tr7 <- tsqc_trend(window = 7)
  se <- tsqc_season()
  se7 <- tsqc_season(period = 7)
  
  expect_type(tr, "list")
  expect_equal(tr$kind, "trend")
  expect_null(tr$window)
  
  expect_type(tr7, "list")
  expect_equal(tr7$kind, "trend")
  expect_equal(tr7$window, 7)
  
  expect_type(se, "list")
  expect_equal(se$kind, "season")
  expect_null(se$period)
  
  expect_type(se7, "list")
  expect_equal(se7$kind, "season")
  expect_equal(se7$period, 7)
})