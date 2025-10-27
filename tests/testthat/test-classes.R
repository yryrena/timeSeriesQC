test_that("tsqc_scan print works", {
  df <- data.frame(
    id    = "A",
    time  = as.Date("2024-01-01") + 0:2,
    value = c(1,2,3)
  )
  sc <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
  expect_s3_class(sc, "tsqc_scan")
  expect_invisible(print(sc))
})

test_that("tsqc_fix print works", {
  df <- data.frame(
    id    = c("A","A"),
    time  = as.Date("2024-01-01") + 0:1,
    value = c(1,2)
  )
  sc <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
  fx <- tsqc_fix(sc, rules = c("dedup"))
  expect_s3_class(fx, "tsqc_fix")
  expect_invisible(print(fx))
})