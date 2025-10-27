test_that("tsqc_suggest returns suggestions per id", {
  df <- data.frame(
    id    = rep("A", 10),
    time  = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    value = c(1,2,3,4,5,6,7,8,9,10)
  )
  
  scan <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
  sug  <- tsqc_suggest(scan)
  
  expect_true(is.data.frame(sug))
   
  expect_true(all(c("id",
                    "suggest_log",
                    "shift",
                    "suggest_diff",
                    "suggest_seasadj",
                    "notes") %in% names(sug)))
  
  ## single id -> one row
  expect_equal(nrow(sug), 1L)
  
  ## suggest_log should be "log" or "shifted_log"
  expect_true(sug$suggest_log[1] %in% c("log","shifted_log"))
  
  ## shift should be numeric >=0
  expect_true(is.numeric(sug$shift[1]))
  expect_true(sug$shift[1] >= 0)
  
  ## suggest_diff should be integer-ish (0 or 1)
  expect_true(sug$suggest_diff[1] %in% c(0L,1L))
  
  ## suggest_seasadj is logical
  expect_true(is.logical(sug$suggest_seasadj[1]))
})