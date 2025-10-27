test_that("tsqc_report falls back and calls rmarkdown::render", {
  skip_if_not_installed("rmarkdown")
  
  ## tiny scan
  df <- data.frame(
    id    = "A",
    time  = as.Date("2024-01-01") + 0:2,
    value = c(1, 2, 3)
  )
  scan <- tsqc_scan(df, time = time, id = id, value = value, freq = "daily")
  
  fake_pkg_avail <- function(pkg) {
    if (pkg == "quarto")    return(FALSE)    ## pretend quarto not available
    if (pkg == "rmarkdown") return(TRUE)     ## pretend rmarkdown available
    timeSeriesQC:::pkg_avail(pkg)            ## defer otherwise
  }
  
  fake_render <- function(input,
                          output_file = NULL,
                          ...) { 
    tmp <- tempfile(fileext = ".html")
    writeLines("<html><body>fake report</body></html>", tmp)
    tmp
  }
  
  ## branch on testthat version for CI compatibility
  if (utils::packageVersion("testthat") >= "3.3.0") {
    
    ## new-style mocking
    testthat::local_mocked_bindings(
      pkg_avail = fake_pkg_avail,
      .package  = "timeSeriesQC",
      
      render    = fake_render,
      .package  = "rmarkdown"
    )
    
    out <- tsqc_report(scan)
    
    expect_type(out, "character")
    expect_match(out, "\\.html$")
    
  } else {
    
    ## fallback for old testthat  
    out <- testthat::with_mock(
      `timeSeriesQC::pkg_avail` = fake_pkg_avail,
      `rmarkdown::render`       = fake_render,
      {
        tsqc_report(scan)
      }
    )
    
    expect_type(out, "character")
    expect_match(out, "\\.html$")
     
    succeed()
  }
})