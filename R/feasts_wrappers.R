#' Internal wrapper standing in for feasts::trend()
#' DO NOT call feasts::trend() here because it's not exported and can
#' break tests / CRAN checks. Just return a tagged list that the
#' code downstream only ever uses inside formulas passed to STL().
#'
#' @keywords internal
#' @noRd
tsqc_trend <- function(window = NULL) {
  ## make a tiny spec object we can inspect / test
  list(
    kind   = "trend",
    window = window
  )
}

#' Internal wrapper standing in for feasts::season() 
#'
#' @keywords internal
#' @noRd
tsqc_season <- function(period = NULL) {
  list(
    kind   = "season",
    period = period
  )
}