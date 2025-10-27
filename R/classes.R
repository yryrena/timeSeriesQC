#' Internal constructors & print methods for timeSeriesQC objects
#' @name timeSeriesQC-classes
#' @keywords internal
#' @importFrom fabletools model components
NULL

#' Internal constructor for tsqc_scan objects
#' @keywords internal
#' @noRd
new_tsqc_scan <- function(meta, issues, features = NULL, preview = NULL) {
  ## normalize shapes
  issues   <- dplyr::as_tibble(issues)
  if (!is.null(features)) features <- dplyr::as_tibble(features)
  if (!is.null(preview))  preview  <- dplyr::as_tibble(preview)
  
  ## basic meta sanity
  stopifnot(
    is.list(meta),
    all(c("n_series","nrow_raw","start","end","freq_resolved","tz","raw_df") %in% names(meta))
  )
  
  structure(
    list(
      meta     = meta,
      issues   = issues,
      features = features,
      preview  = preview
    ),
    class = "tsqc_scan"
  )
}

#' Internal constructor for tsqc_fix objects
#' @keywords internal
#' @noRd
new_tsqc_fix <- function(data_fixed, fix_log) {
  ## normalize log types
  fix_log <- dplyr::as_tibble(fix_log)
  if (!"step"          %in% names(fix_log)) fix_log$step          <- integer(nrow(fix_log))
  if (!"rule"          %in% names(fix_log)) fix_log$rule          <- character(nrow(fix_log))
  if (!"params"        %in% names(fix_log)) fix_log$params        <- character(nrow(fix_log))
  if (!"changed_count" %in% names(fix_log)) fix_log$changed_count <- integer(nrow(fix_log))
  
  fix_log <- dplyr::mutate(
    fix_log,
    step          = as.integer(step),
    rule          = as.character(rule),
    params        = as.character(params),
    changed_count = as.integer(changed_count)
  )
  
  structure(
    list(data = data_fixed, log = fix_log),
    class = "tsqc_fix"
  )
}

#' @export
print.tsqc_scan <- function(x, ...) {
  cat("<tsqc_scan>\n")
  with(x$meta, cat(
    sprintf(
      "  series: %s | rows: %s | freq: %s | range: %s - %s\n",
      n_series, nrow_raw, freq_resolved, as.character(start), as.character(end)
    )
  ))
  cat(sprintf(
    "  issues: %s rows | features: %s rows\n",
    nrow(x$issues), if (!is.null(x$features)) nrow(x$features) else 0
  ))
  invisible(x)
}

#' @export
print.tsqc_fix <- function(x, ...) {
  cat("<tsqc_fix>\n")
  cat(sprintf("  rows: %s | steps: %s\n", nrow(x$data), nrow(x$log)))
  invisible(x)
}
