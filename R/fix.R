#' Interpolate short NA runs within a single id series
#'
#' Internal helper. Fills runs of missing values up to `max_run` length
#' using either last-observation-carried-forward ("locf") or "linear"
#' interpolation between neighbors.
#'
#' @param d single-id data.frame/tibble with columns `time` and `value`
#'          already ordered by time
#' @param method "locf" or "linear"
#' @param max_run maximum run length of consecutive NAs to fill
#' @keywords internal
#' @noRd
interp_runs_one_series <- function(d, method = "locf", max_run = 10L) {
  is_na <- is.na(d$value)
  if (!any(is_na)) return(d)
  
  r <- rle(is_na)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  runs <- data.frame(start = starts[r$values], end = ends[r$values])
  
  if (identical(method, "locf")) {
    ## precompute forward/backward fills on the FULL vector
    fwd <- zoo::na.locf(d$value, fromLast = FALSE, na.rm = FALSE)
    bwd <- zoo::na.locf(d$value, fromLast = TRUE,  na.rm = FALSE)
  }
  
  for (i in seq_len(nrow(runs))) {
    s <- runs$start[i]; e <- runs$end[i]
    run_len <- e - s + 1
    if (run_len <= max_run) {
      if (identical(method, "locf")) {
        ## fill from both directions, prefer forward then backward
        seg <- fwd[s:e]
        still_na <- is.na(seg)
        if (any(still_na)) {
          seg[still_na] <- bwd[s:e][still_na]
        }
        d$value[s:e] <- seg
        
      } else if (identical(method, "linear")) {
        left_idx  <- max(s - 1, 1L)
        right_idx <- min(e + 1, nrow(d))
        
        d$value[s:e] <- stats::approx(
          x    = c(left_idx, right_idx),
          y    = c(d$value[left_idx], d$value[right_idx]),
          xout = s:e,
          method = "linear",
          rule   = 2
        )$y
      }
    }
  }
  
  d
}


#' Apply traceable fixes to time series based on scan
#'
#' Applies a set of repair rules (dedup, fill_gaps, interpolate, winsorize)
#' to a `tsqc_scan` object and returns a `tsqc_fix` object with both the
#' cleaned data and a step-by-step log.
#'
#' @importFrom fabletools model components
#' @param scan_obj A tsqc_scan object
#' @param rules Vector of rules: "dedup","fill_gaps","interpolate","winsorize"
#' @param ... Extra parameters per rule, e.g.:
#'   \itemize{
#'     \item interpolate: \code{method = "locf"} or \code{"linear"}, \code{max_run = 10}
#'     \item winsorize: \code{lower = 0.005}, \code{upper = 0.995}
#'   }
#'
#' @return An object of class \code{tsqc_fix} with:
#' \itemize{
#'   \item \code{data}: repaired data frame
#'   \item \code{log}: tibble describing each applied step
#' }
#' @export
tsqc_fix <- function(scan_obj,
                     rules = c("dedup","fill_gaps","interpolate","winsorize"),
                     ...) {
  stopifnot(inherits(scan_obj, "tsqc_scan"))
  
  ## working data
  df <- scan_obj$meta$raw_df
  
  ## start a clean, strictly-typed log
  log <- dplyr::tibble(
    step          = integer(),
    rule          = character(),
    params        = character(),   ## JSON string
    changed_count = integer()
  )
  
  ## helper: always coerce params to JSON (character)
  add_log <- function(step, rule, changed, params = list()) {
    dplyr::tibble(
      step          = as.integer(step),
      rule          = as.character(rule),
      params        = as.character(jsonlite::toJSON(params, auto_unbox = TRUE)),
      changed_count = as.integer(changed)
    )
  }
  
  step <- 0L
  
  # ---------------------------------------------------------------------------
  # 1) dedup: keep last observation within (id, time)
  if ("dedup" %in% rules) {
    step   <- step + 1L
    before <- nrow(df)
    
    df <- df %>%
      dplyr::arrange(id, time) %>%
      dplyr::distinct(id, time, .keep_all = TRUE)
    
    changed <- before - nrow(df)
    log <- dplyr::bind_rows(log, add_log(step, "dedup", changed))
  }
  
  # ---------------------------------------------------------------------------
  # 2) fill_gaps: materialize full grid with NA (only for regular frequencies)
  if ("fill_gaps" %in% rules) {
    step <- step + 1L
    fp <- freq_params(scan_obj$meta$freq_resolved)
    
    if (!is.na(fp$by)) {
      expanded <- df %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          tmin = min(time, na.rm = TRUE),
          tmax = max(time, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(grid = list(seq(from = tmin, to = tmax, by = fp$by))) %>%
        tidyr::unnest(grid) %>%
        dplyr::rename(time = grid)
      
      before <- nrow(df)
      df <- dplyr::left_join(expanded, df, by = c("id", "time"))
      after <- nrow(df)
      
      log <- dplyr::bind_rows(
        log,
        add_log(step, "fill_gaps", after - before, list(by = fp$by))
      )
    } else {
      log <- dplyr::bind_rows(
        log,
        add_log(step, "fill_gaps", 0L, list(note = "irregular; skipped"))
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 3) interpolate: LOCF/linear for small runs of NA
  if ("interpolate" %in% rules) {
    step <- step + 1L
    
    args <- list(method = "locf", max_run = 10L)
    dots <- list(...)
    args[names(dots)] <- dots
    
    before_na <- sum(is.na(df$value))
    
    df <- df %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(time, .by_group = TRUE) %>%
      dplyr::group_modify(~ interp_runs_one_series(.x,
                                                   method  = args$method,
                                                   max_run = args$max_run)) %>%
      dplyr::ungroup()
    
    after_na <- sum(is.na(df$value))
    
    log <- dplyr::bind_rows(
      log,
      add_log(step, "interpolate", before_na - after_na, args)
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4) winsorize: cap extremes by per-id quantiles
  if ("winsorize" %in% rules) {
    step <- step + 1L
    
    ## defaults, then override with ...
    args <- list(lower = 0.005, upper = 0.995)
    dots <- list(...)
    args[names(dots)] <- dots
    
    before <- df$value
    
    safe_quantiles <- function(x, lower, upper) {
      if (all(is.na(x))) {
        return(list(lo = NA_real_, hi = NA_real_))
      }
      lo <- stats::quantile(x, probs = lower, na.rm = TRUE, names = FALSE)
      hi <- stats::quantile(x, probs = upper, na.rm = TRUE, names = FALSE)
      list(lo = lo, hi = hi)
    }
    
    caps <- df %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(function(d, ...) {
        q <- safe_quantiles(d$value, args$lower, args$upper)
        dplyr::tibble(lo = q$lo, hi = q$hi)
      }) %>%
      dplyr::ungroup()
    
    df <- dplyr::left_join(df, caps, by = "id") %>%
      dplyr::mutate(value = pmin(pmax(value, lo), hi)) %>%
      dplyr::select(id, time, value)
    
    changed <- sum(!is.na(before) & !is.na(df$value) & before != df$value)
    
    log <- dplyr::bind_rows(
      log,
      add_log(step, "winsorize", changed, args)
    )
  }
  
  new_tsqc_fix(df, log)
}