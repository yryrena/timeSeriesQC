#' Scan panel/long time series for common QC issues
#'
#' Scans for duplicates, gaps, point outliers, and structural breakpoints (if
#' `strucchange` is available). Also computes per-series features (KPSS/ADF hints,
#' seasonal strength via STL if feasible).
#'
#' @importFrom feasts STL
#' @importFrom fabletools model components
#'
#' @param data Tidy data with time series
#' @param time,id,value Column mappings
#' @param freq One of "auto","daily","weekly","monthly","irregular"
#' @export
tsqc_scan <- function(data, time, id = NULL, value,
                      freq = c("auto","daily","weekly","monthly","irregular")) {
  freq <- match.arg(freq)
  df <- standardize_df(data, {{ time }}, {{ id }}, {{ value }})
  
  ## resolve frequency
  freq_resolved <- if (freq == "auto") infer_frequency(df) else freq
  fp <- freq_params(freq_resolved)
  
  ## meta
  meta <- list(
    n_series      = dplyr::n_distinct(df$id),
    nrow_raw      = nrow(df),
    start         = suppressWarnings(min(df$time, na.rm = TRUE)),
    end           = suppressWarnings(max(df$time, na.rm = TRUE)),
    freq_resolved = freq_resolved,
    tz            = if (inherits(df$time, "POSIXct")) attr(df$time, "tzone") %||% "" else "Date",
    raw_df        = df
  )
  
  # ---------------------------------------------------------------------------
  ## duplicates
  issues_dup <- df %>%
    dplyr::count(id, time, name = "n") %>%
    dplyr::filter(n > 1) %>%
    dplyr::transmute(
      id,
      time_start = time,
      time_end   = time,
      issue_type = "duplicate",
      severity   = "medium",
      score      = as.integer(n),
      details    = as.character(jsonlite::toJSON(list(n = n), auto_unbox = TRUE))
    )
  
  # ---------------------------------------------------------------------------
  ## gaps (only if regular)
  issues_gap <- dplyr::tibble()
  if (!is.na(fp$by)) {
    complete_grid <- build_regular_grid(meta$raw_df, fp$by)
    
    missing_ts <- dplyr::anti_join(complete_grid, meta$raw_df, by = c("id","time"))
    if (nrow(missing_ts) > 0) {
      issues_gap <- missing_ts %>%
        dplyr::group_by(id) %>%
        dplyr::arrange(time, .by_group = TRUE) %>%
        dplyr::transmute(
          id,
          time_start = time,
          time_end   = time,
          issue_type = "gap",
          severity   = "high",
          score      = 1L,
          details    = "{}"
        ) %>%
        dplyr::ungroup()
    }
  }
  
  # ---------------------------------------------------------------------------
  ## outliers (MAD on values or STL residuals if feasts available)
  issues_out <- meta$raw_df %>%
    dplyr::group_by(id) %>%
    dplyr::group_modify(function(d, .y, ...) {
      d <- d[order(d$time), ]
      vals <- d$value
      
      ## default no outliers
      flags <- rep(FALSE, length(vals))
      
      if (length(vals) >= 3) {
        can_use_stl <- pkg_avail("feasts") &&
          !is.na(fp$period) &&
          !anyNA(vals) &&
          length(vals) >= max(8, fp$period * 2)
        
        if (can_use_stl) {
          fit <- try({
            tsb <- tsibble::as_tsibble(
              dplyr::tibble(
                id    = .y$id[[1]],
                idx   = seq_along(vals),
                value = vals
              ),
              key   = !!rlang::sym("id"),
              index = !!rlang::sym("idx")
            )
            fabletools::model(
              tsb,
              feasts::STL(
                value ~ tsqc_trend(window = 7) +
                  tsqc_season(period = fp$period)
              )
            )
            
          }, silent = TRUE)
          
          if (!inherits(fit, "try-error")) {
            comp <- try(fabletools::components(fit), silent = TRUE)
            if (!inherits(comp, "try-error") && "remainder" %in% names(comp)) {
              resid <- as.numeric(comp$remainder)
              flags <- flag_outliers_mad(resid)
            } else {
              flags <- flag_outliers_mad(vals)
            }
          } else {
            flags <- flag_outliers_mad(vals)
          }
        } else {
          flags <- flag_outliers_mad(vals)
        }
      }
      
      ## harden against NAs in flags
      flags <- flags %in% TRUE
      idx <- which(flags)
      
      ## NOTE: don't return `id`; group_modify will add it.
      dplyr::tibble(
        time_start = d$time[idx],
        time_end   = d$time[idx],
        issue_type = "outlier_point",
        severity   = "medium",
        score      = 1L,
        details    = "{}"
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(time_start))
  
  # ---------------------------------------------------------------------------
  ## breakpoints
  issues_bp <- dplyr::tibble()
  if (pkg_avail("strucchange")) {
    issues_bp <- meta$raw_df %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(function(d, .y, ...) {
        d <- d[order(d$time), ]
        vv <- stats::na.omit(d$value)
        if (length(vv) < 20) return(dplyr::tibble())
        fit <- try(strucchange::breakpoints(value ~ 1, data = d), silent = TRUE)
        if (inherits(fit, "try-error")) return(dplyr::tibble())
        bp <- try(strucchange::breakdates(fit), silent = TRUE)
        if (inherits(bp, "try-error") || length(bp) == 0) return(dplyr::tibble())
        times <- d$time[bp]
        if (length(times) == 0) return(dplyr::tibble())
        dplyr::tibble(
          time_start = times,
          time_end   = times,
          issue_type = "breakpoint",
          severity   = "medium",
          score      = 1L,
          details    = "{}"
        )
      }) %>%
      dplyr::ungroup()
  }
  
  # ---------------------------------------------------------------------------
  ## features (unit root / seasonality hints)
  features <- meta$raw_df %>%
    dplyr::group_by(id) %>%
    dplyr::group_modify(function(d, .y, ...) {
      d <- d[order(d$time), ]
      vals <- d$value
      v_complete <- stats::na.omit(vals)
      
      kpss_p <- NA_real_
      adf_p  <- NA_real_
      seas_strength <- NA_real_
      
      ## stationarity / unit root hints via tseries
      if (pkg_avail("tseries") && length(v_complete) >= 8) {
        k <- try(tseries::kpss.test(v_complete, null = "Level"), silent = TRUE)
        if (!inherits(k, "try-error")) {
          kpss_p <- suppressWarnings(as.numeric(k$p.value))
        }
        a <- try(tseries::adf.test(v_complete, k = 0), silent = TRUE)
        if (!inherits(a, "try-error")) {
          adf_p <- suppressWarnings(as.numeric(a$p.value))
        }
      } else if (pkg_avail("urca") && length(v_complete) >= 8) {
        invisible(NULL)   ## leave p-values as NA; urca doesn't give easy exact p
      }
      
      ## seasonal strength via STL (only if no NAs and enough length)
      can_use_stl <- pkg_avail("feasts") &&
        !is.na(fp$period) &&
        !anyNA(vals) &&
        length(vals) >= max(8, fp$period * 2)
      
      if (can_use_stl) {
        fit2 <- try({
          tsb2 <- tsibble::as_tsibble(
            dplyr::tibble(
              id    = .y$id[[1]],
              idx   = seq_len(length(vals)),
              value = vals
            ),
            key   = !!rlang::sym("id"),
            index = !!rlang::sym("idx")
          )
          fabletools::model(
            tsb2,
            feasts::STL(
              value ~ tsqc_trend() +
                tsqc_season(period = fp$period)
            )
          )
          
        }, silent = TRUE)
        
        if (!inherits(fit2, "try-error")) {
          comp <- try(fabletools::components(fit2), silent = TRUE)
          if (!inherits(comp, "try-error")) {
            seas_col <- if ("season" %in% names(comp)) "season" else {
              sc <- grep("^season", names(comp), value = TRUE)
              if (length(sc)) sc[1] else NA_character_
            }
            if (!is.na(seas_col)) {
              seas_vec <- comp[[seas_col]]
              seas <- stats::var(seas_vec, na.rm = TRUE)
              tot  <- stats::var(vals,     na.rm = TRUE)
              seas_strength <- if (is.finite(seas) && is.finite(tot) && tot > 0) {
                seas / tot
              } else {
                NA_real_
              }
            }
          }
        }
      }
      
      dplyr::tibble(
        kpss_p = kpss_p,
        adf_p  = adf_p,
        seasonal_strength = seas_strength
      )
    }) %>%
    dplyr::ungroup()
  
  # ---------------------------------------------------------------------------
  issues  <- dplyr::bind_rows(issues_dup, issues_gap, issues_out, issues_bp)
  preview <- make_preview_sample(meta$raw_df, max_ids = 12, per_id = 200)
  
  new_tsqc_scan(meta, issues, features, preview)
}
