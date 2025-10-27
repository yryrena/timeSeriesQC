# Internal helpers for standardization, frequency inference, etc.
# All functions below are internal (documented with @noRd).

#' Standardize input to (id, time, value)
#' Accepts bare symbols (e.g., time = time) or character names (e.g., time = "time").
#' Ensures rows are ordered by id/time and preserves time zone attribute for POSIXct.
#' @keywords internal
#' @noRd
standardize_df <- function(data, time, id = NULL, value) {
  ## capture original args (quosures or chars)
  time_q  <- rlang::enquo(time)
  id_q    <- rlang::enquo(id)
  value_q <- rlang::enquo(value)
  
  ## convert to symbol from quosure or string
  to_sym <- function(q) {
    if (rlang::is_quosure(q)) {
      expr <- rlang::get_expr(q)
      if (rlang::is_string(expr)) rlang::sym(expr) else rlang::ensym(expr)
    } else if (rlang::is_string(q)) {
      rlang::sym(q)
    } else {
      rlang::ensym(q)
    }
  }
  
  time_sym  <- to_sym(time_q)
  value_sym <- to_sym(value_q)
  
  col_extract <- function(df, sym) df[[rlang::as_string(sym)]]
  
  if (rlang::quo_is_null(id_q)) {
    out <- dplyr::tibble(
      id    = factor(1L),
      time  = col_extract(data, time_sym),
      value = col_extract(data, value_sym)
    )
  } else {
    id_sym <- to_sym(id_q)
    out <- dplyr::tibble(
      id    = col_extract(data, id_sym),
      time  = col_extract(data, time_sym),
      value = col_extract(data, value_sym)
    )
  }
  
  ## ensure types & order
  if (inherits(out$time, "POSIXt")) {
    attr(out$time, "tzone") <- attr(out$time, "tzone") %||% ""
  }
  out <- out[order(out$id, out$time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Infer target frequency from median time delta across series
#' Returns one of "daily","weekly","monthly","irregular".
#' Heuristics are in seconds, robust to Date or POSIXct.
#' @keywords internal
#' @noRd
infer_frequency <- function(df) {
  stopifnot(all(c("id","time","value") %in% names(df)))
  
  ## numeric seconds for diffs
  to_seconds <- function(x) {
    if (inherits(x, "Date")) {
      as.numeric(x) * 86400
    } else {
      as.numeric(x)
    }
  }
  
  ## per-id median delta
  ids <- unique(df$id)
  med_diffs <- numeric(length(ids))
  for (i in seq_along(ids)) {
    di <- df[df$id == ids[i], , drop = FALSE]
    di <- di[order(di$time), , drop = FALSE]
    tt <- to_seconds(di$time)
    dd <- diff(tt)
    med_diffs[i] <- suppressWarnings(stats::median(dd, na.rm = TRUE))
  }
  
  label_one <- function(x) {
    if (is.na(x) || is.infinite(x) || length(x) == 0) return("irregular")
    day   <- 86400
    week  <- 7 * day
    month <- 30 * day
    if (abs(x - day)   < day / 2)         "daily"
    else if (abs(x - week)  < 3 * day)    "weekly"
    else if (abs(x - month) < 15 * day)   "monthly"
    else "irregular"
  }
  
  labs <- vapply(med_diffs, label_one, character(1))
  tab  <- table(labs)
  ## majority label; tie-breaker = first in names(tab)
  names(tab)[which.max(tab)]
}

#' Get seq-by-frequency step and nominal seasonal period
#' @keywords internal
#' @noRd
freq_params <- function(freq) {
  if (identical(freq, "daily"))   return(list(by = "1 day",   period = 7L))
  if (identical(freq, "weekly"))  return(list(by = "1 week",  period = 52L))
  if (identical(freq, "monthly")) return(list(by = "1 month", period = 12L))
  list(by = NA_character_, period = NA_integer_)     ## irregular
}

#' Safe package-availability check
#' @keywords internal
#' @noRd
pkg_avail <- function(pkg) {
  isTRUE(requireNamespace(pkg, quietly = TRUE))
}

#' Compress identical timestamps (helper for contiguous runs)
#' Returns a tibble(time_start, time_end) for repeated single timestamps.
#' @keywords internal
#' @noRd
runs_from_times <- function(times) {
  times <- sort(times)
  if (length(times) == 0) {
    return(dplyr::tibble(time_start = as.POSIXct(character()), time_end = as.POSIXct(character())))
  }
  idx    <- c(TRUE, diff(times) != 0)
  starts <- times[idx]
  ends   <- times[c(which(idx)[-1] - 1, length(times))]
  dplyr::tibble(time_start = starts, time_end = ends)
}

#' Robust outlier flags via MAD
#' Flags values with |(x - median)/MAD| > k; returns logical vector.
#' @keywords internal
#' @noRd
flag_outliers_mad <- function(x, k = 3.5) {
  med <- stats::median(x, na.rm = TRUE)
  mad <- stats::mad(x, center = med, constant = 1.4826, na.rm = TRUE)
  if (mad == 0 || is.na(mad)) return(rep(FALSE, length(x)))
  abs((x - med) / mad) > k
}

#' Null coalescing helper
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Build a regular per-id grid from min(time) to max(time)
#' @keywords internal
#' @noRd
build_regular_grid <- function(df, by) {
  stopifnot(all(c("id","time") %in% names(df)))
  bounds <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      tmin = suppressWarnings(min(time, na.rm = TRUE)),
      tmax = suppressWarnings(max(time, na.rm = TRUE)),
      .groups = "drop"
    )
  do.call(
    dplyr::bind_rows,
    lapply(seq_len(nrow(bounds)), function(i) {
      dplyr::tibble(
        id   = bounds$id[i],
        time = seq(from = bounds$tmin[i], to = bounds$tmax[i], by = by)
      )
    })
  )
}

#' Sample a small per-id preview for plotting
#' @keywords internal
#' @noRd
make_preview_sample <- function(df, max_ids = 12, per_id = 200) {
  ids <- unique(df$id)[seq_len(min(length(unique(df$id)), max_ids))]
  dplyr::bind_rows(lapply(ids, function(k) {
    di <- df[df$id == k, , drop = FALSE]
    if (nrow(di) > per_id) {
      keep <- unique(round(seq(1, nrow(di), length.out = per_id)))
      di <- di[keep, , drop = FALSE]
    }
    di
  }))
}

