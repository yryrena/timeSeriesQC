# internal helper: robustly estimate seasonal strength via STL
# not exported
#' @keywords internal
#' @noRd
safe_season_strength <- function(vals, fp_period) {
  out <- list(strength = NA_real_, ok = FALSE)
  
  ## guards
  if (is.na(fp_period)) return(out)
  if (anyNA(vals))      return(out)
  if (length(vals) < max(8, fp_period * 2)) return(out)
  if (!pkg_avail("feasts")) return(out)
  
  res <- tryCatch({
    tsb <- tsibble::as_tsibble(
      dplyr::tibble(
        id    = "1",
        idx   = seq_along(vals),
        value = vals
      ),
      key   = id,
      index = idx
    )
    
    fit <- fabletools::model(
      tsb,
      feasts::STL(
        value ~ tsqc_trend() + tsqc_season(period = fp_period)
      )
    )
    
    comp <- suppressWarnings(fabletools::components(fit))
    
    seas_col <- if ("season" %in% names(comp)) {
      "season"
    } else {
      sc <- grep("^season", names(comp), value = TRUE)
      if (length(sc)) sc[1] else NA_character_
    }
    
    if (is.na(seas_col)) return(out)
    
    seas <- stats::var(comp[[seas_col]], na.rm = TRUE)
    tot  <- stats::var(vals,             na.rm = TRUE)
    
    strength <- if (is.finite(seas) && is.finite(tot) && tot > 0) {
      seas / tot
    } else {
      NA_real_
    }
    
    list(strength = strength, ok = TRUE)
  }, error = function(e) {
    out
  })
  
  res
}

#' Suggest transformations: log/Box-Cox, differencing, seasonal adjustment
#'
#' Heuristics per series:
#' - log vs shifted_log
#' - difference? (based on lag-1 ACF)
#' - seasonal adjustment recommended? (if STL seasonal strength > 0.2)
#'
#' @importFrom fabletools model components 
#' @importFrom feasts STL
#' @param scan_obj tsqc_scan
#' @export
tsqc_suggest <- function(scan_obj) {
  stopifnot(inherits(scan_obj, "tsqc_scan"))
  
  df <- scan_obj$meta$raw_df
  freq <- scan_obj$meta$freq_resolved
  fp <- freq_params(freq)
  
  df %>%
    dplyr::group_by(id) %>%
    dplyr::group_modify(function(d, .y, ...) {
      d <- d[order(d$time), ]
      vals <- d$value
      
      ## log / shifted_log suggestion
      suggest_log <- if (!is.finite(min(vals, na.rm = TRUE)) ||
                         min(vals, na.rm = TRUE) <= 0) {
        "shifted_log"
      } else {
        "log"
      }
      
      shift <- if (suggest_log == "shifted_log") {
        abs(min(vals, na.rm = TRUE)) + 1e-6
      } else {
        0
      }
      
      ## lag-1 ACF for differencing hint
      ac1 <- suppressWarnings(
        stats::acf(vals, na.action = na.pass, plot = FALSE, lag.max = 1)$acf[2]
      )
      suggest_diff <- if (!is.na(ac1) && ac1 > 0.8) 1L else 0L
      
      ## seasonal strength (safe)
      seas_info <- safe_season_strength(vals, fp$period)
      suggest_seasadj <- isTRUE(seas_info$ok && seas_info$strength > 0.2)
      
      dplyr::tibble(
        suggest_log = suggest_log,
        shift = shift,
        suggest_diff = suggest_diff,
        suggest_seasadj = suggest_seasadj,
        notes = if (suggest_log == "shifted_log")
          "min<=0: use value+shift before log"
        else
          ""
      )
    }) %>%
    dplyr::ungroup()
}