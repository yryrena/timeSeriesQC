#' Write scan/fix payload JSON to disk (internal)
#'
#' Builds the report payload and writes it as JSON to `file`.
#' Returns the file path (invisibly).
#'
#' This is factored out so tests can verify the payload structure
#' without needing `quarto` or `rmarkdown` installed.
#'
#' @param scan_obj tsqc_scan
#' @param fix_obj  tsqc_fix or NULL
#' @param file     path to write the JSON
#' @keywords internal
#' @noRd
build_report_json <- function(scan_obj, fix_obj, file) {
  payload <- export_payload(scan_obj, fix_obj)
  jsonlite::write_json(
    payload,
    path       = file,
    auto_unbox = TRUE,
    pretty     = TRUE
  )
  invisible(file)
}


#' Render an HTML QC report
#'
#' Given a \code{tsqc_scan} (and optional \code{tsqc_fix}), this function
#' writes a standalone QC HTML report. Internally it:
#'
#' \itemize{
#'   \item builds a JSON payload describing issues, fixes, and metadata;
#'   \item writes that JSON to a temp file;
#'   \item calls \code{quarto::quarto_render()} if Quarto is available,
#'         otherwise falls back to \code{rmarkdown::render()}.
#' }
#'
#' Tests can exercise \code{build_report_json()} directly to avoid requiring
#' Quarto on CI.
#'
#' @importFrom fabletools model components 
#' @param scan_obj tsqc_scan
#' @param fix_obj  tsqc_fix or NULL
#' @param out      output HTML path (string)
#'
#' @return Invisibly returns the output path.
#' @export
tsqc_report <- function(scan_obj, fix_obj = NULL, out = "qc_report.html") {
  stopifnot(inherits(scan_obj, "tsqc_scan"))
  
  tmp_json <- tempfile("qc_meta_", fileext = ".json")
  build_report_json(scan_obj, fix_obj, tmp_json)
  
  template <- system.file("templates", "qc_report.qmd", package = "timeSeriesQC")
  if (!nzchar(template) || !file.exists(template)) {
    template <- file.path(tempdir(), "qc_report_fallback.qmd")
    write_simple_qmd(template)
  }
  
  on.exit(try(unlink(tmp_json), silent = TRUE), add = TRUE)
  
  if (pkg_avail("quarto")) {
    quarto::quarto_render(
      input = template,
      execute_params = list(payload_path = normalizePath(tmp_json)),
      output_file = normalizePath(out)
    )
  } else {
    if (!pkg_avail("rmarkdown")) {
      stop("Neither 'quarto' nor 'rmarkdown' is installed.")
    }
    rmarkdown::render(
      input = template,
      output_file = out,
      params = list(payload_path = normalizePath(tmp_json))
    )
  }
  
  cli::cli_alert_success("Report written to {out}")
  invisible(out)
}


#' Build JSON-ready payload for the report
#'
#' Internal helper that constructs a list of:
#' \itemize{
#'   \item meta: basic dataset stats
#'   \item issues: detected QC issues
#'   \item features: per-series diagnostics (stationarity, seasonality, etc)
#'   \item fix_log: actions applied by \code{tsqc_fix()}, if provided
#'   \item sample: small preview of the data for plotting
#' }
#'
#' @keywords internal
#' @noRd
export_payload <- function(scan_obj, fix_obj) {
  list(
    meta     = scan_obj$meta[c("n_series","nrow_raw","start","end","freq_resolved","tz")],
    issues   = dplyr::mutate(
      scan_obj$issues,
      dplyr::across(dplyr::everything(), as.character)
    ),
    features = if (!is.null(scan_obj$features)) {
      dplyr::mutate(
        scan_obj$features,
        dplyr::across(dplyr::everything(), as.character)
      )
    } else {
      NULL
    },
    fix_log  = if (!is.null(fix_obj)) {
      dplyr::mutate(
        fix_obj$log,
        dplyr::across(dplyr::everything(), as.character)
      )
    } else {
      NULL
    },
    sample   = if (!is.null(scan_obj$preview)) {
      dplyr::as_tibble(scan_obj$preview)
    } else {
      dplyr::as_tibble(
        scan_obj$meta$raw_df[
          1:min(2000, nrow(scan_obj$meta$raw_df)),
        ]
      )
    }
  )
}


#' Tiny fallback QMD if packaged template is missing
#'
#' Writes a very simple .qmd file if the bundled template cannot be found.
#'
#' @keywords internal
#' @noRd
write_simple_qmd <- function(path) {
  lines <- c(
    "---",
    "title: \"Time Series QC Report\"",
    "format:",
    "  html:",
    "    toc: true",
    "params:",
    "  payload_path: \"qc_meta.json\"",
    "---",
    "",
    "```{r}",
    "payload <- jsonlite::read_json(params$payload_path, simplifyVector = TRUE)",
    "```",
    "",
    "# Overview",
    "",
    "- **Series:** `r payload$meta$n_series`  ",
    "- **Rows:** `r payload$meta$nrow_raw`  ", 
    "- **Date range:** `r payload$meta$start` - `r payload$meta$end`  ", 
    "- **Frequency:** `r payload$meta$freq_resolved`",
    "",
    "# Issues (top 100)",
    "```{r}",
    "head(payload$issues, 100)",
    "```"
  )
  writeLines(lines, path)
}