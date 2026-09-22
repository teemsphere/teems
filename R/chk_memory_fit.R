#' @keywords internal
#' @noRd
.memory_fit_gb <- function(x) {
  txt <- format(round(x, 1), nsmall = 1, trim = TRUE)
  return(txt)
}

#' @keywords internal
#' @noRd
.memory_fit_check <- function(method,
                              n_tasks,
                              plain_size,
                              condensed = FALSE,
                              host = NULL,
                              th = .auto_thresholds(),
                              call = NULL,
                              report_only = FALSE) {
  est_gb <- .auto_memory_gb(method, n_tasks, plain_size, condensed, th)
  limit <- host$mem_gb %|||% NA_real_
  rec <- list(
    method = method,
    n_tasks = as.integer(n_tasks),
    plain_size = plain_size,
    condensed = isTRUE(condensed),
    est_gb = est_gb,
    limit_gb = limit,
    share = NA_real_,
    verdict = "unknown"
  )
  if (is.na(est_gb) || is.na(limit) || limit <= 0) {
    return(rec)
  }
  rec$share <- est_gb / limit
  if (rec$share > th$mem_abort_ratio) {
    rec$verdict <- "exceeds"
    if (isTRUE(report_only)) {
      return(rec)
    }
    est_gb <- .memory_fit_gb(est_gb)
    mem_gb <- .memory_fit_gb(limit)
    kb_per_eq <- format(round(1e6 * rec$est_gb / plain_size, 2), nsmall = 2, trim = TRUE)
    plain_size <- format(round(plain_size), big.mark = ",", scientific = FALSE, trim = TRUE)
    .cli_action(solve_err$wont_fit,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }
  if (rec$share > th$mem_fit_share) {
    rec$verdict <- "tight"
    if (isTRUE(report_only)) {
      return(rec)
    }
    est_gb <- .memory_fit_gb(est_gb)
    mem_gb <- .memory_fit_gb(limit)
    share <- paste0(round(100 * rec$share), "%")
    .cli_action(solve_wrn$memory_tight,
      action = c("warn", "inform"),
      call = call
    )
    return(rec)
  }
  rec$verdict <- "fits"
  return(rec)
}
