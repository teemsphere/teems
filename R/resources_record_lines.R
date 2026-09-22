#' @keywords internal
#' @noRd
.record_gb <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(solve_info$record$unknown_gb)
  }
  txt <- paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
  return(txt)
}

#' @keywords internal
#' @noRd
.resources_record_lines <- function(r) {
  if (is.null(r)) {
    return(NULL)
  }
  host <- if (is.null(r$cores) || is.na(r$cores)) {
    solve_info$record$host_unknown
  } else {
    sprintf(solve_info$record$host, r$cores, .record_gb(r$mem_gb))
  }
  fit <- r$fit
  fit_line <- if (is.null(fit) || is.na(fit$est_gb)) {
    solve_info$record$fit_na
  } else if (identical(fit$verdict, "unknown")) {
    sprintf(solve_info$record$fit_unknown, .record_gb(fit$est_gb))
  } else {
    sprintf(
      solve_info$record$fit,
      fit$method, fit$n_tasks, .record_gb(fit$est_gb),
      format(round(1e6 * fit$est_gb / fit$plain_size, 2), nsmall = 2, trim = TRUE),
      format(round(fit$plain_size), big.mark = ",", scientific = FALSE, trim = TRUE),
      if (isTRUE(fit$condensed)) {
        solve_info$record$fit_condensed
      } else {
        ""
      },
      paste0(round(100 * fit$share), "%"), .record_gb(fit$limit_gb), fit$verdict
    )
  }
  lines <- c(
    sprintf(
      solve_info$record$resources,
      r$n_tasks, r$n_threads,
      if (is.null(r$inmemory)) {
        solve_info$record$solver_default
      } else {
        tolower(as.character(r$inmemory))
      },
      r$tempdir %|||% solve_info$record$solver_default,
      host
    ),
    fit_line
  )
  return(lines)
}
