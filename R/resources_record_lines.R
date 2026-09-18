#' @description Lines for the model_diagnostics.txt solve record: the
#'   tasks/threads/scratch the run used, the container inspected and
#'   the pre-solve memory check.
#' @keywords internal
#' @noRd
.resources_record_lines <- function(r) {
  if (is.null(r)) {
    return(NULL)
  }
  fmt_gb <- function(x) {
    if (is.null(x) || is.na(x)) {
      return("unknown")
    }
    txt <- paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
    return(txt)
  }
  host <- if (is.null(r$cores) || is.na(r$cores)) {
    "container not inspected"
  } else {
    sprintf("container %s core(s), %s", r$cores, fmt_gb(r$mem_gb))
  }
  fit <- r$fit
  fit_line <- if (is.null(fit) || is.na(fit$est_gb)) {
    "  memory check: not applied (system size unknown)"
  } else if (identical(fit$verdict, "unknown")) {
    sprintf("  memory check: estimate %s, container limit unknown", fmt_gb(fit$est_gb))
  } else {
    sprintf(
      "  memory check: %s at %s task(s) estimated %s = %s kB/eq x %s plain-equivalent equations%s -> %s of %s (%s)",
      fit$method, fit$n_tasks, fmt_gb(fit$est_gb),
      format(round(1e6 * fit$est_gb / fit$plain_size, 2), nsmall = 2, trim = TRUE),
      format(round(fit$plain_size), big.mark = ",", scientific = FALSE, trim = TRUE),
      if (isTRUE(fit$condensed)) {
        " (condensed)"
      } else {
        ""
      },
      paste0(round(100 * fit$share), "%"), fmt_gb(fit$limit_gb), fit$verdict
    )
  }
  lines <- c(
    sprintf(
      "Resources: n_tasks %s, n_threads %s, inmemory %s, tempdir %s (%s)",
      r$n_tasks, r$n_threads,
      if (is.null(r$inmemory)) {
        "solver default"
      } else {
        tolower(as.character(r$inmemory))
      },
      r$tempdir %|||% "solver default",
      host
    ),
    fit_line
  )
  return(lines)
}
