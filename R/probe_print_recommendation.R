#' @keywords internal
#' @noRd
.recommend_gb <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(probe_info$recommend$unknown_gb)
  }
  if (x < 0.95) {
    txt <- paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
    return(txt)
  }
  txt <- paste0(format(round(x, 1), nsmall = 1, trim = TRUE), " GB")
  return(txt)
}

#' @importFrom cli cli_text
#' @keywords internal
#' @noRd
.probe_print_recommendation <- function(r) {
  if (is.null(r)) {
    return(invisible(NULL))
  }
  n_tasks <- r$n_tasks
  n_threads <- r$n_threads
  if (is.null(r$host$cores) || is.na(r$host$cores)) {
    cli::cli_text(probe_info$recommend$no_host)
  } else {
    cores <- r$host$cores
    mem <- .recommend_gb(r$host$mem_gb)
    where <- if (identical(r$host$source, "given")) {
      probe_info$recommend$where_given
    } else {
      probe_info$recommend$where_container
    }
    cli::cli_text(probe_info$recommend$host)
  }
  cli::cli_text(probe_info$recommend$evidence)
  cli::cli_text(probe_info$recommend$rationale)
  if (!identical(r$method_johansen, r$matrix_method)) {
    cli::cli_text(probe_info$recommend$johansen)
  }
  d <- r$decision
  if (isTRUE(d$memory_arm)) {
    sbbd_gb <- .recommend_gb(d$memory$estimates$SBBD)
    cli::cli_text(probe_info$recommend$memory_arm)
  }
  if (isTRUE(d$dbbd_memory_blocked)) {
    dbbd_gb <- .recommend_gb(d$memory$estimates$DBBD)
    cli::cli_text(probe_info$recommend$dbbd_blocked)
  }
  if (isTRUE(d$lu_excluded)) {
    cli::cli_text(probe_info$recommend$lu_excluded)
  }
  fit <- r$fit
  if (!is.null(fit) && !is.na(fit$est_gb)) {
    est_gb <- .recommend_gb(fit$est_gb)
    share_txt <- if (is.na(fit$share)) {
      ""
    } else {
      paste0(", ", round(100 * fit$share), "% of ", .recommend_gb(fit$limit_gb))
    }
    cli::cli_text(probe_info$recommend$memory)
  }
  if (!is.null(r$tempdir)) {
    cli::cli_text(probe_info$recommend$scratch)
  }
  cli::cli_text(probe_info$recommend$call)
  return(invisible(NULL))
}
