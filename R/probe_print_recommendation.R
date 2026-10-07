#' @keywords internal
#' @noRd
.recommend_gb <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(probe_info$recommend$unknown_gb)
  }
  if (x < 0.01) {
    return(probe_info$recommend$small_gb)
  }
  if (x < 0.95) {
    txt <- paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
    return(txt)
  }
  txt <- paste0(format(round(x, 1), nsmall = 1, trim = TRUE), " GB")
  return(txt)
}

#' @keywords internal
#' @noRd
.recommend_est <- function(x) {
  txt <- .recommend_gb(x)
  if (!is.null(x) && !is.na(x) && x >= 0.01) {
    txt <- sprintf(probe_info$recommend$about, txt)
  }
  return(txt)
}

#' @importFrom cli format_inline
#' @keywords internal
#' @noRd
.recommend_where <- function(host) {
  m <- probe_info$recommend
  cores <- host$cores
  mem <- if (is.null(host$mem_gb) || is.na(host$mem_gb)) {
    m$unknown_mem
  } else {
    .recommend_gb(host$mem_gb)
  }
  mem_known <- !identical(mem, m$unknown_mem)
  template <- switch(host$source %|||% "container",
    given = m$where_given,
    cores_given = if (mem_known) m$where_cores_given else m$where_cores_given_nomem,
    memory_given = m$where_memory_given,
    m$where_container
  )
  where <- cli::format_inline(template)
  return(where)
}

#' @importFrom cli cli_bullets format_inline
#' @keywords internal
#' @noRd
.recommend_print_none <- function(r) {
  m <- probe_info$recommend
  est_gb <- .recommend_gb(r$fit$est_gb)
  limit_gb <- .recommend_gb(r$fit$limit_gb)
  template <- switch(r$status,
    singular = m$why_singular,
    no_host = m$why_no_host,
    wont_fit = m$why_wont_fit
  )
  why <- cli::format_inline(template)
  cli::cli_bullets(c("x" = m$none))
  return(invisible(NULL))
}

#' @importFrom cli cli_rule cli_bullets
#' @keywords internal
#' @noRd
.probe_print_recommendation <- function(r,
                                        brief = FALSE) {
  if (is.null(r)) {
    return(invisible(NULL))
  }
  m <- probe_info$recommend
  if (!brief) {
    cli::cli_rule(left = m$rule)
  }
  if (!identical(r$status %|||% "ok", "ok")) {
    .recommend_print_none(r)
    return(invisible(NULL))
  }
  fit <- r$fit
  n_tasks <- r$n_tasks
  n_threads <- r$n_threads
  where_txt <- .recommend_where(r$host)
  if (brief) {
    if (identical(fit$verdict %|||% "unknown", "tight")) {
      est_gb <- .recommend_gb(fit$est_gb)
      limit_gb <- .recommend_gb(fit$limit_gb)
      cli::cli_bullets(c("!" = m$memory_tight))
    }
    cli::cli_bullets(c(">" = m$brief, " " = m$call))
    return(invisible(NULL))
  }
  cli::cli_bullets(c(
    "v" = m$host,
    "*" = m$evidence,
    "*" = m$rationale
  ))
  if (!identical(r$method_johansen, r$matrix_method)) {
    cli::cli_bullets(c("i" = m$johansen))
  }
  d <- r$decision
  if (isTRUE(d$memory_arm)) {
    sbbd_gb <- .recommend_gb(d$memory$estimates$SBBD)
    cli::cli_bullets(c("!" = m$memory_arm))
  }
  if (isTRUE(d$dbbd_memory_blocked)) {
    dbbd_gb <- .recommend_gb(d$memory$estimates$DBBD)
    cli::cli_bullets(c("!" = m$dbbd_blocked))
    if (!is.na(r$dbbd_plain_gb)) {
      plain_gb <- .recommend_gb(r$dbbd_plain_gb)
      cli::cli_bullets(c("i" = m$dbbd_blocked_plain))
    }
  }
  if (isTRUE(d$lu_excluded)) {
    cli::cli_bullets(c("!" = m$lu_excluded))
  }
  if (!is.null(fit) && !is.na(fit$est_gb)) {
    est_gb <- .recommend_est(fit$est_gb)
    if (identical(fit$verdict, "unknown")) {
      cli::cli_bullets(c("*" = m$memory_unknown))
    } else {
      share_txt <- if (fit$share < 0.005) {
        sprintf(m$mem_share_small, .recommend_gb(fit$limit_gb))
      } else {
        sprintf(m$mem_share, round(100 * fit$share), .recommend_gb(fit$limit_gb))
      }
      line <- m$memory
      names(line) <- if (identical(fit$verdict, "tight")) "!" else "*"
      cli::cli_bullets(line)
    }
  }
  rf <- r$refine
  if (!is.null(rf)) {
    if (isTRUE(rf$on)) {
      cli::cli_bullets(c("*" = m$refine_on))
    } else {
      refine_gb <- .recommend_est(rf$est_gb)
      cli::cli_bullets(c("*" = m$refine_off))
    }
  }
  if (!is.null(r$tempdir)) {
    cli::cli_bullets(c("*" = m$scratch))
  }
  cli::cli_bullets(c(">" = m$call))
  return(invisible(NULL))
}
