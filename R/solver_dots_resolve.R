#' @keywords internal
#' @noRd
.rk_step_controls <- function() {
  controls <- list(
    adaptive = c("no", "yes", "accuracy-only"),
    eps_tolerance = 0.01,
    max_retries = 3L,
    retry_adjust = 0.5
  )
  return(controls)
}

#' @keywords internal
#' @noRd
.resolve_solver_dots <- function(a,
                                 call) {
  if (is.null(a$solver_extras)) {
    return(a)
  }
  rk_args <- .rk_step_controls()
  xtr_args <- .solver_extra_args()
  dots <- a$solver_extras
  a$solver_extras <- NULL
  unknown_args <- setdiff(names(dots), c(names(rk_args), names(xtr_args)))
  if (length(dots) &&
    (is.null(names(dots)) || !all(nzchar(names(dots))) || length(unknown_args))) {
    if (!length(unknown_args)) {
      unknown_args <- "<unnamed>"
    }
    .cli_action(solve_err$solver_dots,
      action = c("abort", "inform"),
      call = call
    )
  }
  for (nm in names(dots)) {
    if (nm %in% names(rk_args)) {
      rk_args[nm] <- dots[nm]
    } else {
      xtr_args[nm] <- dots[nm]
    }
  }
  a <- c(a, rk_args, xtr_args)
  return(a)
}
