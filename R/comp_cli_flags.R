#' Render a teems_complementarity spec into the solver's -comp_*
#' command-line flags; NULL fields are omitted so the solver defaults
#' apply (the effective values land in sol.stats.json either way)
#'
#' @keywords internal
#' @noRd
.comp_cli_flags <- function(spec) {
  if (is.null(spec)) {
    return(NULL)
  }
  flags <- character(0)
  if (!is.null(spec$steps_approx_run)) {
    flags <- c(flags, paste("-comp_steps", spec$steps_approx_run))
  }
  if (!is.null(spec$redo_steps)) {
    flags <- c(flags, paste("-comp_redo", as.integer(spec$redo_steps)))
  }
  if (!is.null(spec$redo_step_min_fraction)) {
    flags <- c(flags, paste("-comp_redo_min_frac", spec$redo_step_min_fraction))
  }
  if (!is.null(spec$do_approx_run)) {
    flags <- c(flags, paste("-comp_do_approx", as.integer(spec$do_approx_run)))
  }
  if (!is.null(spec$do_acc_run)) {
    flags <- c(flags, paste("-comp_do_acc", as.integer(spec$do_acc_run)))
  }
  if (!is.null(spec$state_bound_error)) {
    flags <- c(flags, paste("-comp_sberr_warn", as.integer(spec$state_bound_error == "warn")))
  }
  flags <- paste(flags, collapse = " ")
  return(flags)
}
