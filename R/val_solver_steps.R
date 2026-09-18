# the step counts: one per stage for the Runge-Kutta methods, the
# extrapolation triple otherwise (manual 25.2)
#' @keywords internal
#' @noRd
.validate_solver_steps <- function(a,
                                   call) {
  is_rk <- .solver_is_rk(a$solution_method)
  is_rk_embedded <- .solver_is_rk_embedded(a$solution_method)
  if (is_rk) {
    if (!all(
      is.numeric(a$steps), length(a$steps) == 1,
      rlang::is_integerish(a$steps), a$steps >= 1
    )) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_single_rk,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$adaptive %!=% "no" && !is_rk_embedded) {
      adaptive <- a$adaptive
      .cli_action(solve_err$adaptive_method,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$n_subintervals != 1) {
      solution_method <- a$solution_method
      .cli_action(solve_err$rk_subintervals,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (!all(is.numeric(a$eps_tolerance), length(a$eps_tolerance) == 1, a$eps_tolerance > 0)) {
      .cli_action(solve_err$epstol_range,
        action = "abort",
        call = call
      )
    }
  } else {
    if (!all(is.numeric(a$steps), length(a$steps) == 3)) {
      .cli_action(solve_err$step_length,
        action = "abort",
        call = call
      )
    }
    if (a$solution_method %=% "Gragg" && !all(a$steps %% 2 == 0)) {
      .cli_action(solve_err$step_parity,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$solution_method %in% c("Gragg", "Euler") && !all(diff(a$steps) > 0)) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_increasing,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(invisible(NULL))
}
