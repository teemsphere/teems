#' @keywords internal
#' @noRd
.implement_RK <- function(args_list,
                          call) {
  a <- .validate_RK_args(
    a = args_list,
    call = call
  )
  solve_args <- c(
    list(
      cmf_path = a$cmf_path,
      solution_method = a$solution_method,
      steps = a$steps,
      adaptive = a$adaptive,
      eps_tolerance = a$eps_tolerance,
      max_retries = a$max_retries,
      retry_adjust = a$retry_adjust,
      rk_chart = a$chart,
      rk_norm = a$error_norm,
      rk_controller = a$controller,
      rk_scope = a$scope,
      rk_h0 = a$h_init,
      rk_guard = a$guard,
      matrix_method = a$matrix_method,
      n_subintervals = a$n_subintervals,
      n_tasks = a$n_tasks,
      n_threads = a$n_threads,
      precision = a$precision,
      verbosity = a$verbosity,
      suppress_outputs = a$suppress_outputs,
      terminal_run = a$terminal_run,
      complementarity = a$complementarity,
      accuracy_vars = a$accuracy_vars
    ),
    a$solve_args
  )
  sol <- do.call(ems_solve, solve_args)
  return(sol)
}
