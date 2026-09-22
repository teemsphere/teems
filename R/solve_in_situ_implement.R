#' @keywords internal
#' @noRd
.implement_solve_in_situ <- function(model_dir,
                                     model_file,
                                     closure_file,
                                     input_files,
                                     shock_file,
                                     ignore_condense,
                                     solution_method,
                                     matrix_method,
                                     n_subintervals,
                                     steps,
                                     adaptive,
                                     eps_tolerance,
                                     max_retries,
                                     retry_adjust,
                                     n_tasks,
                                     n_threads,
                                     precision,
                                     verbosity,
                                     suppress_outputs,
                                     terminal_run,
                                     complementarity,
                                     solver_args,
                                     call) {
  cmf_path <- .in_situ_cmf(
    input_files = input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file,
    model_dir = model_dir,
    ignore_condense = ignore_condense,
    call = call
  )
  
  sol <- do.call(ems_solve, c(
    list(
      cmf_path = cmf_path,
      solution_method = solution_method,
      matrix_method = matrix_method,
      n_subintervals = n_subintervals,
      steps = steps,
      adaptive = adaptive,
      eps_tolerance = eps_tolerance,
      max_retries = max_retries,
      retry_adjust = retry_adjust,
      n_tasks = n_tasks,
      n_threads = n_threads,
      precision = precision,
      verbosity = verbosity,
      suppress_outputs = suppress_outputs,
      terminal_run = terminal_run,
      complementarity = complementarity
    ),
    solver_args
  ))
  if (is.null(sol)) {
    return(invisible(NULL))
  }
  return(sol)
}
