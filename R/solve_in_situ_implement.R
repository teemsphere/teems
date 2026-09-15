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
                                     inmemory,
                                     resources,
                                     verbosity,
                                     suppress_outputs,
                                     terminal_run,
                                     assertions,
                                     range_test_initial,
                                     range_test_updated,
                                     postsim,
                                     complementarity,
                                     solver_args,
                                     call,
                                     explicit = character()) {
  cmf_path <- .in_situ_cmf(
    input_files = input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file,
    model_dir = model_dir,
    ignore_condense = ignore_condense,
    call = call
  )
  
  # splice the named solver extras into the ems_solve dots; the
  # resource arguments travel only when the caller wrote them, so
  # ems_solve() sees the same explicit set under resources = "auto"
  resource_args <- list(
    n_tasks = n_tasks,
    n_threads = n_threads,
    inmemory = inmemory
  )[explicit]
  return(do.call(ems_solve, c(
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
      precision = precision,
      resources = resources,
      verbosity = verbosity,
      suppress_outputs = suppress_outputs,
      terminal_run = terminal_run,
      assertions = assertions,
      range_test_initial = range_test_initial,
      range_test_updated = range_test_updated,
      postsim = postsim,
      complementarity = complementarity
    ),
    resource_args,
    solver_args
  )))
}