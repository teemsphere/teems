#' @keywords internal
#' @noRd
.implement_solve_in_situ <- function(model_file,
                                     model_dir,
                                     closure_file,
                                     input_files,
                                     shock_file,
                                     n_tasks,
                                     n_subintervals,
                                     matrix_method,
                                     solution_method,
                                     steps,
                                     laA,
                                     laD,
                                     laDi,
                                     suppress_outputs,
                                     terminal_run,
                                     append_args,
                                     call) {
  cmf_path <- .in_situ_cmf(
    input_files = input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file,
    model_dir = model_dir,
    call = call
  )
  
  return(ems_solve(
    cmf_path = cmf_path,
    n_tasks = n_tasks,
    n_subintervals = n_subintervals,
    matrix_method = matrix_method,
    solution_method = solution_method,
    steps = steps,
    laA = laA,
    laD = laD,
    laDi = laDi,
    suppress_outputs = suppress_outputs,
    terminal_run = terminal_run,
    append_args = append_args
  ))
}