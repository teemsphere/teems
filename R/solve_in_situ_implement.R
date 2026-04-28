#' @keywords internal
#' @noRd
.implement_solve_in_situ <- function(model_file,
                                     model_dir,
                                     closure_file,
                                     input_files,
                                     shock_file,
                                     writeout,
                                     n_tasks,
                                     n_subintervals,
                                     matrix_method,
                                     solution_method,
                                     steps,
                                     laA,
                                     laD,
                                     laDi,
                                     terminal_run,
                                     suppress_outputs,
                                     call) {
  cmf_path <- .in_situ_cmf(
    input_files = input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file,
    writeout = writeout,
    model_dir = model_dir,
    call = call
  )
  
  ems_solve(
    cmf_path = cmf_path,
    n_tasks = n_tasks,
    n_subintervals = n_subintervals,
    matrix_method = matrix_method,
    solution_method = solution_method,
    steps = steps,
    laA = laA,
    laD = laD,
    laDi = laDi,
    terminal_run = terminal_run,
    suppress_outputs = TRUE
  )
  
  if (!suppress_outputs) {
    if (!writeout) {
      output <- ems_compose(
        cmf_path = cmf_path,
        minimal = TRUE
      )
    } else {
      output <- ems_compose(cmf_path = cmf_path)
    }
  } else {
    output <- cmf_path
  }
}