#' @keywords internal
#' @noRd
.implement_solve_in_situ <- function(args_list,
                                     call) {
  v <- .validate_in_situ_args(
    a = args_list,
    call = call
  )
  cmf_path <- .in_situ_cmf(
    input_files = v$input_files,
    model_file = v$model_file,
    closure_file = v$closure_file,
    shock_file = v$shock_file,
    model_dir = v$model_dir,
    ignore_condense = v$ignore_condense,
    call = call
  )
  solve_args <- v[setdiff(names(formals(ems_solve)), c("cmf_path", "..."))]
  sol <- do.call(ems_solve, c(
    list(cmf_path = cmf_path),
    solve_args,
    v$solver_args
  ))
  return(sol)
}
