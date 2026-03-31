#' @keywords internal
#' @noRd
.implement_solve <- function(args_list,
                             call) {
  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- format(x = Sys.time(), "%H%M")
  paths <- .get_solver_paths(
    cmf_path = args_list$cmf_path,
    timeID = timeID,
    call = call
  )

  v <- .validate_solver_args(
    a = args_list,
    paths = paths,
    call = call
  )

  cmds <- .construct_cmd(
    paths = paths,
    terminal_run = v$terminal_run,
    timeID = timeID,
    n_tasks = v$n_tasks,
    steps = v$steps,
    laA = v$laA,
    laDi = v$laDi,
    laD = v$laD,
    matsol = v$matsol,
    solmed = v$solmed,
    n_subintervals = v$n_subintervals,
    nesteddbbd = v$nesteddbbd,
    enable_time = v$enable_time
  )

  # need a process running in parallel, grepping output for error and then kill appropriate PID
  if (isFALSE(cmds)) {
    return(invisible(NULL))
  }
  if (.o_verbose()) {
    elapsed_time <- system.time(system(cmds$solve))
  } else {
    elapsed_time <- system.time(system(cmds$solve,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    ))
  }
  .check_solver_log(
    elapsed_time = elapsed_time,
    solve_cmd = cmds$solve,
    paths = paths,
    call = call
  )
  if (!v$suppress_outputs) {
    output <- ems_compose(
      cmf_path = v$cmf_path,
      type = "all"
    )
    return(output)
  }
  return(invisible(NULL))
}