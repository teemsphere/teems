#' @keywords internal
#' @noRd
.implement_solve <- function(args_list,
                             call,
                             solmed = NULL,
                             extra_flags = NULL) {

  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- .run_id()
  paths <- .get_solver_paths(
    cmf_path = args_list$cmf_path,
    timeID = timeID,
    call = call
  )

  v <- .validate_solver_args(
    a = args_list,
    paths = paths,
    call = call,
    timeID = timeID
  )
  if (!is.null(solmed)) {
    v$solmed <- solmed
    v$n_subintervals <- 1
  }

  if (isTRUE(v$terminal_run) && .under_tempdir(paths$run)) {
    run_dir <- paths$run
    .cli_action(solve_wrn$terminal_tempdir,
      action = c("warn", "inform"),
      call = call
    )
  }

  lock_path <- .solve_lock_acquire(
    run_dir = dirname(paths$cmf),
    timeID = timeID,
    call = call
  )
  if (!isTRUE(v$terminal_run)) {
    on.exit(.solve_lock_release(lock_path), add = TRUE)
  }

  cmds <- .construct_cmd(
    paths = paths,
    terminal_run = v$terminal_run,
    timeID = timeID,
    n_tasks = v$n_tasks,
    steps = v$steps,
    matsol = v$matsol,
    solmed = v$solmed,
    adaptive = v$adaptive,
    eps_tolerance = v$eps_tolerance,
    max_retries = v$max_retries,
    retry_adjust = v$retry_adjust,
    n_threads = v$n_threads,
    precision = v$precision,
    n_subintervals = v$n_subintervals,
    verbosity = v$verbosity,
    assertions = .o_assertions(),
    range_test_initial = .o_range_test_initial(),
    range_test_updated = .o_range_test_updated(),
    complementarity = v$complementarity,
    laA = v$laA,
    laD = v$laD,
    laDi = v$laDi,
    extra_flags = paste(c(.extra_cli_flags(v), extra_flags), collapse = " ")
  )
  if (isFALSE(cmds)) {
    .solve_lock_release(lock_path)
    return(invisible(NULL))
  }

  completed <- FALSE
  on.exit(if (!completed) .container_remove(timeID), add = TRUE, after = FALSE)

  status <- 0L
  elapsed_time <- system.time(
    status <- .relay_solver_cmd(cmds$solve,
      verbosity = as.integer(v$verbosity),
      timeID = timeID
    )
  )
  completed <- TRUE

  .check_solver_log(
    elapsed_time = elapsed_time,
    solve_cmd = cmds$solve,
    paths = paths,
    call = call,
    status = status,
    resources_record = v$resources_record
  )
  if (!v$suppress_outputs) {
    output <- ems_compose(cmf_path = v$cmf_path)
    return(output)
  }
  cmf_path <- v$cmf_path
  return(invisible(cmf_path))
}

#' @keywords internal
#' @noRd
.under_tempdir <- function(path) {
  root <- normalizePath(tempdir(), "/", mustWork = FALSE)
  path <- normalizePath(path, "/", mustWork = FALSE)
  under <- identical(path, root) || startsWith(path, paste0(root, "/"))
  return(under)
}
