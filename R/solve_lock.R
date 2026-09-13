#' Exclusive claim on a deploy directory for the duration of a solve
#'
#' Concurrent runs are supported a directory at a time: every run gets
#' its own deploy directory (`ems_option_set(tempdir = ...)` before
#' [ems_deploy()]), and runs so separated solve simultaneously without
#' touching each other. Two solves against ONE deploy directory cannot,
#' and did not fail cleanly: the solver names its scratch tab file from
#' the MPI rank alone, so both runs write `_temp_tab_file0000.tab` there
#' and corrupt each other mid-parse, and the solution binaries and the
#' set and coefficient CSVs the CMF names are shared besides. Both runs
#' then abort with a garbled-statement error that points at neither
#' cause.
#'
#' The claim is a directory, whose creation is atomic on every platform:
#' the process that creates it owns the deploy directory until it exits,
#' and a second one is refused by name with the remedy. It is released
#' on exit, including on error.
#'
#' @keywords internal
#' @noRd
.solve_lock_acquire <- function(run_dir,
                                timeID,
                                call) {
  lock_path <- file.path(run_dir, ".solve_lock")
  if (!dir.create(lock_path, showWarnings = FALSE)) {
    lock_owner <- tryCatch(
      readLines(file.path(lock_path, "owner"), warn = FALSE),
      error = function(e) character()
    )
    lock_owner <- if (length(lock_owner) > 0L) lock_owner[[1]] else "an earlier run"
    .cli_action(
      msg = c(
        "A solve is already running in this deploy directory: {lock_owner}.",
        "Concurrent runs need a deploy directory each: call
         {.code ems_option_set(tempdir = ...)} before {.fn ems_deploy} for
         every run. Two solves in one directory share the solver's scratch
         files and the outputs the CMF names, and would corrupt each other.
         If a previous run was interrupted, remove {.path {lock_path}}."
      ),
      action = c("abort", "inform"),
      call = call
    )
  }
  writeLines(
    sprintf(
      "run %s (pid %d) started %s",
      timeID, Sys.getpid(), format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ),
    file.path(lock_path, "owner")
  )
  return(lock_path)
}

#' @keywords internal
#' @noRd
.solve_lock_release <- function(lock_path) {
  if (!is.null(lock_path)) {
    unlink(lock_path, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}
