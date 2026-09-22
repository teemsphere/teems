#' @keywords internal
#' @noRd
.solve_lock_acquire <- function(run_dir,
                                timeID,
                                call) {
  lock_path <- file.path(run_dir, ".solve_lock")
  if (!dir.create(lock_path, showWarnings = FALSE)) {
    lock_owner <- tryCatch(
      suppressWarnings(readLines(file.path(lock_path, "owner"), warn = FALSE)),
      error = \(e) character()
    )
    lock_owner <- if (length(lock_owner) > 0L) {
      lock_owner[[1]]
    } else {
      solve_err$lock_earlier_run
    }
    .cli_action(
      msg = solve_err$lock_held,
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
