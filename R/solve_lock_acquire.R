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
    owner <- .lock_owner_parse(lock_owner)
    if (!is.na(owner$pid) && !.pid_alive(owner$pid)) {
      lock_owner <- lock_owner[[1]]
      .container_remove(owner$run)
      .cli_action(solve_info$lock_stale,
        action = "inform",
        call = call
      )
      unlink(lock_path, recursive = TRUE, force = TRUE)
      dir.create(lock_path, showWarnings = FALSE)
    } else {
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
.lock_owner_parse <- function(lock_owner) {
  owner <- list(run = NA_character_, pid = NA_integer_)
  if (length(lock_owner) > 0L) {
    hit <- regmatches(lock_owner[[1]], regexec("^run (\\S+) \\(pid ([0-9]+)\\)", lock_owner[[1]]))[[1]]
    if (length(hit) == 3L) {
      owner$run <- hit[[2]]
      owner$pid <- as.integer(hit[[3]])
    }
  }
  return(owner)
}
