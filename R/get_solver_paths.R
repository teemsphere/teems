#' Identifier for one solve, carried into the names of the files that
#' solve writes (`out/solver_out_<id>.txt` above all). Minute
#' resolution collided: two solves started in the same minute named the
#' same log, `tee` truncated it, and the surviving file could belong to
#' the other run while the abort message pointed at it. Seconds plus the
#' process id are unique across concurrent processes, which is the case
#' that actually collided.
#'
#' @keywords internal
#' @noRd
.run_id <- function() {
  paste0(format(x = Sys.time(), "%H%M%S"), "_", Sys.getpid())
}

#' @keywords internal
#' @noRd
.get_solver_paths <- function(cmf_path,
                              timeID,
                              call) {
  if (!file.exists(cmf_path)) {
    .cli_action(action = "abort",
                msg = "The {.arg cmf_path} provided {.path {cmf_path}} does not
                exist.",
                call = call)
  }
  cmf_path <- normalizePath(cmf_path, "/")
  run_dir <- dirname(cmf_path)
  diagnostic_out <- file.path(run_dir,
                              "out",
                              paste0("solver_out", "_", timeID, ".txt"))
  docker_run_dir <- "/opt/teems"
  docker_cmf_path <- sub(pattern = run_dir,
                         replacement = docker_run_dir,
                         x = cmf_path,
                         fixed = TRUE)
  
  paths <- list(cmf = unname(obj = cmf_path),
                run = run_dir,
                diag_out = diagnostic_out,
                docker_run = docker_run_dir,
                docker_cmf = unname(obj = docker_cmf_path))
  return(paths)
}