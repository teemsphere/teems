#' @keywords internal
#' @noRd
.run_counter <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.run_id <- function() {
  n <- (.run_counter$n %|||% 0L) + 1L
  .run_counter$n <- n
  id <- paste0(format(x = Sys.time(), "%H%M%S"), "_", Sys.getpid(), "_", n)
  return(id)
}

#' @keywords internal
#' @noRd
.get_solver_paths <- function(cmf_path,
                              timeID,
                              call) {
  if (!file.exists(cmf_path)) {
    .cli_action(solve_err$no_cmf,
      action = "abort",
      call = call
    )
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
