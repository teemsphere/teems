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
  run_dir <- normalizePath(dirname(path = cmf_path), winslash = "/")
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