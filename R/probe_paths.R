#' @keywords internal
#' @noRd
.probe_paths <- function(paths) {
  probe_dir <- file.path(paths$run, "out", "probe")
  unlink(probe_dir, recursive = TRUE)
  dir.create(probe_dir, recursive = TRUE)
  docker_probe_dir <- file.path(paths$docker_run, "out", "probe")
  soldata <- sprintf('soldata "SolFiles" "%s";', file.path(docker_probe_dir, "sol"))
  cmf <- readLines(paths$cmf)
  is_sol <- grepl("^\\s*soldata\\s+\"solfiles\"", cmf, ignore.case = TRUE)
  cmf <- c(cmf[!is_sol], soldata)
  probe_cmf <- file.path(probe_dir, basename(paths$cmf))
  writeLines(cmf, probe_cmf)
  paths$docker_cmf <- file.path(docker_probe_dir, basename(paths$cmf))
  paths$sol_prefix <- file.path(probe_dir, "sol")
  paths$diag_out <- file.path(probe_dir, "solver_out_probe.txt")
  paths$docker_diag_out <- file.path(docker_probe_dir, "solver_out_probe.txt")
  return(paths)
}
