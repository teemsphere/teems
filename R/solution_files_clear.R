#' @keywords internal
#' @noRd
.solution_file_suffixes <- function() {
  suffixes <- c(
    ".bin", ".est", ".acc", ".var", ".sel", ".set", ".mds",
    ".cof", ".cbin", ".stats.json", ".cbin0", ".xac", ".cols",
    ".cols.json", ".jac", ".jac.json", ".outputs.json", ".outputs.json.tmp",
    ".probe.json", ".probe.pattern"
  )
  return(suffixes)
}

#' @keywords internal
#' @noRd
.clear_solution_files <- function(run_dir) {
  sol_prefix <- file.path(run_dir, "out", "variables", "bin", "sol")
  stale <- paste0(sol_prefix, .solution_file_suffixes())
  unlink(stale[file.exists(stale)])
  csv_out <- list.files(
    file.path(run_dir, "out", c("sets", "coefficients")),
    pattern = "\\.csv$",
    full.names = TRUE
  )
  unlink(csv_out)
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.clear_solution_files_cmd <- function(docker_run) {
  sol_files <- paste0(
    file.path(docker_run, "out", "variables", "bin", "sol"),
    "{", paste(.solution_file_suffixes(), collapse = ","), "}"
  )
  csv_out <- file.path(docker_run, "out", "{sets,coefficients}", "*.csv")
  rm_cmd <- paste("rm -f", sol_files, csv_out)
  return(rm_cmd)
}
