#' @keywords internal
#' @noRd
.clear_solution_files <- function(run_dir) {
  sol_prefix <- file.path(run_dir, "out", "variables", "bin", "sol")
  stale <- paste0(sol_prefix, c(
    ".bin", ".est", ".acc", ".var", ".sel", ".set", ".mds",
    ".cof", ".cbin", ".stats.json"
  ))
  unlink(stale[file.exists(stale)])
  return(invisible(NULL))
}
