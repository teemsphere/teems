#' @keywords internal
#' @noRd
.collect_probe <- function(paths,
                           call) {
  sol_prefix <- file.path(dirname(paths$cmf), "out", "variables", "bin", "sol")
  diag_out <- normalizePath(paths$diag_out, "/", mustWork = FALSE)
  probe <- .probe_object(
    probe_path = paste0(sol_prefix, ".probe.json"),
    stats_path = paste0(sol_prefix, ".stats.json"),
    diag_out = diag_out,
    cmf_path = paths$cmf,
    call = call
  )
  return(probe)
}
