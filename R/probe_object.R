#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
.probe_object <- function(probe_path,
                          stats_path = NULL,
                          diag_out = NULL,
                          cmf_path = NULL,
                          call = NULL) {
  if (!file.exists(probe_path)) {
    probe_path <- normalizePath(probe_path, "/", mustWork = FALSE)
    .cli_action(probe_err$no_report,
      action = "abort",
      call = call
    )
  }
  probe <- jsonlite::fromJSON(probe_path, simplifyMatrix = FALSE)

  stats <- NULL
  if (!is.null(stats_path) && file.exists(stats_path)) {
    stats <- jsonlite::fromJSON(stats_path)
  }

  statements <- .probe_statements(probe$statements)
  incidence <- .probe_incidence(probe$statements)

  out <- structure(
    list(
      valid = !isTRUE(probe$defective),
      version = probe$version,
      vecsize = probe$vecsize,
      structural = .probe_pattern(probe$structural, probe$vecsize),
      realized = .probe_pattern(probe$realized, probe$vecsize),
      defects = .probe_defects(probe),
      statements = statements,
      incidence = incidence,
      cores = .probe_cores(probe$fine),
      structure = .probe_stats(stats),
      condense = .probe_cndns(
        stats = .probe_stats(stats),
        cmf_path = cmf_path
      ),
      paths = list(
        report = normalizePath(probe_path, "/", mustWork = FALSE),
        stats = if (is.null(stats_path)) {
          NULL
        } else {
          normalizePath(stats_path, "/", mustWork = FALSE)
        },
        log = diag_out,
        cmf = cmf_path
      )
    ),
    class = "teems_probe"
  )
  return(out)
}
