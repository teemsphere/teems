# Condensation advice (teems-solver ROADMAP 6.2, benchmark round
# 2026-07-17). Condensation is an LU-era lever: it pays on static models
# solved by plain LU (-41% wall at 1.35M equations) and is
# counterproductive under the bordered methods at every elimination share
# measured, because substitution destroys the intra-block sparsity
# SBBD/DBBD/NDBBD exploit while the eliminated rows are re-evaluated per
# step by backsolve recovery anyway. Omission does not densify anything,
# so only backsolves trigger the advice.

# Probe-informed condensation advice (ROADMAP 6.2 follow-up, via the
# 6.10 probe pathway). The solve-time advisory above knows only whether a
# deployment is condensed; the probe additionally measures the block
# structure a bordered method would exploit, which is what actually
# decides the question. A partition of more than one diagonal block means
# a bordered method applies and substitution works against it; no usable
# partition means the run is LU-bound, where condensation is the measured
# lever -- and worth suggesting once the system is large enough for the
# gain to show (-41% wall at 1.35M equations; nothing at 202k).
.cndns_lu_size <- 1e6

#' @keywords internal
#' @noRd
.advise_cndns <- function(metadata,
                          matrix_method,
                          enable_time,
                          call) {
  condense <- metadata$condense
  if (is.null(condense) || (condense$n_backsolve %|||% 0L) < 1L) {
    return(invisible(NULL))
  }
  n_backsolve <- condense$n_backsolve
  share <- .cndns_share(condense)

  if (enable_time) {
    .cli_action(solve_info$condense_intertemporal,
      action = rep("inform", 3),
      call = call
    )
  } else if (matrix_method %in% c("SBBD", "DBBD", "NDBBD")) {
    .cli_action(solve_info$condense_bordered,
      action = rep("inform", 3),
      call = call
    )
  }
  return(invisible(NULL))
}
