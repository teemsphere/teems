# Condensation record for the deployed model. The measured cost of
# backsolving scales with the share of the uncondensed system that was
# substituted out (teems-solver ROADMAP 6.2), so the solve-time and
# probe-time advisories need the share, not just the nomination counts.
#' @keywords internal
#' @noRd
.compute_cndns_metadata <- function(model,
                                    sets,
                                    system_size) {
  vars <- model[model$type == "Variable" & !is.na(model$condense), ]
  backsolved <- vars[vars$condense %in% "backsolve", ]

  n_backsolve_ele <- .count_var_elements(
    var_extract = backsolved,
    sets = sets
  )
  uncondensed <- system_size + n_backsolve_ele

  metadata <- list(
    n_backsolve = nrow(backsolved),
    n_backsolve_ele = n_backsolve_ele,
    elimination_share = if (uncondensed > 0) {
      n_backsolve_ele / uncondensed
    } else {
      0
    }
  )
  return(metadata)
}
